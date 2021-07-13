{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra

import Data.List.Extra
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Format.Numbers
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Distribution.Koji
import Distribution.Koji.API
import Network.HTTP.Directory
import SimpleCmd
import SimpleCmdArgs
import System.FilePath

import Paths_koji_query (version)

-- FIXME query a taskid!
main :: IO ()
main =
  simpleCmdArgs' (Just version) "koji-query" "Helper client for koji queries" $
  program
  <$> optional (strOptionWith 'u' "user" "USER" "Koji user")
  <*> optionalWith auto 'l' "limit" "NUMTASKS" "Maximum number of tasks to show [default: 20]" 20
  <*> optional (optionWith auto 'p' "parent" "TASKID" "List children tasks")
  <*> many (parseTaskState <$> strOptionWith 's' "state" "STATE" "Filter tasks by state")
  <*> many (strOptionWith 'a' "arch" "ARCH" "Task arch")
  <*> optional (strArg "DAY")

program :: Maybe String -> Int -> Maybe Int -> [TaskState] -> [String]
        -> Maybe String
        -> IO ()
program muser limit mparent states archs mdate = do
  tz <- getCurrentTimeZone
  mgr <- httpManager
  case mparent of
    Just parent -> do
      when (isJust muser || isJust mdate) $
        error' $ "cannot use --parent together with --user or date"
      listTasks fedoraKojiHub
        ([("parent", ValueInt parent),
          ("decode", ValueBool True)]
         ++ [("state", ValueArray (map taskStateToValue states)) | notNull states]
         ++ [("arch", ValueArray (map ValueString archs)) | notNull archs])
        [("order", ValueString "id")]
        >>= mapM_ (printTask mgr tz)
    Nothing -> do
      date <- cmd "date" ["+%F", "--date=" ++ dateString mdate]
      user <- case muser of
                Just user -> return user
                Nothing -> do
                  mfasid <- (dropSuffix "@FEDORAPROJECT.ORG" <$>) . find ("@FEDORAPROJECT.ORG" `isSuffixOf`) . words <$> cmd "klist" ["-l"]
                  case mfasid of
                    Just fas -> return fas
                    Nothing -> error' "Could not determine FAS id from klist"
      mowner <- kojiGetUserID fedoraKojiHub user
      case mowner of
        Nothing -> error "No owner found"
        Just owner -> do
          listTasks fedoraKojiHub
            ([("owner", ValueInt (getID owner)),
              ("startedAfter", ValueString date),
              ("decode", ValueBool True)]
             ++ [("state", ValueArray (map taskStateToValue states)) | notNull states]
             ++ [("arch", ValueArray (map ValueString archs)) | notNull archs])
            [("limit",ValueInt limit), ("order", ValueString "id")]
            >>= mapM_ (printTask mgr tz)
  where
    dateString :: Maybe String -> String
    dateString Nothing = "yesterday"
    dateString (Just s) =
      if s `elem` ["hour", "day", "week", "month", "year"]
      then "last " ++ s
      else s

    printTask :: Manager -> TimeZone -> Struct -> IO ()
    printTask mgr tz task = do
      putStrLn ""
      whenJust (taskResult task) $ \ result -> do
        let mendtime = mtaskEndTime result
        time <- case mendtime of
          Just end -> return end
          Nothing -> getCurrentTime
        (mapM_ putStrLn . formatTaskResult (isJust mendtime) tz) (result {mtaskEndTime = Just time})
        buildlogSize mgr (taskId result)

    formatTaskResult :: Bool -> TimeZone -> TaskResult -> [String]
    formatTaskResult ended tz (TaskResult package method state mparent' taskid start mendtime) =
      [ package +-+ method +-+ show state +-+ maybe "" (\p -> "(parent: " ++ show p ++ ")") mparent'
      , "https://koji.fedoraproject.org/koji/taskinfo?taskID=" ++ show taskid
      , formatTime defaultTimeLocale "%c (start)" (utcToLocalTime tz start)
      ]
      ++
      case mendtime of
        Nothing -> []
        Just end ->
          [formatTime defaultTimeLocale "%c (end)" (utcToLocalTime tz end) | ended]
#if MIN_VERSION_time(1,9,1)
          ++
          let dur = diffUTCTime end start
          in [(if not ended then "current " else "") ++ "duration: " ++ formatTime defaultTimeLocale "%Hh %Mm %Ss" dur]
#endif

    readTime' :: String -> UTCTime
    readTime' = read . replace "+00:00" "Z"

    taskResult :: Struct -> Maybe TaskResult
    taskResult st = do
      arch <- lookupStruct "arch" st
      start_time <- readTime' <$> lookupStruct "start_time" st
      let mend_time = readTime' <$> lookupStruct "completion_time" st
      taskid <- lookupStruct "id" st
      method <- lookupStruct "method" st
      state <- getTaskState st
      request <- head <$> lookupStruct "request" st >>= getString
      let package =
            if method == "buildArch"
            then removeSuffix ".src.rpm" (takeFileName request) <.> arch
            else takeFileName request
          mparent' = lookupStruct "parent" st :: Maybe Int
      return $
        TaskResult package method state mparent' taskid start_time mend_time

data TaskResult =
  TaskResult {_taskPackage :: String,
              _taskMethod :: String,
              _taskState :: TaskState,
              _mtaskParent :: Maybe Int,
              taskId :: Int,
              _taskStartTime :: UTCTime,
              mtaskEndTime :: (Maybe UTCTime)
             }

----

#if !MIN_VERSION_koji(0,0,3)
taskStateToValue :: TaskState -> Value
taskStateToValue = ValueInt . fromEnum

parseTaskState :: String -> TaskState
parseTaskState s =
  case lower s of
    "free" -> TaskFree
    "open" -> TaskOpen
    "close" -> TaskClosed
    "closed" -> TaskClosed
    "cancel" -> TaskCanceled
    "canceled" -> TaskCanceled
    "assigned" -> TaskAssigned
    "fail" -> TaskFailed
    "failed" -> TaskFailed
    _ -> error' $! "unknown task state: " ++ s
#endif

buildlogSize :: Manager -> Int -> IO ()
buildlogSize mgr taskid = do
  exists <- httpExists mgr buildlog
  when exists $ do
    putStr $ buildlog ++ " "
    msize <- httpFileSize mgr buildlog
    whenJust msize $ \size -> do
      putStr "("
      (T.putStr . kiloBytes) size
      putStrLn ")"
      -- FIXME if too small show root.log instead
      putStrLn logtail
  where
    tid = show taskid

    buildlog = "https://kojipkgs.fedoraproject.org/work/tasks" </> lastFew </> tid </> "build.log"

    lastFew =
      let few = dropWhile (== '0') $ drop 4 tid in
        if null few then "0" else few

    kiloBytes s = prettyI (Just ',') (fromInteger s `div` 1000) <> T.pack "kB"

    logtail = "https://koji.fedoraproject.org/koji/getfile?taskID=" ++ tid ++ "&name=build.log&offset=-4000"
