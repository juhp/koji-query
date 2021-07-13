{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra

import Data.List.Extra
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Distribution.Koji
import Distribution.Koji.API
import SimpleCmd
import SimpleCmdArgs
import System.FilePath

import Paths_koji_query (version)

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
        >>= mapM_ (printTask tz)
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
            >>= mapM_ (printTask tz)
  where
    dateString :: Maybe String -> String
    dateString Nothing = "yesterday"
    dateString (Just s) =
      if s `elem` ["hour", "day", "week", "month", "year"]
      then "last " ++ s
      else s

    printTask :: TimeZone -> Struct -> IO ()
    printTask tz task = do
      putStrLn ""
      whenJust (taskResult task) $ \ result -> do
        let mendtime = mtaskEndTime result
        time <- case mendtime of
          Just end -> return end
          Nothing -> getCurrentTime
        (mapM_ putStrLn . formatTaskResult (isJust mendtime) tz) (result {mtaskEndTime = Just time})

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
              _taskId :: Int,
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
    "closed" -> TaskClosed
    "canceled" -> TaskCanceled
    "assigned" -> TaskAssigned
    "failed" -> TaskFailed
    _ -> error $! "unknown task state: " ++ s
#endif
