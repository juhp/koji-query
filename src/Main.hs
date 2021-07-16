{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra

import Data.List.Extra
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.RPM.NVR
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
import Text.Pretty.Simple

import Paths_koji_query (version)

data TaskIdReq = Task Int | Parent Int | TaskQuery

data TaskFilter = TaskPackage String | TaskNVR String

main :: IO ()
main =
  simpleCmdArgs' (Just version) "koji-query"
  "Helper client for koji queries: https://github.com/juhp/koji-query" $
  program
  <$> strOptionalWith 'S' "server" "URL" "Koji Hub [default: Fedora]" fedoraKojiHub
  <*> optional (strOptionWith 'u' "user" "USER" "Koji user")
  <*> optionalWith auto 'l' "limit" "INT" "Maximum number of tasks to show [default: 20]" 20
  <*> (Task <$> optionWith auto 't' "task" "TASKID" "Show task"
       <|> Parent <$> optionWith auto 'P' "parent" "TASKID" "List children tasks"
       <|> pure TaskQuery)
  <*> many (parseTaskState <$> strOptionWith 's' "state" "STATE" "Filter tasks by state")
  <*> many (strOptionWith 'a' "arch" "ARCH" "Task arch")
  <*> optional (strOptionWith 'd' "date" "DAY" "Tasks started after date [default: yesterday]")
  <*> optional (strOptionWith 'm' "method" "METHOD" "Select tasks by method: [build,buildarch,etc]")
  <*> switchWith 'D' "debug" "Pretty-pretty raw XML result"
  <*> optional (TaskPackage <$> strOptionWith 'p' "package" "PKG" "Filter results to specified package"
               <|> TaskNVR <$> strOptionWith 'n' "nvr" "PREFIX" "Filter results by NVR prefix")
program :: String -> Maybe String -> Int -> TaskIdReq -> [TaskState]
        -> [String] -> Maybe String -> Maybe String -> Bool
        -> Maybe TaskFilter
        -> IO ()
program server muser limit taskreq states archs mdate mmethod debug mfilter' = do
  tz <- getCurrentTimeZone
  mgr <- httpManager
  case taskreq of
    Task taskid -> do
      when (isJust muser || isJust mdate || isJust mfilter') $
        error' "cannot use --task together with --user, --date, or filter"
      mtask <- kojiGetTaskInfo server (TaskId taskid)
      whenJust mtask$ \task -> do
        when debug $ pPrintCompact task
        whenJust (maybeTaskResult task) $ printTask mgr tz
    _ -> do
      query <- setupQuery
      results <- listTasks server query
                 [("limit",ValueInt limit), ("order", ValueString "id")]
      when debug $ mapM_ pPrintCompact results
      (mapM_ (printTask mgr tz) . filterResults . mapMaybe maybeTaskResult) results
  where
    setupQuery = do
      case taskreq of
        Task _ -> error' "reachable task request"
        Parent parent -> do
          when (isJust muser || isJust mdate || isJust mfilter') $
            error' "cannot use --parent together with --user, --date, or filter"
          return $
            ("parent", ValueInt parent) : commonParams
        TaskQuery -> do
          date <- cmd "date" ["+%F", "--date=" ++ dateString mdate]
          putStrLn $ "since " ++ date
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
            Just owner ->
              return $
                [("owner", ValueInt (getID owner)),
                 ("startedAfter", ValueString date)]
                ++ commonParams
        where
          commonParams =
            [("decode", ValueBool True)]
            ++ [("state", ValueArray (map taskStateToValue states)) | notNull states]
            ++ [("arch", ValueArray (map ValueString archs)) | notNull archs]
            ++ [("method",  ValueString method) | Just method <- [mmethod]]

    dateString :: Maybe String -> String
    dateString Nothing = "yesterday"
    dateString (Just s) =
      if s `elem` ["hour", "day", "week", "month", "year"]
      then "last " ++ s
      else s

    maybeTaskResult :: Struct -> Maybe TaskResult
    maybeTaskResult st = do
      arch <- lookupStruct "arch" st
      start_time <- readTime' <$> lookupStruct "start_time" st
      let mend_time = readTime' <$> lookupStruct "completion_time" st
      taskid <- lookupStruct "id" st
      method <- lookupStruct "method" st
      state <- getTaskState st
      request <- lookupStruct "request" st >>= getString . head
      let nvr =
            if method == "buildArch"
            then removeSuffix ".src.rpm" (takeFileName request)
            else takeFileName request
          mparent' = lookupStruct "parent" st :: Maybe Int
      return $
        TaskResult (readNVR nvr) arch method state mparent' taskid start_time mend_time
      where
        readTime' :: String -> UTCTime
        readTime' = read . replace "+00:00" "Z"

    filterResults :: [TaskResult] -> [TaskResult]
    filterResults ts =
      case mfilter' of
        Nothing -> ts
        Just (TaskPackage pkg) ->
          filter (isPackage pkg . taskNVR) ts
        Just (TaskNVR nvr) ->
          filter ((nvr `isPrefixOf`) . showNVR . taskNVR) ts
      where
        isPackage pkg (NVR n _) = n == pkg

    printTask :: Manager -> TimeZone -> TaskResult -> IO ()
    printTask mgr tz task = do
      putStrLn ""
      let mendtime = mtaskEndTime task
      time <- maybe getCurrentTime return mendtime
      (mapM_ putStrLn . formatTaskResult (isJust mendtime) tz) (task {mtaskEndTime = Just time})
      buildlogSize mgr (taskId task)

    pPrintCompact =
#if MIN_VERSION_pretty_simple(4,0,0)
      pPrintOpt CheckColorTty
      (defaultOutputOptionsDarkBg {outputOptionsCompact = True})
#else
      pPrint
#endif

formatTaskResult :: Bool -> TimeZone -> TaskResult -> [String]
formatTaskResult ended tz (TaskResult nvr arch method state mparent taskid start mendtime) =
  [ showNVR nvr +-+ arch +-+ method +-+ show state +-+ maybe "" (\p -> "(parent: " ++ show p ++ ")") mparent
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


data TaskResult =
  TaskResult {taskNVR :: NVR,
              _taskArch :: String,
              _taskMethod :: String,
              _taskState :: TaskState,
              _mtaskParent :: Maybe Int,
              taskId :: Int,
              _taskStartTime :: UTCTime,
              mtaskEndTime :: Maybe UTCTime
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
      -- FIXME if too small show root.log url instead
      putStrLn logtail
  where
    tid = show taskid

    buildlog = "https://kojipkgs.fedoraproject.org/work/tasks" </> lastFew </> tid </> "build.log"

    lastFew =
      let few = dropWhile (== '0') $ drop 4 tid in
        if null few then "0" else few

    kiloBytes s = prettyI (Just ',') (fromInteger s `div` 1000) <> T.pack "kB"

    logtail = "https://koji.fedoraproject.org/koji/getfile?taskID=" ++ tid ++ "&name=build.log&offset=-4000"
