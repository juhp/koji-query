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

main :: IO ()
main =
  simpleCmdArgs' Nothing "koji-query" "Helper client for koji queries" $
  program
  <$> optional (strOptionWith 'u' "user" "USER" "Koji user")
  <*> optionalWith auto 'l' "limit" "NUMTASKS" "Maximum number of tasks to show [default: 20]" 20
  <*> many (strOptionWith 'a' "arch" "ARCH" "Task arch")
  <*> optional (strArg "DAY")

program :: Maybe String -> Int -> [String] -> Maybe String -> IO ()
program muser limit archs mdate = do
  date <- cmd "date" ["+%F", "--date=" ++ fromMaybe "yesterday" mdate]
  user <- case muser of
            Just user -> return user
            Nothing -> do
              mfasid <- (dropSuffix "@FEDORAPROJECT.ORG" <$>) . find ("@FEDORAPROJECT.ORG" `isSuffixOf`) . words <$> cmd "klist" ["-l"]
              case mfasid of
                Just fas -> return fas
                Nothing -> error' "Could not determine FAS id from klist"
  mowner <- kojiGetUserID fedoraKojiHub user
  tz <- getCurrentTimeZone
  case mowner of
    Nothing -> error "No owner found"
    Just owner -> do
      listTasks fedoraKojiHub
        ([--("method", ValueString "build"),
         ("owner", ValueInt (getID owner)),
         ("startedAfter", ValueString date),
         ("decode", ValueBool True)]
        ++ [("arch", ValueArray (map ValueString archs)) | notNull archs])
        [("limit",ValueInt limit), ("order", ValueString "id")]
        >>= mapM_ (printTask tz)
  where
    printTask :: TimeZone -> Struct -> IO ()
    printTask tz task = do
      putStrLn ""
      whenJust (taskLines task) $ mapM_ putStrLn . formatTaskResult tz

    formatTaskResult :: TimeZone -> TaskResult -> [String]
    formatTaskResult tz (TaskResult title url start mcompletion) =
      [ title
      , url
      , formatTime defaultTimeLocale "%c" (utcToLocalTime tz start)
      ]
      ++
      case mcompletion of
        Nothing -> []
        Just c_t ->
          [formatTime defaultTimeLocale "%c" (utcToLocalTime tz c_t)]
#if MIN_VERSION_time(1,9,1)
          ++
          let dur = diffUTCTime c_t start
          in ["duration: " ++ formatTime defaultTimeLocale "%Hh %Mm %Ss" dur]
#endif

    readTime' :: String -> UTCTime
    readTime' = read . replace "+00:00" "Z"

    taskLines :: Struct -> Maybe TaskResult
    taskLines st = do
      arch <- lookupStruct "arch" st
      start_time <- readTime' <$> lookupStruct "start_time" st
      let mcompletion_time = readTime' <$> lookupStruct "completion_time" st
      taskid <- lookupStruct "id" st
      method <- lookupStruct "method" st
      state <- getTaskState st
      request <- head <$> lookupStruct "request" st >>= getString
      let package =
            if method == "buildArch"
            then removeSuffix ".src.rpm" (takeFileName request) <.> arch
            else takeFileName request
          parent = lookupStruct "parent" st :: Maybe Int
      return $
        TaskResult
          (package +-+ method +-+ show state +-+ maybe "" show parent)
          ("https://koji.fedoraproject.org/koji/taskinfo?taskID=" ++ show (taskid :: Int))
          start_time
          mcompletion_time

data TaskResult = TaskResult String String UTCTime (Maybe UTCTime)
