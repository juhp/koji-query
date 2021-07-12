-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-License-Identifier: BSD-3-Clause

-- #if !MIN_VERSION_base(4,8,0)
-- import Control.Applicative ((<$>), (<*>))
-- #endif
import Control.Monad.Extra

-- import Data.List (sortOn)
import Data.List.Extra
import Data.Maybe
-- #if !MIN_VERSION_base(4,11,0)
-- import Data.Monoid ((<>))
-- #endif
-- import qualified Data.Text as T
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
        [("limit",ValueInt limit)]
        >>= mapM_ (printTask tz)
  where
    printTask :: TimeZone -> Struct -> IO ()
    printTask tz task = do
      putStrLn ""
      whenJust (taskLines tz task) (mapM_ putStrLn)

    readTime' :: String -> UTCTime
    readTime' = read . replace "+00:00" "Z"

    taskLines :: TimeZone -> Struct -> Maybe [String]
    taskLines tz st = do
      arch <- lookupStruct "arch" st
      completion_time <- readTime' <$> lookupStruct "completion_time" st
      start_time <- readTime' <$> lookupStruct "start_time" st
      taskid <- lookupStruct "id" st
      method <- lookupStruct "method" st
      state <- getTaskState st
      let duration = diffUTCTime completion_time start_time
      request <- head <$> lookupStruct "request" st >>= getString
      let package =
            if method == "buildArch"
            then removeSuffix ".src.rpm" (takeFileName request) <.> arch
            else takeFileName request
          parent = lookupStruct "parent" st :: Maybe Int
      return $
        [package +-+ method +-+ show state +-+ maybe "" show parent,
         "https://koji.fedoraproject.org/koji/taskinfo?taskID=" ++ show (taskid :: Int),
         formatTime defaultTimeLocale "%c" (utcToLocalTime tz start_time),
         formatTime defaultTimeLocale "%c" (utcToLocalTime tz completion_time),
         "duration: " ++ formatTime defaultTimeLocale "%H:%M:%S" duration
        ]
