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
import Distribution.Koji
import Distribution.Koji.API
import SimpleCmd
import SimpleCmdArgs
import System.FilePath

main :: IO ()
main =
  simpleCmdArgs' Nothing "koji-query" "Helper client for koji queries" $
  program <$>
  optional (strOptionWith 'u' "user" "USER" "Koji user")
  <*> optional (strArg "DAY")

program :: Maybe String -> Maybe String -> IO ()
program muser mdate = do
  date <- cmd "date" ["+%F", "--date=" ++ fromMaybe "yesterday" mdate]
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
        [--("method", ValueString "build"),
         ("owner", ValueInt (getID owner)),
         ("startedAfter", ValueString date),
         ("decode", ValueBool True)]
        [("limit",ValueInt 10)]
        >>= mapM_ printTask
  where
    printTask :: Struct -> IO ()
    printTask task = do
      putStrLn ""
      whenJust (render task) (mapM_ putStrLn)

    render :: Struct -> Maybe [String]
    render st = do
      arch <- lookupStruct "arch" st
      completion_time <- lookupStruct "completion_time" st
      start_time <- lookupStruct "start_time" st
      taskid <- lookupStruct "id" st
      state <- getTaskState st
      request <- head <$> lookupStruct "request" st >>= getString
      let parent = lookupStruct "parent" st :: Maybe Int
      return $
        [takeFileName request +-+ arch +-+ show state +-+ maybe "" show parent,
         "https://koji.fedoraproject.org/koji/taskinfo?taskID=" ++ show (taskid :: Int),
         start_time, completion_time
        ]
