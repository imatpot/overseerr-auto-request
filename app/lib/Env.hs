module Lib.Env where

import Lib.Util (commaSeparated, trim)
import System.Environment (getEnv, lookupEnv)

username :: IO String
username = trim <$> getEnv "USERNAME"

password :: IO String
password = trim <$> getEnv "PASSWORD"

overseerrUrl :: IO String
overseerrUrl = trim <$> getEnv "OVERSEERR_URL"

movieIds :: IO [String]
movieIds = do
  movieEnv <- lookupEnv "MOVIES"
  case movieEnv of
    Just ids -> return $ commaSeparated ids
    Nothing -> return []

tvShowIds :: IO [String]
tvShowIds = do
  tvShowsEnv <- lookupEnv "TV_SHOWS"
  case tvShowsEnv of
    Just ids -> return $ commaSeparated ids
    Nothing -> return []
