module Lib.Env (username, password, overseerrUrl, movieIds, tvIds) where

import Lib.Util (commaSeparated, dropEmpty, trim)
import System.Environment (lookupEnv)

username :: IO String
username = readEnvOrError "USERNAME"

password :: IO String
password = readEnvOrError "PASSWORD"

overseerrUrl :: IO String
overseerrUrl = readEnvOrError "OVERSEERR_URL"

movieIds :: IO [String]
movieIds = readEnvListOrEmpty "MOVIES"

tvIds :: IO [String]
tvIds = readEnvListOrEmpty "TV"

readEnvOrError :: String -> IO String
readEnvOrError name = do
  env <- lookupEnv name
  case env of
    Just val -> return $ trim val
    Nothing -> error $ errorEnvNotSet name

readEnvListOrEmpty :: String -> IO [String]
readEnvListOrEmpty name = do
  env <- lookupEnv name
  case env of
    Just val -> return $ dropEmpty $ commaSeparated val
    Nothing -> return []

errorEnvNotSet :: String -> String
errorEnvNotSet name = "Environment variable '" ++ name ++ "' not set"
