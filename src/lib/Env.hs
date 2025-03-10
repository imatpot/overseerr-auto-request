module Lib.Env (emailEnv, passwordEnv, overseerrUrlEnv, movieIdsEnv, tvIdsEnv) where

import Lib.Util (commaSeparated, dropEmpty, trim)
import System.Environment (lookupEnv)

emailEnv :: IO String
emailEnv = readEnvOrError "EMAIL"

passwordEnv :: IO String
passwordEnv = readEnvOrError "PASSWORD"

overseerrUrlEnv :: IO String
overseerrUrlEnv = readEnvOrError "OVERSEERR_URL"

movieIdsEnv :: IO [String]
movieIdsEnv = readEnvListOrEmpty "MOVIES"

tvIdsEnv :: IO [String]
tvIdsEnv = readEnvListOrEmpty "TV"

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
