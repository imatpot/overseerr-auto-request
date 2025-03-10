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
readEnvOrError name = maybe (error $ envNotSetError name) (return . trim) =<< lookupEnv name

readEnvListOrEmpty :: String -> IO [String]
readEnvListOrEmpty name = maybe [] (dropEmpty . commaSeparated) <$> lookupEnv name

envNotSetError :: String -> String
envNotSetError name = "Environment variable '" ++ name ++ "' not set"
