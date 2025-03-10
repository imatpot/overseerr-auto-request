module Lib.Env (emailEnv, passwordEnv, overseerrUrlEnv, movieIdsEnv, tvShowIdsEnv, debounceEnv) where

import Data.Maybe (mapMaybe)
import Lib.Util (commaSeparated, dropEmpty, trim)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

debounceEnv :: IO Double
debounceEnv = maybe 300 read <$> lookupEnv "DEBOUNCE_SECONDS"

emailEnv :: IO String
emailEnv = readEnvOrError "EMAIL"

passwordEnv :: IO String
passwordEnv = readEnvOrError "PASSWORD"

overseerrUrlEnv :: IO String
overseerrUrlEnv = readEnvOrError "OVERSEERR_URL"

movieIdsEnv :: IO [Int]
movieIdsEnv = mapMaybe readMaybe <$> readEnvListOrEmpty "MOVIES"

tvShowIdsEnv :: IO [Int]
tvShowIdsEnv = mapMaybe readMaybe <$> readEnvListOrEmpty "TV_SHOWS"

readEnvOrError :: String -> IO String
readEnvOrError name = maybe (error $ envNotSetError name) (return . trim) =<< lookupEnv name

readEnvListOrEmpty :: String -> IO [String]
readEnvListOrEmpty name = maybe [] (dropEmpty . commaSeparated) <$> lookupEnv name

envNotSetError :: String -> String
envNotSetError name = "Environment variable '" ++ name ++ "' not set"
