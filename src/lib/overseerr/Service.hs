module Lib.Overseerr.Service (signIn, getMovie, getTv) where

import Data.Aeson (decode, eitherDecode)
import Debug.Trace (trace)
import Lib.Env (overseerrUrlEnv)
import Lib.Http
import Lib.Overseerr.Models
import Network.HTTP.Client (CookieJar, Response (responseCookieJar))

overseerrBaseUrl :: IO String
overseerrBaseUrl = (++ overseerrApiPath) <$> overseerrUrlEnv

overseerrApiPath :: String
overseerrApiPath = "/api/v1"

overseerrAuthPath :: String
overseerrAuthPath = "/auth/local"

overseerrTvPath :: String -> String
overseerrTvPath tvId = "/tv/" ++ tvId

overseerrMoviePath :: String -> String
overseerrMoviePath movieId = "/movie/" ++ movieId

signIn :: String -> String -> IO (Either Int CookieJar)
signIn email password = do
  baseUrl <- overseerrBaseUrl
  let authUrl = baseUrl ++ overseerrAuthPath
  let authDto = MkAuthenticationDto email password
  signInRes <- httpPost authUrl authDto
  return $ case signInRes of
    Right res -> Right $ responseCookieJar res
    Left status -> Left status

getTv :: Int -> CookieJar -> IO (Either Int MediaDetailsDto)
getTv tvId jar = do
  baseUrl <- overseerrBaseUrl
  let tvUrl = baseUrl ++ overseerrTvPath (show tvId)
  tvRes <- httpGetJsonAuthenticated tvUrl jar
  return $ case tvRes of
    Left status -> Left status
    Right body -> case decode body of
      Just dto -> Right dto
      Nothing -> Left (-1)

getMovie :: Int -> CookieJar -> IO (Either Int MediaDetailsDto)
getMovie movieId jar = do
  baseUrl <- overseerrBaseUrl
  let movieUrl = baseUrl ++ overseerrMoviePath (show movieId)
  movieRes <- httpGetJsonAuthenticated movieUrl jar
  return $ case movieRes of
    Left status -> Left status
    Right body -> case eitherDecode body of
      Right dto -> Right dto
      Left err -> trace err Left (-1)
