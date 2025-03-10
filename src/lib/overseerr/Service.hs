module Lib.Overseerr.Service (signIn, getMovie, getTv, requestTv, requestMovie) where

import Control.Monad (join, void)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Debug.Trace (trace)
import Lib.Env (overseerrUrlEnv)
import Lib.Http (httpGetJsonAuthenticated, httpPost, httpPostAuthenticated)
import Lib.Overseerr.Models (MediaDetailsDto, RequestMediaDto (MkRequestMovieDto, MkRequestTvDto), SignInDto (MkSignInDto))
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

overseerrRequestPath :: String
overseerrRequestPath = "/request"

signIn :: String -> String -> IO (Either Int CookieJar)
signIn email password = do
  signInRes <- join $ httpPost <$> ((++ overseerrAuthPath) <$> overseerrBaseUrl) <*> pure (MkSignInDto email password)
  return $ case signInRes of
    Right res -> Right $ responseCookieJar res
    Left status -> Left status

getTv :: CookieJar -> Int -> IO (Either Int MediaDetailsDto)
getTv jar tvId = do
  tvRes <- httpGetJsonAuthenticated jar . (++ overseerrTvPath (show tvId)) =<< overseerrBaseUrl
  return $ rightWhenDecodable tvRes

getMovie :: CookieJar -> Int -> IO (Either Int MediaDetailsDto)
getMovie jar movieId = do
  movieRes <- httpGetJsonAuthenticated jar . (++ overseerrMoviePath (show movieId)) =<< overseerrBaseUrl
  return $ rightWhenDecodable movieRes

requestTv :: CookieJar -> Int -> IO (Either Int ())
requestTv jar tvId = requestMedia jar $ MkRequestTvDto tvId

requestMovie :: CookieJar -> Int -> IO (Either Int ())
requestMovie jar movieId = requestMedia jar $ MkRequestMovieDto movieId

requestMedia :: CookieJar -> RequestMediaDto -> IO (Either Int ())
requestMedia jar dto = do
  request <- join $ httpPostAuthenticated jar <$> ((++ overseerrRequestPath) <$> overseerrBaseUrl) <*> pure dto
  return $ void request

rightWhenDecodable :: (FromJSON a) => Either Int ByteString -> Either Int a
rightWhenDecodable body = case body of
  Left status -> Left status
  Right body' -> case eitherDecode body' of
    Left err -> trace err $ Left (-1)
    Right res -> Right res
