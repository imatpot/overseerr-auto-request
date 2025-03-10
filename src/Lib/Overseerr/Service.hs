module Lib.Overseerr.Service (signIn, getMovie, getTv, requestTv, requestMovie, getUnavailableTvs, getUnavailableMovies, getTvs, getMovies) where

import Control.Monad (join, void)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Debug.Trace (trace)
import Lib.Env (overseerrUrlEnv)
import Lib.Http (httpGetJsonAuthenticated, httpPost, httpPostAuthenticated)
import Lib.Overseerr.Models (Availability (Unknown), MediaDetailsDto (mediaInfo), MediaInfoDto (mediaInfoAvailability, mediaInfoRequests), MediaRequestDto (mediaRequestStatus), RequestMediaDto (MkRequestMovieDto, MkRequestTvDto), RequestStatus (Approved), SignInDto (MkSignInDto))
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

getTvs :: CookieJar -> [Int] -> IO (Either Int [MediaDetailsDto])
getTvs jar tvIds = sequence <$> mapM (getTv jar) tvIds

getUnavailableTvs :: CookieJar -> [Int] -> IO (Either Int [MediaDetailsDto])
getUnavailableTvs jar tvIds = fmap whereUnavailable <$> getTvs jar tvIds

getMovie :: CookieJar -> Int -> IO (Either Int MediaDetailsDto)
getMovie jar movieId = do
  movieRes <- httpGetJsonAuthenticated jar . (++ overseerrMoviePath (show movieId)) =<< overseerrBaseUrl
  return $ rightWhenDecodable movieRes

getMovies :: CookieJar -> [Int] -> IO (Either Int [MediaDetailsDto])
getMovies jar movieIds = sequence <$> mapM (getMovie jar) movieIds

getUnavailableMovies :: CookieJar -> [Int] -> IO (Either Int [MediaDetailsDto])
getUnavailableMovies jar movieIds = fmap whereUnavailable <$> getMovies jar movieIds

requestTv :: CookieJar -> Int -> IO (Either Int ())
requestTv jar tvId = requestMedia jar $ MkRequestTvDto tvId

requestTvs :: CookieJar -> [Int] -> IO (Either Int ())
requestTvs jar tvIds = sequence_ <$> mapM (requestTv jar) tvIds

requestMovie :: CookieJar -> Int -> IO (Either Int ())
requestMovie jar movieId = requestMedia jar $ MkRequestMovieDto movieId

requestMovies :: CookieJar -> [Int] -> IO (Either Int ())
requestMovies jar movieIds = sequence_ <$> mapM (requestMovie jar) movieIds

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

whereUnavailable :: [MediaDetailsDto] -> [MediaDetailsDto]
whereUnavailable = filter (maybe True isUnrequested . mediaInfo)

isUnrequested :: MediaInfoDto -> Bool
isUnrequested x = isUnvailable x && hasNoPendingRequests x

isUnvailable :: MediaInfoDto -> Bool
isUnvailable = (== Unknown) . mediaInfoAvailability

hasNoPendingRequests :: MediaInfoDto -> Bool
hasNoPendingRequests = not . any ((== Approved) . mediaRequestStatus) . mediaInfoRequests
