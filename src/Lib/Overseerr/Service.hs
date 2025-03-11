module Lib.Overseerr.Service (signIn, getMovie, getShow, requestTvShow, requestMovie, getUnavailableTvShows, getUnavailableMovies, getTvShows, getMovies, requestMovies, requestTvShows) where

import Control.Monad (join, void)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Debug.Trace (trace)
import Lib.Env (overseerrUrlEnv)
import Lib.Http (httpGetJsonAuthenticated, httpPost, httpPostAuthenticated)
import Lib.Overseerr.Models (Availability (Unknown), MediaDetailsDto (mediaInfo), MediaInfoDto (mediaInfoAvailability, mediaInfoRequests), MediaRequestDto (mediaRequestStatus), RequestMediaDto (MkRequestMovieDto, MkRequestShowDto), RequestStatus (Approved), SignInDto (MkSignInDto))
import Network.HTTP.Client (CookieJar, Response (responseCookieJar))

overseerrBaseUrl :: IO String
overseerrBaseUrl = (++ overseerrApiPath) <$> overseerrUrlEnv

overseerrApiPath :: String
overseerrApiPath = "/api/v1"

overseerrAuthPath :: String
overseerrAuthPath = "/auth/local"

overseerrTvShowPath :: String -> String
overseerrTvShowPath tvShowId = "/tv/" ++ tvShowId

overseerrMoviePath :: String -> String
overseerrMoviePath movieId = "/movie/" ++ movieId

overseerrRequestPath :: String
overseerrRequestPath = "/request"

signIn :: String -> String -> IO (Either Int CookieJar)
signIn email password = do
  signInRes <- join $ httpPost <$> ((++ overseerrAuthPath) <$> overseerrBaseUrl) <*> pure (MkSignInDto email password)
  return $ responseCookieJar <$> signInRes

getShow :: CookieJar -> Int -> IO (Either Int MediaDetailsDto)
getShow jar tvShowId = do
  tvShowRes <- httpGetJsonAuthenticated jar . (++ overseerrTvShowPath (show tvShowId)) =<< overseerrBaseUrl
  return $ rightWhenDecodable tvShowRes

getTvShows :: CookieJar -> [Int] -> IO (Either Int [MediaDetailsDto])
getTvShows jar tvShowIds = sequence <$> mapM (getShow jar) tvShowIds

getUnavailableTvShows :: CookieJar -> [Int] -> IO (Either Int [MediaDetailsDto])
getUnavailableTvShows jar tvShowIds = fmap whereUnavailable <$> getTvShows jar tvShowIds

getMovie :: CookieJar -> Int -> IO (Either Int MediaDetailsDto)
getMovie jar movieId = do
  movieRes <- httpGetJsonAuthenticated jar . (++ overseerrMoviePath (show movieId)) =<< overseerrBaseUrl
  return $ rightWhenDecodable movieRes

getMovies :: CookieJar -> [Int] -> IO (Either Int [MediaDetailsDto])
getMovies jar movieIds = sequence <$> mapM (getMovie jar) movieIds

getUnavailableMovies :: CookieJar -> [Int] -> IO (Either Int [MediaDetailsDto])
getUnavailableMovies jar movieIds = fmap whereUnavailable <$> getMovies jar movieIds

requestTvShow :: CookieJar -> Int -> IO (Either Int ())
requestTvShow jar tvShowId = requestMedia jar $ MkRequestShowDto tvShowId

requestTvShows :: CookieJar -> [Int] -> IO (Either Int ())
requestTvShows jar tvShowIds = sequence_ <$> mapM (requestTvShow jar) tvShowIds

requestMovie :: CookieJar -> Int -> IO (Either Int ())
requestMovie jar movieId = trace ("Requesting movie " ++ show movieId) $ requestMedia jar $ MkRequestMovieDto movieId

requestMovies :: CookieJar -> [Int] -> IO (Either Int ())
requestMovies jar movieIds = sequence_ <$> mapM (requestMovie jar) movieIds

requestMedia :: CookieJar -> RequestMediaDto -> IO (Either Int ())
requestMedia jar dto = do
  request <- join $ httpPostAuthenticated jar <$> ((++ overseerrRequestPath) <$> overseerrBaseUrl) <*> pure dto
  return $ void request

rightWhenDecodable :: (FromJSON a) => Either Int ByteString -> Either Int a
rightWhenDecodable = either Left (either (const $ Left (-1)) Right . eitherDecode)

whereUnavailable :: [MediaDetailsDto] -> [MediaDetailsDto]
whereUnavailable = filter (maybe True isUnrequested . mediaInfo)

isUnrequested :: MediaInfoDto -> Bool
isUnrequested x = isUnvailable x && hasNoPendingRequests x

isUnvailable :: MediaInfoDto -> Bool
isUnvailable = (== Unknown) . mediaInfoAvailability

hasNoPendingRequests :: MediaInfoDto -> Bool
hasNoPendingRequests = not . any ((== Approved) . mediaRequestStatus) . mediaInfoRequests
