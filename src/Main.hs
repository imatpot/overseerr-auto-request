module Main where

import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, join)
import Data.Either (fromLeft, fromRight)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Debug.Trace (trace)
import GHC.IO.Handle (BufferMode (NoBuffering), hSetBuffering)
import Lib.Env (debounceEnv, emailEnv, movieIdsEnv, passwordEnv, tvShowIdsEnv)
import Lib.Overseerr.Models (MediaDetailsDto (mediaDetailsId))
import Lib.Overseerr.Service (getUnavailableMovies, getUnavailableTvShows, requestMovies, requestTvShows, signIn)
import Lib.Util (secondsToMicroseconds)
import Network.HTTP.Client (CookieJar, createCookieJar)
import System.IO (stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  onMissingFile (loadFile defaultConfig) (pure ())

  cookieJarRef <- newIORef $ createCookieJar []
  updateCookieJar cookieJarRef

  debounce <- debounceEnv

  forever $ do
    movies <- join $ getUnavailableMovies <$> readIORef cookieJarRef <*> movieIdsEnv
    tvShows <- join $ getUnavailableTvShows <$> readIORef cookieJarRef <*> tvShowIdsEnv

    handleGetMediaError cookieJarRef movies
    handleGetMediaError cookieJarRef tvShows

    let movies' = fromRight [] movies
    let tvShows' = fromRight [] tvShows

    putStrLn $ "Requesting movies: " ++ show (map mediaDetailsId movies')
    putStrLn $ "Requesting TV shows: " ++ show (map mediaDetailsId tvShows')

    handleRequestMediaError <$> join (requestMovies <$> readIORef cookieJarRef <*> movieIdsEnv)
    handleRequestMediaError <$> join (requestTvShows <$> readIORef cookieJarRef <*> tvShowIdsEnv)

    putStrLn $ "Waiting " ++ show debounce ++ " seconds for next request..."
    threadDelay $ secondsToMicroseconds debounce

--

updateCookieJar :: IORef CookieJar -> IO ()
updateCookieJar cookieJarRef = do
  signInRes <- join $ signIn <$> emailEnv <*> passwordEnv
  case signInRes of
    Left status -> error $ "Failed to sign in: " ++ show status
    Right cookieJar -> writeIORef cookieJarRef cookieJar

handleGetMediaError :: IORef CookieJar -> Either Int a -> IO ()
handleGetMediaError cookieJarRef res = case fromLeft (-1) res of
  (-1) -> pure () -- value was Right
  403 -> trace "Refreshing cookies..." $ updateCookieJar cookieJarRef
  status -> error $ "Failed to get unavailable media: " ++ show status

handleRequestMediaError :: Either Int () -> ()
handleRequestMediaError res = case res of
  Right _ -> ()
  status -> error $ "Failed to request media: " ++ show status
