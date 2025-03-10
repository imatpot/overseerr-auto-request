module Main where

import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, join)
import Data.IORef (newIORef, readIORef, writeIORef)
import GHC.IO.Handle (BufferMode (NoBuffering), hSetBuffering)
import Lib.Env (emailEnv, passwordEnv, movieIdsEnv)
import Lib.Overseerr.Service (getUnavailableMovies, signIn)
import Lib.Util (minutesToMicroseconds)
import Network.HTTP.Client (createCookieJar)
import System.IO (stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  onMissingFile (loadFile defaultConfig) (pure ())

  cookieJarRef <- newIORef $ createCookieJar []

  signInRes <- join $ signIn <$> emailEnv <*> passwordEnv
  case signInRes of
    Left status -> error $ "Failed to sign in: " ++ show status
    Right cookieJar -> writeIORef cookieJarRef cookieJar

  moviesRes <- join $ getUnavailableMovies <$> readIORef cookieJarRef <*> movieIdsEnv
  case moviesRes of
    Left status -> error $ "Failed to get movies: " ++ show status
    Right movies -> putStrLn $ "Movies: " ++ show movies

  -- requestRes <- join $ requestShow <$> readIORef cookieJarRef <*> pure 82452
  -- case requestRes of
  --   Left status -> error $ "Failed to request show: " ++ show status
  --   Right _ -> putStrLn "Show requested"

  forever $ do
    putStrLn "I'm still alive"
    threadDelay $ minutesToMicroseconds 1
