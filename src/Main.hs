module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, join)
import Data.IORef (newIORef, readIORef, writeIORef)
import Lib.Env (emailEnv, passwordEnv)
import Lib.Overseerr.Service (getUnavailableMovies, signIn)
import Lib.Util (minutesToMicroseconds)
import Network.HTTP.Client (createCookieJar)

main :: IO ()
main = do
  loadFile defaultConfig
  cookieJarRef <- newIORef $ createCookieJar []

  signInRes <- join $ signIn <$> emailEnv <*> passwordEnv
  case signInRes of
    Left status -> error $ "Failed to sign in: " ++ show status
    Right cookieJar -> writeIORef cookieJarRef cookieJar

  moviesRes <- join $ getUnavailableMovies <$> readIORef cookieJarRef <*> pure [126486, 51739, 738091]
  case moviesRes of
    Left status -> error $ "Failed to get movies: " ++ show status
    Right movies -> putStrLn $ "Movies: " ++ show movies

  -- requestRes <- join $ requestTv <$> readIORef cookieJarRef <*> pure 82452
  -- case requestRes of
  --   Left status -> error $ "Failed to request TV: " ++ show status
  --   Right _ -> putStrLn "TV requested"

  forever $ do
    putStrLn "I'm still alive"
    threadDelay $ minutesToMicroseconds 1
