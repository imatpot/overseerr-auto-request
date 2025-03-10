module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, join)
import Data.IORef (newIORef, readIORef, writeIORef)
import Lib.Env (emailEnv, passwordEnv)
import Lib.Overseerr.Service (getMovie, signIn)
import Lib.Util (secondsToMicroseconds)
import Network.HTTP.Client (createCookieJar)

main :: IO ()
main = do
  loadFile defaultConfig
  jarRef <- newIORef $ createCookieJar []
  signInRes <- join $ signIn <$> emailEnv <*> passwordEnv
  case signInRes of
    Left status -> error $ "Failed to sign in: " ++ show status
    Right jar -> writeIORef jarRef jar
  movieRes <- getMovie 126486 =<< readIORef jarRef
  case movieRes of
    Left status -> error $ "Failed to get movie: " ++ show status
    Right mediaDetailsDto -> print mediaDetailsDto
  tvRes <- getMovie 21671 =<< readIORef jarRef
  case tvRes of
    Left status -> error $ "Failed to get tv: " ++ show status
    Right mediaDetailsDto -> print mediaDetailsDto
  forever $ do
    putStrLn "I'm still running"
    threadDelay $ secondsToMicroseconds 5
