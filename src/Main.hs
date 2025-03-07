module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Lib.Env (movieIds, tvIds, username)
import Lib.Util (secondsToMicroseconds)

main :: IO ()
main = do
  loadFile defaultConfig
  movies <- movieIds
  tv <- tvIds
  name <- username
  putStrLn $ "Hello, " ++ name
  putStrLn $ "Movies: " ++ show movies
  putStrLn $ "TV Shows: " ++ show tv
  forever $ do
    putStrLn "I'm still running"
    threadDelay $ secondsToMicroseconds 1
