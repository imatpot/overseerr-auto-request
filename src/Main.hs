module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Lib.Env (movieIds, tvIds, username, password)
import Lib.Http (HttpPost (MkHttpPost), httpPost)
import Lib.Overseerr.Models (AuthenticationDto (MkAuthenticationDto))
import Lib.Util (secondsToMicroseconds)

main :: IO ()
main = do
  loadFile defaultConfig
  movies <- movieIds
  tv <- tvIds
  name <- username
  pw <- password
  putStrLn $ "Hello, " ++ name
  putStrLn $ "Movies: " ++ show movies
  putStrLn $ "TV Shows: " ++ show tv
  res <- httpPost $ MkHttpPost "http://localhost:8080/api/v1/login" $ MkAuthenticationDto name pw
  print res
  forever $ do
    putStrLn "I'm still running"
    threadDelay $ secondsToMicroseconds 5
