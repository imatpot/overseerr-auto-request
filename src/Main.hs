module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Lib.Env (movieIds, tvIds, username)
import Lib.Http (httpGet, httpPost)
import Lib.Overseerr.Models (AuthenticationDto (MkAuthenticationDto))
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
  res <- httpGet "https://jsonplaceholder.typicode.com/todos/1"
  print res
  let auth = MkAuthenticationDto "name" "password"
  res' <- httpPost "http://localhost:8080/api/v1/auth/login" auth
  print res'
  forever $ do
    putStrLn "I'm still running"
    threadDelay $ secondsToMicroseconds 5
