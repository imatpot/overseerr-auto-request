module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Lib.Env (movieIds, tvShowIds, username)

main :: IO ()
main = do
  loadFile defaultConfig
  movies' <- movieIds
  tvShows' <- tvShowIds
  name <- username
  putStrLn $ "Hello, " ++ name
  putStrLn $ "Movies: " ++ show movies'
  putStrLn $ "TV Shows: " ++ show tvShows'
