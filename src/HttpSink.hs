-- This is a simple HTTP server that listens on port 8080 and logs all incoming requests to the console.
-- It should be used for debugging purposes only.
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Types (status200)
import Network.Wai (Application, Request (rawPathInfo, requestMethod), responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  putStrLn "Starting server on http://localhost:8080"
  run 8080 server

server :: Application
server req respond = do
  body <- strictRequestBody req
  putStrLn $ show (requestMethod req) ++ " " ++ show (rawPathInfo req) ++ " " ++ show body
  respond $ responseLBS status200 [("Content-Type", "text/json")] "{\"status\": \"ok\"}"
