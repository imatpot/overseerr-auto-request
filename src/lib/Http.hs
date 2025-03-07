{-# LANGUAGE OverloadedStrings #-}

module Lib.Http where

import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client.Conduit (Request (method, requestBody), RequestBody (RequestBodyLBS), Response (responseBody), parseRequest)
import Network.HTTP.Simple (getResponseStatusCode, httpLBS)

httpGet :: String -> IO (Either Int ByteString)
httpGet url = do
  putStrLn $ "GET " ++ url
  request <- parseRequest url
  response <- httpLBS request
  case getResponseStatusCode response of
    200 -> return $ Right $ responseBody response
    status -> return $ Left status

httpPost :: (ToJSON dto) => String -> dto -> IO (Either Int ByteString)
httpPost url body = do
  putStrLn $ "POST " ++ url
  request <- parseRequest url
  response <- httpLBS $ request {method = "POST", requestBody = RequestBodyLBS $ encode body}
  case getResponseStatusCode response of
    200 -> return $ Right $ responseBody response
    status -> return $ Left status
