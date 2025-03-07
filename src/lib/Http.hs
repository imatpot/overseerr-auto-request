{-# LANGUAGE OverloadedStrings #-}

module Lib.Http (httpGet, httpPost, HttpGet (MkHttpGet, MkHttpGetWithHeaders), HttpPost (MkHttpPost, MkHttpPostWithHeaders)) where

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Client.Conduit (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyLBS), Response (responseBody), parseRequest)
import Network.HTTP.Simple (getResponseStatusCode, httpLBS)

data HttpGet = MkHttpGetWithHeaders String [(String, String)] | MkHttpGet String

httpGet :: HttpGet -> IO (Either Int ByteString)
httpGet get = case get of
  MkHttpGet url -> httpGetWithHeaders url []
  MkHttpGetWithHeaders url headers -> httpGetWithHeaders url headers

data HttpPost dto = MkHttpPostWithHeaders String dto [(String, String)] | MkHttpPost String dto

httpPost :: (ToJSON dto) => HttpPost dto -> IO (Either Int ByteString)
httpPost post = case post of
  MkHttpPost url body -> httpPostWithHeaders url body []
  MkHttpPostWithHeaders url body headers -> httpPostWithHeaders url body headers

httpGetWithHeaders :: String -> [(String, String)] -> IO (Either Int ByteString)
httpGetWithHeaders url headers = do
  putStrLn $ "GET " ++ url
  request <- parseRequest url
  response <- httpLBS $ request {requestHeaders = mkHeaders headers}
  case getResponseStatusCode response of
    200 -> return $ Right $ responseBody response
    status -> return $ Left status

httpPostWithHeaders :: (ToJSON dto) => String -> dto -> [(String, String)] -> IO (Either Int ByteString)
httpPostWithHeaders url body headers = do
  putStrLn $ "POST " ++ url
  request <- parseRequest url
  response <- httpLBS $ request {method = "POST", requestBody = RequestBodyLBS $ encode body, requestHeaders = mkHeaders headers}
  case getResponseStatusCode response of
    200 -> return $ Right $ responseBody response
    status -> return $ Left status

mkHeaders :: [(String, String)] -> [(CI.CI BS.ByteString, BS.ByteString)]
mkHeaders = map (\(k, v) -> (CI.mk (BS.pack k), BS.pack v))
