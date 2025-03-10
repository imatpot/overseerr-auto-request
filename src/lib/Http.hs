{-# LANGUAGE OverloadedStrings #-}

module Lib.Http (httpGet, httpPost, httpGetJson, httpPostJson, httpGetAuthenticated, httpPostAuthenticated, httpGetJsonAuthenticated, httpPostJsonAuthenticated) where

import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy (ByteString)
import Debug.Trace (trace)
import Network.HTTP.Client (CookieJar, Request (cookieJar), RequestBody (RequestBodyLBS), createCookieJar)
import Network.HTTP.Client.Conduit (Request (method, requestBody, requestHeaders), Response (responseBody), parseRequest)
import Network.HTTP.Simple (getResponseStatusCode, httpLBS)

httpGet :: String -> IO (Either Int (Response ByteString))
httpGet = httpGetAuthenticated $ createCookieJar []

httpPost :: (ToJSON a) => String -> a -> IO (Either Int (Response ByteString))
httpPost = httpPostAuthenticated $ createCookieJar []

httpGetJson :: String -> IO (Either Int ByteString)
httpGetJson url = fmap responseBody <$> httpGet url

httpPostJson :: (ToJSON a) => String -> a -> IO (Either Int ByteString)
httpPostJson url body = fmap responseBody <$> httpPost url body

httpGetAuthenticated :: CookieJar -> String -> IO (Either Int (Response ByteString))
httpGetAuthenticated jar url = do
  request <- parseRequest url
  response <- httpLBS request {cookieJar = Just jar}
  return $ rightWhen2XX response

httpPostAuthenticated :: (ToJSON a) => CookieJar -> String -> a -> IO (Either Int (Response ByteString))
httpPostAuthenticated jar url body = do
  request <- parseRequest url
  response <- httpLBS request {method = "POST", requestBody = RequestBodyLBS $ encode body, requestHeaders = [("Content-Type", "application/json")], cookieJar = Just jar}
  return $ rightWhen2XX response

httpGetJsonAuthenticated :: CookieJar -> String -> IO (Either Int ByteString)
httpGetJsonAuthenticated jar url = fmap responseBody <$> httpGetAuthenticated jar url

httpPostJsonAuthenticated :: (ToJSON a) => CookieJar -> String -> a -> IO (Either Int ByteString)
httpPostJsonAuthenticated jar url body = trace url $ fmap responseBody <$> httpPostAuthenticated jar url body

rightWhen2XX :: Response ByteString -> Either Int (Response ByteString)
rightWhen2XX response = case getResponseStatusCode response of
  status | status >= 200 && status < 300 -> Right response
  status -> Left status
