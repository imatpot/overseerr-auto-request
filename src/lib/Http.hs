{-# LANGUAGE OverloadedStrings #-}

module Lib.Http (httpGet, httpGetAuthenticated, httpGetJson, httpGetJsonAuthenticated, httpPost, httpPostJson, httpPostJsonAuthenticated) where

import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (CookieJar, Request (cookieJar), RequestBody (RequestBodyLBS), createCookieJar)
import Network.HTTP.Client.Conduit (Request (method, requestBody, requestHeaders), Response (responseBody), parseRequest)
import Network.HTTP.Simple (getResponseStatusCode, httpLBS)

httpGet :: String -> IO (Either Int (Response ByteString))
httpGet url = httpGetAuthenticated url $ createCookieJar []

httpPost :: (ToJSON a) => String -> a -> IO (Either Int (Response ByteString))
httpPost url body = httpPostAuthenticated url body $ createCookieJar []

httpGetJson :: String -> IO (Either Int ByteString)
httpGetJson url = fmap responseBody <$> httpGet url

httpPostJson :: (ToJSON a) => String -> a -> IO (Either Int ByteString)
httpPostJson url body = fmap responseBody <$> httpPost url body

httpGetAuthenticated :: String -> CookieJar -> IO (Either Int (Response ByteString))
httpGetAuthenticated url jar = do
  request <- parseRequest url
  response <- httpLBS request {cookieJar = Just jar}
  return $ rightWhen2XX response

httpPostAuthenticated :: (ToJSON a) => String -> a -> CookieJar -> IO (Either Int (Response ByteString))
httpPostAuthenticated url body jar = do
  request <- parseRequest url
  response <- httpLBS request {method = "POST", requestBody = RequestBodyLBS $ encode body, requestHeaders = [("Content-Type", "application/json")], cookieJar = Just jar}
  return $ rightWhen2XX response

httpGetJsonAuthenticated :: String -> CookieJar -> IO (Either Int ByteString)
httpGetJsonAuthenticated url jar = fmap responseBody <$> httpGetAuthenticated url jar

httpPostJsonAuthenticated :: (ToJSON a) => String -> a -> CookieJar -> IO (Either Int ByteString)
httpPostJsonAuthenticated url body jar = fmap responseBody <$> httpPostAuthenticated url body jar

rightWhen2XX :: Response ByteString -> Either Int (Response ByteString)
rightWhen2XX response = case getResponseStatusCode response of
  status | status >= 200 && status < 300 -> Right response
  status -> Left status
