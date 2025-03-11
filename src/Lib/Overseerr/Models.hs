{-# LANGUAGE OverloadedStrings #-}

module Lib.Overseerr.Models where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.!=), (.:), (.:?), (.=))

data Availability = Unknown | Pending | Processing | PartiallyAvailable | Available
  deriving (Show, Eq)

data RequestStatus = PendingApproval | Approved | Declined
  deriving (Show, Eq)

data MediaDetailsDto = MkMediaDetailsDto {mediaDetailsId :: Int, mediaInfo :: Maybe MediaInfoDto}
  deriving (Show)

data MediaInfoDto = MkMediaInfoDto {mediaInfoAvailability :: Availability, mediaInfoRequests :: [MediaRequestDto]}
  deriving (Show)

data MediaRequestDto = MkMediaRequestDto {mediaRequestId :: Int, mediaRequestStatus :: RequestStatus}
  deriving (Show)

data SignInDto = MkSignInDto {signInEmail :: String, signInPassword :: String}
  deriving (Show)

data RequestMediaDto = MkRequestMovieDto {requestMovieId :: Int} | MkRequestShowDto {requestShowId :: Int}
  deriving (Show)

instance FromJSON MediaDetailsDto where
  parseJSON = withObject "MediaDetailsDto" $ \json -> do
    MkMediaDetailsDto <$> json .: "id" <*> json .:? "mediaInfo"

instance FromJSON MediaInfoDto where
  parseJSON = withObject "MediaInfoDto" $ \json -> do
    MkMediaInfoDto <$> (toEnum <$> (json .: "status")) <*> json .:? "requests" .!= []

instance FromJSON MediaRequestDto where
  parseJSON = withObject "MediaRequestDto" $ \json -> do
    MkMediaRequestDto <$> json .: "id" <*> (toEnum <$> json .: "status")

instance ToJSON SignInDto where
  toJSON (MkSignInDto email password) = object ["email" .= email, "password" .= password]

instance ToJSON RequestMediaDto where
  toJSON (MkRequestMovieDto movieId) = object ["mediaId" .= movieId, "mediaType" .= ("movie" :: String)]
  toJSON (MkRequestShowDto tvShowId) = object ["mediaId" .= tvShowId, "mediaType" .= ("tv" :: String), "seasons" .= ("all" :: String)]

instance Enum Availability where
  fromEnum x = case x of
    Unknown -> 1
    Pending -> 2
    Processing -> 3
    PartiallyAvailable -> 4
    Available -> 5
  toEnum x = case x of
    1 -> Unknown
    2 -> Pending
    3 -> Processing
    4 -> PartiallyAvailable
    5 -> Available
    _ -> error $ "Illegal availability: " ++ show x

instance Enum RequestStatus where
  fromEnum x = case x of
    PendingApproval -> 1
    Approved -> 2
    Declined -> 3
  toEnum x = case x of
    1 -> PendingApproval
    2 -> Approved
    3 -> Declined
    _ -> error $ "Illegal request status: " ++ show x
