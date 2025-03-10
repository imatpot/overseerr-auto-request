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

data RequestMediaDto = MkRequestMovieDto {requestMovieId :: Int} | MkRequestTvDto {requestTvId :: Int}
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
  toJSON (MkRequestTvDto tvId) = object ["mediaId" .= tvId, "mediaType" .= ("tv" :: String), "seasons" .= ("all" :: String)]

instance Enum Availability where
  fromEnum Unknown = 1
  fromEnum Pending = 2
  fromEnum Processing = 3
  fromEnum PartiallyAvailable = 4
  fromEnum Available = 5
  toEnum 1 = Unknown
  toEnum 2 = Pending
  toEnum 3 = Processing
  toEnum 4 = PartiallyAvailable
  toEnum 5 = Available
  toEnum n = error $ "Illegal availability: " ++ show n

instance Enum RequestStatus where
  fromEnum PendingApproval = 1
  fromEnum Approved = 2
  fromEnum Declined = 3
  toEnum 1 = PendingApproval
  toEnum 2 = Approved
  toEnum 3 = Declined
  toEnum n = error $ "Illegal request status: " ++ show n
