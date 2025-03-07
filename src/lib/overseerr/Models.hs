{-# LANGUAGE OverloadedStrings #-}

module Lib.Overseerr.Models where

import Data.Aeson (FromJSON (parseJSON), withObject, (.!=), (.:), (.:?))

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

instance FromJSON MediaDetailsDto where
  parseJSON = withObject "MediaDetailsDto" $ \json -> do
    mediaDetailsId' <- json .: "id"
    mediaInfo' <- json .:? "mediaInfo"
    return $ MkMediaDetailsDto mediaDetailsId' mediaInfo'

instance FromJSON MediaInfoDto where
  parseJSON = withObject "MediaInfoDto" $ \json -> do
    mediaInfoAvailability' <- toEnum <$> json .: "availability"
    mediaInfoRequests' <- json .:? "requests" .!= []
    return $ MkMediaInfoDto mediaInfoAvailability' mediaInfoRequests'

instance FromJSON MediaRequestDto where
  parseJSON = withObject "MediaRequestDto" $ \json -> do
    mediaRequestId' <- json .: "id"
    mediaRequestStatus' <- toEnum <$> json .: "status"
    return $ MkMediaRequestDto mediaRequestId' mediaRequestStatus'

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
