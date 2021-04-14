{-# LANGUAGE DeriveGeneric #-}

module Card.Dto.Card where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data Card = Card
  { _id :: Text,
    aSide :: Text,
    aSideDetails :: Maybe Text,
    bSide :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    suspended :: Bool,
    archived :: Bool
  }
  deriving (Generic, Show)

instance ToJSON Card

instance FromJSON Card
