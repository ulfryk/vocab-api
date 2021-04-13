{-# LANGUAGE DeriveGeneric #-}

module CardDTO where

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

data CardInput = CardInput
  { initASide :: Text,
    initASideDetails :: Maybe Text,
    initBSide :: Text
  }
  deriving (Generic, Show)

instance FromJSON CardInput
