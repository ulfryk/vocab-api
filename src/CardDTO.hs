{-# LANGUAGE DeriveGeneric #-}

module CardDTO where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Card = Card
  { _id :: Text,
    aSide :: Text,
    -- , aSideDetails :: Text
    bSide :: Text
    -- , createdAt :: Text
    -- , updatedAt :: Text
    -- , suspended :: Bool
    -- , archived :: Bool
  }
  deriving (Generic, Show)

instance ToJSON Card

instance FromJSON Card

data CardInput = CardInput
  { initASide :: Text,
    initBSide :: Text
  }
  deriving (Generic, Show)

instance FromJSON CardInput
