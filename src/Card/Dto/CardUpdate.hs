{-# LANGUAGE DeriveGeneric #-}

module Card.Dto.CardUpdate where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data CardUpdate = CardUpdate
  { aSide :: Maybe Text,
    aSideDetails :: Maybe Text,
    bSide :: Maybe Text,
    archived :: Maybe Bool,
    suspended :: Maybe Bool
  }
  deriving (Generic, Show)

instance ToJSON CardUpdate

instance FromJSON CardUpdate
