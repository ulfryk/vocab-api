{-# LANGUAGE DeriveGeneric #-}

module Card.Dto.CardUpdate where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data CardUpdate = CardUpdate
  { setASide :: Maybe Text,
    setASideDetails :: Maybe Text,
    setBSide :: Maybe Text,
    setArchived :: Maybe Bool,
    setSuspended :: Maybe Bool
  }
  deriving (Generic, Show)

instance ToJSON CardUpdate

instance FromJSON CardUpdate
