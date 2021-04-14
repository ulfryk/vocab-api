{-# LANGUAGE DeriveGeneric #-}

module Card.Dto.CardInput where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)


data CardInput = CardInput
  { aSide :: Text,
    aSideDetails :: Maybe Text,
    bSide :: Text
  }
  deriving (Generic, Show)

instance FromJSON CardInput
instance ToJSON CardInput
