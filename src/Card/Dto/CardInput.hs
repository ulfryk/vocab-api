{-# LANGUAGE DeriveGeneric #-}

module Card.Dto.CardInput where
  
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)


data CardInput = CardInput
  { initASide :: Text,
    initASideDetails :: Maybe Text,
    initBSide :: Text
  }
  deriving (Generic, Show)

instance FromJSON CardInput
instance ToJSON CardInput
