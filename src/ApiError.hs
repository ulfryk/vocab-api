{-# LANGUAGE DeriveGeneric #-}

module ApiError ( apiErr, ApiError ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype ApiError = ApiError { error :: Text}
  deriving (Generic, Show)
  
apiErr :: Text -> ApiError
apiErr = ApiError

instance ToJSON ApiError

instance FromJSON ApiError
