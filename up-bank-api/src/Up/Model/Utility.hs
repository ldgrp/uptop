{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Up.Model.Utility where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import GHC.Generics ( Generic )

import qualified Data.Text as T

type CustomerId = T.Text

data Ping = Ping
  { -- | The unique identifier of the authenticated customer.
    pingId :: CustomerId
    -- | A cute emoji that represents the response status.
  , pingStatusEmoji :: T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON Ping where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON Ping where
  parseJSON = withObject "ping" $ \o -> do
    meta <- o .: "meta"
    Ping <$> meta .: "id"
         <*> meta .: "statusEmoji"
