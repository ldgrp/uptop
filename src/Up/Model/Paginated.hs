{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Up.Model.Paginated where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import GHC.Generics

import qualified Data.Text as T

-- | A (flattened) response of paginated endpoint
data Paginated a = Paginated
    { -- | An array of resources specific to the endpoint
      paginatedData :: [a]
      -- | A link to move to the previous page
    , paginatedPrev :: Maybe T.Text
      -- | A link to move to the next page
    , paginatedNext :: Maybe T.Text
    } deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Paginated a) where
    toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON a => FromJSON (Paginated a) where
    parseJSON = withObject "paginated" $ \o ->
        let links = (o .: "links" >>=)
         in Paginated <$> o .: "data"
                      <*> links (.:? "prev")
                      <*> links (.:? "next")
