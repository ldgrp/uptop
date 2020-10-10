{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Up.Model.Tag where 

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import GHC.Generics

import qualified Data.Text as T

type TagId = T.Text
data Tag = Tag
    { tagId :: TagId
    , tagTransaction :: Maybe T.Text
    } deriving (Eq, Show, Generic)

data Tags = Tags [Tag]
    deriving (Eq, Show, Generic)

instance ToJSON Tags where
    toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON Tags where
  parseJSON = withObject "tags" $ \o -> do
    t <- o .: "data"
    pure (Tags t)

instance ToJSON Tag where
    toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON Tag where
    parseJSON = withObject "tag" $ \o -> do
        let transactions = ((o .: "relationships") >>= (.: "transactions") >>= (.: "links") >>=)
        Tag <$> o .: "id"
            <*> transactions (.: "related")

data TagInput = TagInput 
    { tagInputId :: T.Text
    } deriving (Eq, Show, Generic)

instance ToJSON TagInput where
    toEncoding (TagInput i) = 
        let tag = "tag" :: String
         in pairs ("type" .= tag <> "id" .= i)

instance FromJSON TagInput where
    parseJSON = withObject "tag input" $ \o -> do
        TagInput <$> o .: "id"
