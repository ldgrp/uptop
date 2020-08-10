{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Up.Model.Tag where 

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import GHC.Generics

import qualified Data.Text as T


data Tag = Tag
    { tagId :: T.Text
    , tagTransaction :: Maybe T.Text
    } deriving (Eq, Show, Generic)

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
