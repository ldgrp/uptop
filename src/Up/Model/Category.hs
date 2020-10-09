{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Up.Model.Category where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import GHC.Generics ( Generic )

import qualified Data.Text as T

type CategoryId = T.Text

data Category = Category
  { -- | The unique identifier for this category. 
    -- This is a human-readable but URL-safe value.
    categoryId :: CategoryId
    -- | The name of this category as seen in the Up application.
  , categoryName :: T.Text
  , categoryParent :: Maybe CategoryId
  , categoryChildren :: Maybe [CategoryId]
  } deriving (Eq, Show, Generic)

data Categories = Categories { 
    getCategories :: [Category] 
  } deriving (Eq, Show, Generic)

instance ToJSON Categories where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON Categories where
  parseJSON = withObject "categories" $ \o -> do
    c <- o .: "data"
    pure (Categories c)

instance ToJSON Category where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON Category where
  parseJSON = withObject "category" $ \o -> 
    let attributes = (o .: "attributes" >>=)
        relationships = (o .: "relationships" >>=)
        parent = relationships (.: "parent") >>= (.:? "data") >>= mapM (.: "id")
        children = relationships (.: "children") >>= (.:? "data") >>= mapM (mapM (.: "id"))
     in Category <$> o .: "id"
                 <*> attributes (.: "name")
                 <*> parent
                 <*> children