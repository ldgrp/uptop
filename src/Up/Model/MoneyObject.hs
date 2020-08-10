{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Up.Model.MoneyObject where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Int
import GHC.Generics

import qualified Data.Text as T


data MoneyObject = MoneyObject
    { -- | The ISO 4217 currency code.
      moCurrencyCode :: T.Text
      -- | The amount of money, formatted as a string in the relevant currency.
    , moValue :: T.Text
      -- | The amount of money in the smallest denomination for the currency, as a 64-bit integer.
    , moValueInBaseUnits :: Int64
    } deriving (Eq, Show, Generic)

instance ToJSON MoneyObject where
    toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON MoneyObject where
    parseJSON = genericParseJSON $ aesonPrefix camelCase

