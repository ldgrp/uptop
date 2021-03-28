{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Up.Model.MoneyObject where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Int
import qualified Data.Text as T
import GHC.Generics

data MoneyObject = MoneyObject
  { -- | The ISO 4217 currency code.
    moCurrencyCode :: T.Text,
    -- | The amount of money, formatted as a string in the relevant currency.
    moValue :: T.Text,
    -- | The amount of money in the smallest denomination for the currency, as a 64-bit integer.
    moValueInBaseUnits :: Int64
  }
  deriving (Eq, Generic)

instance ToJSON MoneyObject where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON MoneyObject where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

instance Show MoneyObject where
  show mo = T.unpack $ T.intercalate " " [moCurrencyCode mo, moValue mo]