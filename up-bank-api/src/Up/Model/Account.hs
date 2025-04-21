{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Up.Model.Account where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import qualified Data.Char as C
import qualified Data.Text as T
import GHC.Generics (Generic)
import Up.Model.MoneyObject (MoneyObject)

type AccountId = T.Text

-- | A (flattened) Up Account
data Account = Account
  { -- | The unique identifier for this account.
    accountId :: AccountId,
    -- | The name associated with the account in the Up application.
    accountDisplayName :: T.Text,
    -- | The bank account type of this account. See @AccountType@.
    accountAccountType :: AccountType,
    -- | The ownership structure for this account. See @OwnershipType@.
    accountOwnershipType :: OwnershipType,
    -- | The available balance of the account, taking into account any amounts that are currently on hold.
    accountBalance :: MoneyObject,
    -- | The date-time at which this account was first opened.
    accountCreatedAt :: T.Text,
    -- | The link to retrieve the related resource(s) in this relationship.
    accountTransactions :: Maybe T.Text,
    -- | The canonical link to this resource within the API.
    accountSelf :: Maybe T.Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Account where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON Account where
  parseJSON = withObject "account" $ \o ->
    let attributes = (o .: "attributes" >>=)
        transactions = ((o .: "relationships") >>= (.: "transactions") >>= (.: "links") >>=)
        links = (o .: "links" >>=)
     in Account <$> o .: "id"
          <*> attributes (.: "displayName")
          <*> attributes (.: "accountType")
          <*> attributes (.: "ownershipType")
          <*> attributes (.: "balance")
          <*> attributes (.: "createdAt")
          <*> transactions (.: "related")
          <*> links (.: "self")

-- | Possible bank account type of an 'Account'.
data AccountType
  = Saver
  | Transactional
  | HomeLoan
  deriving (Eq, Show, Generic, Enum)

instance ToJSON AccountType where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = fmap C.toUpper}

instance FromJSON AccountType where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = fmap C.toUpper}

-- | Possible ownership structure of an 'Account'.
data OwnershipType
  = Individual
  | Joint
  deriving (Eq, Show, Generic, Enum)

instance ToJSON OwnershipType where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = fmap C.toUpper}

instance FromJSON OwnershipType where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = fmap C.toUpper}
