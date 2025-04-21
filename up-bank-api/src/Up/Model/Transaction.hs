{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Up.Model.Transaction where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import qualified Data.Char as C
import qualified Data.Text as T
import GHC.Generics
import Up.Model.Category
import Up.Model.MoneyObject
import Up.Model.Tag

type TransactionId = T.Text

-- TODO: Relationships

-- | A (flattened) Up Transaction
data Transaction = Transaction
  { -- | The unique identifier for this transaction.
    transactionId :: TransactionId,
    -- | The current processing status of this transaction,
    --   according to whether or not this transaction has settled or is still held.
    transactionStatus :: TransactionStatus,
    -- | The original, unprocessed text of the transaction. This is often not a perfect
    --   indicator of the actual merchant, but it is useful for reconciliation purposes in some cases.
    transactionRawText :: Maybe T.Text,
    -- | A short description for this transaction. Usually the merchant name for purchases.
    transactionDescription :: T.Text,
    -- | Attached message for this transaction, such as a payment message, or a transfer note.
    transactionMessage :: Maybe T.Text,
    -- | If this transaction is currently in the HELD status, or was ever in the
    --   HELD status, the amount and foreignAmount of the transaction while HELD.
    transactionHoldInfo :: Maybe HoldInfo,
    -- | Details of how this transaction was rounded-up. If no Round Up was applied this field will be null.
    transactionRoundUp :: Maybe RoundUp,
    -- | If all or part of this transaction was instantly reimbursed in the form of cashback, details of the reimbursement.
    transactionCashback :: Maybe Cashback,
    -- | The amount of this transaction in Australian dollars. For transactions that were once
    --   HELD but are now SETTLED, refer to the holdInfo field for the original amount the transaction was HELD at.
    transactionAmount :: MoneyObject,
    -- | The foreign currency amount of this transaction. This field will be null for domestic transactions.
    --   The amount was converted to the AUD amount reflected in the amount of this transaction.
    --   Refer to the holdInfo field for the original foreignAmount the transaction was HELD at.
    transactionForeignAmount :: Maybe MoneyObject,
    -- | The date-time at which this transaction settled. This field will be null for
    --   transactions that are currently in the HELD status.
    transactionSettledAt :: Maybe T.Text,
    -- | The date-time at which this transaction was first encountered.
    transactionCreatedAt :: T.Text,
    transactionCategory :: Maybe CategoryId,
    transactionParentCategory :: Maybe CategoryId,
    transactionTags :: Maybe [TagId],
    -- | The canonical link to this resource within the API.
    transactionSelf :: Maybe T.Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Transaction where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON Transaction where
  parseJSON = withObject "transaction" $ \o ->
    let attributes = (o .: "attributes" >>=)
        links = (o .: "links" >>=)
        relationships = (o .: "relationships" >>=)
        category = relationships (.: "category") >>= (.:? "data") >>= mapM (.: "id")
        parentCategory = relationships (.: "parentCategory") >>= (.:? "data") >>= mapM (.: "id")
        tags = relationships (.: "tags") >>= (.:? "data") >>= mapM (mapM (.: "id"))
     in Transaction <$> o .: "id"
          <*> attributes (.: "status")
          <*> attributes (.: "rawText")
          <*> attributes (.: "description")
          <*> attributes (.: "message")
          <*> attributes (.: "holdInfo")
          <*> attributes (.: "roundUp")
          <*> attributes (.: "cashback")
          <*> attributes (.: "amount")
          <*> attributes (.: "foreignAmount")
          <*> attributes (.: "settledAt")
          <*> attributes (.: "createdAt")
          <*> category
          <*> parentCategory
          <*> tags
          <*> links (.: "self")

data TransactionStatus
  = Held
  | Settled
  deriving (Eq, Show, Generic, Enum)

instance ToJSON TransactionStatus where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = fmap C.toUpper}

instance FromJSON TransactionStatus where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = fmap C.toUpper}

data HoldInfo = HoldInfo
  { holdAmount :: MoneyObject,
    holdForeignAmount :: Maybe MoneyObject
  }
  deriving (Eq, Show, Generic)

instance ToJSON HoldInfo where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON HoldInfo where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

data RoundUp = RoundUp
  { roundupAmount :: MoneyObject,
    roundupBoostPortion :: Maybe MoneyObject
  }
  deriving (Eq, Show, Generic)

instance ToJSON RoundUp where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON RoundUp where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

data Cashback = Cashback
  { cashbackDescription :: T.Text,
    cashbackAmount :: MoneyObject
  }
  deriving (Eq, Show, Generic)

instance ToJSON Cashback where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON Cashback where
  parseJSON = genericParseJSON $ aesonPrefix camelCase
