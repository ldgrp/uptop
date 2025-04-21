{-# LANGUAGE DataKinds #-}

{-# LANGUAGE TypeOperators #-}

module Up.API
  ( -- * Accounts API
    listAccounts,
    listAccounts_,
    retrieveAccount,

    -- * Categories API
    listCategories,
    retrieveCategory,

    -- * Tags API
    listTags,
    listTags_,
    addTags,
    removeTags,

    -- * Transactions API
    listTransactions,
    listTransactions_,
    retrieveTransaction,
    listTransactionsByAccount,
    listTransactionsByAccount_,

    -- * Utility API
    ping,
    unpaginate,
  )
where

import Control.Arrow (second)
import Data.Proxy
import qualified Data.Text as T
import Servant.API
import Servant.Client
import Up.Model.Account
import Up.Model.Category
import Up.Model.Paginated
import Up.Model.Tag
import Up.Model.Transaction
import Up.Model.Utility

-- * Accounts API
type AccountsAPI =
       "accounts" :> QueryParam "page[size]" Int
                  :> QueryParam "page[before]" AccountId
                  :> QueryParam "page[after]" AccountId
                  :> Get '[JSON] (Paginated Account)
  :<|> "accounts" :> Capture "id" T.Text
                  :> Get '[JSON] Account

-- | Retrieve a paginated list of all accounts for the currently authenticated user.
listAccounts :: Maybe Int         -- ^ page[size]
             -> Maybe AccountId   -- ^ page[before]
             -> Maybe AccountId   -- ^ page[after]
             -> ClientM (Paginated Account)

-- | Retrieve an unfolded list of all accounts for the currently authenticated user.
listAccounts_ :: Maybe Int         -- ^ page[size]
              -> ClientM [Account]
listAccounts_ size = concat <$> unpaginate f g
  where
    f = listAccounts size Nothing Nothing
    g = listAccounts size Nothing

-- | Retrieve a specific account by providing its unique identifier.
retrieveAccount :: T.Text
                -> ClientM Account

listAccounts :<|> retrieveAccount = client (Proxy :: Proxy AccountsAPI)


-- * Categories API
type CategoriesAPI =
  -- listCategories
       "categories" :> QueryParam "filter[parent]" CategoryId
                    :> Get '[JSON] Categories
  -- retrieveCategory
  :<|> "categories" :> Capture "id" T.Text
                    :> Get '[JSON] Category

-- | Retrieve a list of all categories and their ancestry.
listCategories :: Maybe CategoryId
               -> ClientM Categories

-- | Retrieve a specific category by providing its unique identifier.
retrieveCategory :: T.Text
                 -> ClientM Category

listCategories :<|> retrieveCategory = client (Proxy :: Proxy CategoriesAPI)

-- * Tags API
type TagsAPI =
       "tags"         :> QueryParam "page[size]" Int
                      :> QueryParam "page[before]" AccountId
                      :> QueryParam "page[after]" AccountId
                      :> Get '[JSON] (Paginated Tag)
  :<|> "transactions" :> Capture "transactionId" T.Text
                      :> "relationships"
                      :> "tags"
                      :> ReqBody '[JSON] TagInput
                      :> Post '[JSON] ()
  :<|> "transactions" :> Capture "transactionId" T.Text
                      :> "relationships"
                      :> "tags"
                      :> ReqBody '[JSON] TagInput
                      :> Delete '[JSON] ()

-- | Retrieve a paginated list of all tags currently in use.
listTags :: Maybe Int     -- ^ page[size]
         -> Maybe TagId   -- ^ page[before]
         -> Maybe TagId   -- ^ page[after]
         -> ClientM (Paginated Tag)

-- | Retrieve an unfolded list of all tags currently in use.
listTags_ :: Maybe Int     -- ^ page[size]
          -> ClientM [Tag]
listTags_ size = concat <$> unpaginate f g
  where f = listTags size Nothing Nothing
        g = listTags size Nothing

-- | Associates one or more tags with a specific transaction. No more than 6 tags
--   may be present on any single transaction. Duplicate tags are silently ignored.
addTags :: T.Text
        -> TagInput
        -> ClientM ()

-- | Delete a specific webhook by providing its unique identifier.
--   Once deleted, webhook events will no longer be sent to the configured URL.
removeTags :: T.Text
           -> TagInput
           -> ClientM ()

listTags
  :<|> addTags
  :<|> removeTags =
    client (Proxy :: Proxy TagsAPI)


-- * Transactions API
type TransactionsAPI =
  -- listTransactions
       "transactions" :> QueryParam "page[size]" Int
                      :> QueryParam "page[before]" TransactionId
                      :> QueryParam "page[after]" TransactionId
                      :> QueryParam "filter[since]" T.Text
                      :> QueryParam "filter[until]" T.Text
                      :> QueryParam "filter[tag]" T.Text
                      :> Get '[JSON] (Paginated Transaction)
  -- retrieveTransaction
  :<|> "transactions" :> Capture "transactionId" TransactionId
                      :> Get '[JSON] Transaction
  -- listTransactionsByAccount
  :<|> "accounts"     :> Capture "accountID" AccountId
                      :> "transactions"
                      :> QueryParam "page[size]" Int
                      :> QueryParam "page[before]" TransactionId
                      :> QueryParam "page[after]" TransactionId
                      :> QueryParam "filter[since]" T.Text
                      :> QueryParam "filter[until]" T.Text
                      :> QueryParam "filter[tag]" T.Text
                      :> Get '[JSON] (Paginated Transaction)

-- | Retrieve a paginated list of all transactions across all accounts for the currently authenticated user.
listTransactions :: Maybe Int                -- ^ page[size]
                 -> Maybe TransactionId      -- ^ page[before]
                 -> Maybe TransactionId      -- ^ page[after]
                 -> Maybe T.Text             -- ^ filter[since]
                 -> Maybe T.Text             -- ^ filter[until]
                 -> Maybe T.Text             -- ^ filter[tag]
                 -> ClientM (Paginated Transaction)

-- | Retrieve an unfolded list of all transactions across all accounts for the currently authenticated user.
listTransactions_ :: Maybe Int                -- ^ page[size]
                  -> Maybe T.Text             -- ^ filter[since]
                  -> Maybe T.Text             -- ^ filter[until]
                  -> Maybe T.Text             -- ^ filter[tag]
                  -> ClientM [Transaction]
listTransactions_ size fsince funtil ftag = concat <$> unpaginate f g
  where
    f = listTransactions size Nothing Nothing fsince funtil ftag
    g after = listTransactions size Nothing after Nothing Nothing Nothing

-- | Retrieve a specific transaction by providing its unique identifier.
retrieveTransaction :: TransactionId         -- ^ transactionId
                    -> ClientM Transaction

-- | Retrieve a list of all transactions for a specific account.
listTransactionsByAccount :: AccountId       -- ^ accountId
                          -> Maybe Int       -- ^ page[size]
                          -> Maybe AccountId -- ^ page[before]
                          -> Maybe AccountId -- ^ page[after]
                          -> Maybe T.Text    -- ^ filter[since]
                          -> Maybe T.Text    -- ^ filter[until]
                          -> Maybe T.Text    -- ^ filter[tag]
                          -> ClientM (Paginated Transaction)

-- | Retrieve an unfolded list of all transactions for a specific account.
listTransactionsByAccount_ :: AccountId      -- ^ accountId
                           -> Maybe Int      -- ^ page[size]
                           -> Maybe T.Text   -- ^ filter[since]
                           -> Maybe T.Text   -- ^ filter[until]
                           -> Maybe T.Text   -- ^ filter[tag]
                           -> ClientM [Transaction]
listTransactionsByAccount_ aid size fsince funtil ftag = concat <$> unpaginate f g
  where
    f = listTransactionsByAccount aid size Nothing Nothing fsince funtil ftag
    g after = listTransactionsByAccount aid size Nothing after Nothing Nothing Nothing
listTransactions
  :<|> retrieveTransaction
  :<|> listTransactionsByAccount =
    client (Proxy :: Proxy TransactionsAPI)

-- * Utlity API
type UtilityAPI =
  --  Ping
  "util" :> "ping" :> Get '[JSON] Ping

-- | Make a basic ping request to the API.
-- | This is useful to verify that authentication is functioning correctly.
ping :: ClientM Ping
ping = client (Proxy :: Proxy UtilityAPI)

data Cursor a = Start | Next a | End deriving (Show)

maybeToCursor :: Maybe a -> Cursor a
maybeToCursor (Just x) = Next x
maybeToCursor Nothing = End

unfoldrM :: Show b => Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f b = do
  r <- f b
  case r of
    Just (a, b') -> fmap (a :) (unfoldrM f b')
    Nothing -> return []

wrapped ::
  Applicative m =>
  m (Maybe (a, Maybe b)) ->
  (Maybe b -> m (Maybe (a, Maybe b))) ->
  Cursor b ->
  m (Maybe (a, Cursor b))
wrapped f _g Start = fmap (second maybeToCursor) <$> f
wrapped _f g (Next b) = fmap (second maybeToCursor) <$> g (Just b)
wrapped _f _g End = pure Nothing

unpaginate ::
  Monad m =>
  m (Paginated a) ->
  (Maybe T.Text -> m (Paginated a)) ->
  m [[a]]
unpaginate f g = unfoldrM (wrapped f' g') Start
  where
    f' = f >>= \p -> return $ Just (paginatedData p, paginatedNext p)
    g' b = g b >>= \p -> return $ Just (paginatedData p, paginatedNext p)
