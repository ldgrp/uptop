{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Up.API ( 
    -- * Accounts API
      listAccounts
    , retrieveAccount
    -- * Tags API
    , listTags
    , addTags
    , removeTags
    -- * Transactions API
    , listTransactions
    , listTransactionsByAccount
    ) where

import Data.Proxy
import Servant.API
import Servant.Client
import Up.Model.Account
import Up.Model.Paginated
import Up.Model.Tag
import Up.Model.Transaction

import qualified Data.Text as T


-- * Accounts API
type AccountsAPI = 
         "accounts" :> QueryParam "page[size]" Int :> Get '[JSON] (Paginated Account)
    :<|> "accounts" :> Capture "id" T.Text         :> Get '[JSON] Account

-- | Retrieve a paginated list of all accounts for the currently authenticated user. 
--   The returned list is paginated and can be scrolled by following the prev and next links where present.
listAccounts :: Maybe Int
             -> ClientM (Paginated Account)

-- | Retrieve a specific account by providing its unique identifier.
retrieveAccount :: T.Text 
                -> ClientM Account

listAccounts :<|> retrieveAccount = client (Proxy :: Proxy AccountsAPI)


-- * Tags API
type TagsAPI =
         "tags"         :> QueryParam "page[size]" Int :> Get '[JSON] (Paginated Tag)
    :<|> "transactions" :> Capture "transactionId" T.Text :> "relationships" 
              :> "tags" :> ReqBody '[JSON] TagInput :> Post '[JSON] ()
    :<|> "transactions" :> Capture "transactionId" T.Text :> "relationships" 
              :> "tags" :> ReqBody '[JSON] TagInput :> Delete '[JSON] ()

-- | Retrieve a list of all tags currently in use. The returned list is paginated and can be 
--   scrolled by following the next and prev links where present. Results are ordered 
--   lexicographically. The transactions relationship for each tag exposes a link to 
--   get the transactions with the given tag.
listTags :: Maybe Int
         -> ClientM (Paginated Tag)

-- | Associates one or more tags with a specific transaction. No more than 6 tags may be present 
--   on any single transaction. Duplicate tags are silently ignored. An HTTP 204 is returned on 
--   success. The associated tags, along with this request URL, are also exposed via the tags 
--   relationship on the transaction resource returned from /transactions/{id}.
addTags :: T.Text
        -> TagInput
        -> ClientM ()

-- | Delete a specific webhook by providing its unique identifier. 
--   Once deleted, webhook events will no longer be sent to the configured URL.
removeTags :: T.Text
           -> TagInput
           -> ClientM ()

listTags :<|> addTags :<|> removeTags = client (Proxy :: Proxy TagsAPI)


-- * Transactions API
type TransactionsAPI =
         "transactions" :> QueryParam "page[size]" Int       :> QueryParam "filter[since]" T.Text
                        :> QueryParam "filter[until]" T.Text :> QueryParam "filter[tag]" T.Text 
                        :> Get '[JSON] (Paginated Transaction)
    :<|> "transactions" :> Capture "transactionId" T.Text 
                        :> Get '[JSON] (Transaction)
    :<|> "accounts" :> Capture "accountID" T.Text :> "transactions" 
                        :> QueryParam "page[size]" Int       :> QueryParam "filter[since]" T.Text
                        :> QueryParam "filter[until]" T.Text :> QueryParam "filter[tag]" T.Text 
                        :> Get '[JSON] (Paginated Transaction)

-- | Retrieve a list of all transactions across all accounts for the currently authenticated user. 
--   The returned list is paginated and can be scrolled by following the next and prev links where 
--   present. To narrow the results to a specific date range pass one or both of filter[since] 
--   and filter[until] in the query string. These filter parameters should not be used for 
--   pagination. Results are ordered newest first to oldest last.
listTransactions :: Maybe Int
                 -> Maybe T.Text
                 -> Maybe T.Text
                 -> Maybe T.Text
                 -> ClientM (Paginated Transaction)

-- | Retrieve a specific transaction by providing its unique identifier.
retrieveTransaction :: T.Text
                    -> ClientM (Transaction)

-- | Retrieve a list of all transactions for a specific account. The returned list is paginated 
--   and can be scrolled by following the next and prev links where present. To narrow the results 
--   to a specific date range pass one or both of filter[since] and filter[until] in the query 
--   string. These filter parameters should not be used for pagination. 
--   Results are ordered newest first to oldest last.
listTransactionsByAccount :: T.Text
                          -> Maybe Int
                          -> Maybe T.Text
                          -> Maybe T.Text
                          -> Maybe T.Text
                          -> ClientM (Paginated Transaction)

listTransactions :<|> retrieveTransaction :<|> listTransactionsByAccount = client (Proxy :: Proxy TransactionsAPI)
