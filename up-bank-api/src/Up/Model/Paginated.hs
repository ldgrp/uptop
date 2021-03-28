{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Up.Model.Paginated where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics
import Network.HTTP.Types.URI

-- | A (flattened) response of a paginated endpoint
data Paginated a = Paginated
  { -- | An array of resources specific to the endpoint
    paginatedData :: [a],
    -- | A cursor to move to the previous page
    paginatedPrev :: Maybe T.Text,
    -- | A cursor to move to the next page
    paginatedNext :: Maybe T.Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Paginated a) where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON a => FromJSON (Paginated a) where
  parseJSON = withObject "paginated" $ \o -> do
    pData <- o .: "data"
    pLinks <- o .: "links"
    pPrevUrl <- pLinks .:? "prev"
    pNextUrl <- pLinks .:? "next"
    let pPrev = maybe Nothing (parseCursor "page[before]") pPrevUrl
        pNext = maybe Nothing (parseCursor "page[after]") pNextUrl
    return $ Paginated pData pPrev pNext

-- | Given a url, return the cursor
parseCursor :: T.Text -> T.Text -> Maybe T.Text
parseCursor target url = do
  cursor <- findTarget bsTarget $ snd $ decodePath $ extractPath $ urlDecode False $ TE.encodeUtf8 url
  return $ TE.decodeUtf8 cursor
  where
    bsTarget = urlDecode True $ TE.encodeUtf8 target

    findTarget :: B.ByteString -> [(B.ByteString, Maybe B.ByteString)] -> Maybe B.ByteString
    findTarget t xs =
      case filter ((t ==) . fst) xs of
        x : _xs -> snd x
        [] -> Nothing
