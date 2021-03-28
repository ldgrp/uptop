{-# LANGUAGE OverloadedStrings #-}

module Common where

import Control.Monad
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Format
import Data.Time.LocalTime
import Up.Model.MoneyObject

showMoneyObject :: MoneyObject -> T.Text
showMoneyObject mo = T.intercalate " " [value mo, moCurrencyCode mo]
  where
    value x
      | T.isPrefixOf "-" (moValue x) = moValue x
      | otherwise = T.cons '+' (moValue x)

fmtTime :: String -> Maybe String
fmtTime = fmap (formatTime defaultTimeLocale "%d %b %R") . utcTime
  where
    utcTime :: String -> Maybe LocalTime
    utcTime = parseTimeM True defaultTimeLocale "%FT%T%Ez"

lpad :: Int -> T.Text -> String
lpad i t = T.unpack $ T.justifyRight i ' ' t

rpad :: Int -> T.Text -> String
rpad i t = T.unpack $ T.justifyLeft i ' ' t

lpad' :: Int -> Maybe T.Text -> String
lpad' i t = lpad i t'
  where
    t' = fromMaybe "" t :: T.Text

rpad' :: Int -> Maybe T.Text -> String
rpad' i t = rpad i t'
  where
    t' = fromMaybe "" t :: T.Text

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return
