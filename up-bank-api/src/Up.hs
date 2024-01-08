{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Up where

import qualified Data.ByteString.Char8 as BSC
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS
import Servant.Client
import Servant.Client.Core.Request (Request)
import Up.Model.Token (Token, tokenToString)

-- | Base URL of the Up Banking API
upBaseUrl :: BaseUrl
upBaseUrl = BaseUrl Https "api.up.com.au" 443 "/api/v1"

-- | Adds the Authorization header to every transaction
makeUpClientRequest :: Token -> BaseUrl -> Request -> IO Client.Request
makeUpClientRequest token base = fmap addHeader . defaultMakeClientRequest base
  where
    addHeader req = req {Client.requestHeaders = ("Authorization", BSC.pack $ tokenToString token) : Client.requestHeaders req}

mkUpClient :: Token -> IO ClientEnv
mkUpClient t = do
  mgr <- Client.newManager tlsManagerSettings
  pure (mkClientEnv mgr upBaseUrl) {makeClientRequest = makeUpClientRequest t}