{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Up where

import Up.Model.Token

import Servant.Client
import Servant.Client.Core.Request (Request)

import qualified Network.HTTP.Client as Client
import qualified Data.ByteString.Char8 as BSC

import Network.HTTP.Client.TLS (tlsManagerSettings)

-- | Base URL of the Up Banking API
upBaseUrl :: BaseUrl
upBaseUrl = BaseUrl Https "api.up.com.au" 443 "/api/v1"

-- | Adds the Authorization header to every transaction
makeUpClientRequest :: Token -> BaseUrl -> Request -> Client.Request
makeUpClientRequest token = (addHeader .) . defaultMakeClientRequest
    where 
          addHeader req = req {Client.requestHeaders = ("Authorization", BSC.pack $ tokenToString token) : Client.requestHeaders req}

mkUpClient :: Token -> IO ClientEnv
mkUpClient t = do
  mgr <- Client.newManager tlsManagerSettings
  pure (mkClientEnv mgr upBaseUrl) { makeClientRequest = makeUpClientRequest t }
 