{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Up where

import qualified Data.ByteString.Char8 as BSC
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Servant.Client
    ( defaultMakeClientRequest,
      ClientEnv(..),
      BaseUrl(BaseUrl),
      Scheme(Https) )
import Servant.Client.Core.Request (Request)
import Up.Model.Token (Token, tokenToString)
import qualified Network.HTTP.Types as Client

-- | Base URL of the Up Banking API
upBaseUrl :: BaseUrl
upBaseUrl = BaseUrl Https "api.up.com.au" 443 "/api/v1"

-- | Adds Authentication headers to the request
upClientRequest :: Token -> BaseUrl -> Request -> IO Client.Request
upClientRequest token url req = do
  baseReq <- defaultMakeClientRequest url req

  pure $ baseReq 
    { Client.requestHeaders = createAuthHeader token 
      : Client.requestHeaders baseReq
    }

createAuthHeader :: Token -> Client.Header
createAuthHeader token = ("Authorization", BSC.pack $ tokenToString token)

-- | Creates a ClientEnv configured for the Up Banking API
mkUpClient :: Token -> IO ClientEnv
mkUpClient token = do
  mgr <- Client.newManager tlsManagerSettings
  pure $ ClientEnv
    { manager = mgr
    , baseUrl = upBaseUrl
    , makeClientRequest = upClientRequest token
    , cookieJar = Nothing
    , middleware = id
    }