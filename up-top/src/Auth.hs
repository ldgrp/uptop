{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Auth (AuthInfo(..), AuthEvent(..), interactiveAuth) where

import Brick
import Brick.BChan
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Control.Concurrent
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Graphics.Vty
import Lens.Micro.Platform

import Servant.Client

import Up
import Up.API
import Up.Model.Token

import qualified Data.Text as T

data Name = TokenField
  deriving  (Eq, Ord, Show)

data AuthEvent
  = AConnect
  | ATimeout
  | ANotAuthorized
  | AInvalid
  | ASuccess AuthInfo
  deriving (Eq, Show)

data AuthState = Idle | Connecting
  deriving  (Eq, Show)

newtype AuthRequest = DoPing AuthInfo

newtype AuthInfo = AuthInfo { _token :: T.Text }
  deriving  (Eq, Show)
makeLenses ''AuthInfo

data State = State
  { _form :: Form AuthInfo AuthEvent Name
  , _currentState :: AuthState
  , _lastAttempt :: Maybe AuthEvent
  , _reqChan :: BChan AuthRequest
  }
makeLenses ''State

-- App definition
app :: App State AuthEvent Name
app = App
  { appDraw = drawAuth
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = pure ()
  , appAttrMap = const theMap
  }

-- Event handler
handleEvent :: BrickEvent Name AuthEvent -> EventM Name State ()
handleEvent (VtyEvent (EvKey KEsc [])) = halt
handleEvent (VtyEvent (EvKey KEnter [])) = do
  st <- get
  case st ^. currentState of
    Connecting ->
      -- If we are already connecting, do nothing.
      pure ()
    Idle ->
      -- Ping the Up server with the AuthInfo
      let aInfo = formState (st ^. form)
       in liftIO $ writeBChan (st ^. reqChan) $ DoPing aInfo

handleEvent (AppEvent AConnect) = do
  currentState .= Connecting
  lastAttempt ?= AConnect
handleEvent (AppEvent ATimeout) = do
  currentState .= Idle
  lastAttempt ?= ATimeout
handleEvent (AppEvent ANotAuthorized) = do
  currentState .= Idle
  lastAttempt ?= ANotAuthorized
handleEvent (AppEvent AInvalid) = do
  currentState .= Idle
  lastAttempt ?= AInvalid
handleEvent (AppEvent (ASuccess a)) = do
  currentState .= Idle
  lastAttempt ?= ASuccess a
  halt

handleEvent e = zoom form $ handleFormEvent e
theMap :: AttrMap
theMap = attrMap defAttr
  [ (focusedFormInputAttr, black `on` yellow)
  , (invalidFormInputAttr, white `on` red)
  , (editAttr, black `on` yellow)
  ]

-- | A thread that pings the Up Bank API.
authWorker :: BChan AuthRequest -> BChan AuthEvent -> IO ()
authWorker requestChan responseChan = forever $ do
  req <- readBChan requestChan
  case req of
    DoPing aInfo -> do
      let tok = aInfo ^. token

      if T.isPrefixOf "up:yeah:" tok
        then do
          writeBChan responseChan AConnect

          env <- mkUpClient $ Token (T.unpack tok)
          res <- runClientM ping env

          case res of
            Right _ping -> writeBChan responseChan $ ASuccess aInfo
            Left _err -> writeBChan responseChan ANotAuthorized
        else do
          writeBChan responseChan AInvalid

emptyAuthInfo :: AuthInfo
emptyAuthInfo = AuthInfo { _token = "" }

initialState :: AuthInfo -> BChan AuthRequest -> State
initialState aInfo chan = State
  { _form = form'
  , _currentState = Idle
  , _lastAttempt = Nothing
  , _reqChan = chan
  }
  where form' = mkForm aInfo

interactiveAuth :: Vty
                -> IO Vty
                -> Maybe String
                -> IO (Maybe AuthEvent, Vty)
interactiveAuth vty buildVty tok = do
  requestChan <- newBChan 10
  responseChan <- newBChan 10
  void $ forkIO $ authWorker requestChan responseChan

  authInfo <- case tok of
        Just t -> do
          let aInfo = AuthInfo (T.pack t)
          liftIO $ writeBChan requestChan $ DoPing aInfo
          pure aInfo
        Nothing -> pure emptyAuthInfo
  (st', vty') <- customMainWithVty vty buildVty (Just responseChan) app (initialState authInfo requestChan)

  pure (st' ^. lastAttempt, vty')

drawAuth :: State -> [Widget Name]
drawAuth st =
  [ center $ hLimit 70 $ vLimit 18 $ border $
    padLeft (Pad 2) $ padRight (Pad 2) $
    vBox [ center $ str "Enter your Up Bank Personal Access Token."
         , vLimit 2 $ fill ' '
         , renderForm (st ^. form)
         , vLimit 2 $ fill ' '
         , center $ str "See https://api.up.com.au/getting_started for more info."
         , vLimit 2 $ fill ' '
         , center $ str $ status (st ^. lastAttempt)
         , vLimit 1 $ fill ' '
         , center $ str "Press ESC to exit"
         ]
  ]
  where
    status :: Maybe AuthEvent -> String
    status Nothing = " "
    status (Just ae) = case ae of
      AConnect -> "Connecting..."
      ANotAuthorized -> "Authentication failed."
      ASuccess _i -> "Succesful"
      ATimeout -> "Timed out."
      AInvalid -> "Invalid token"

mkForm :: AuthInfo -> Form AuthInfo AuthEvent Name
mkForm = newForm [(str "Token: " <+>) @@= editTextField token TokenField (Just 1)]
