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
import Lens.Micro
import Lens.Micro.TH

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
  , appStartEvent = return
  , appAttrMap = const theMap
  }

-- Event handler
handleEvent :: State -> BrickEvent Name AuthEvent -> EventM Name (Next State)
handleEvent st (VtyEvent (EvKey KEsc [])) = halt st
handleEvent st (VtyEvent (EvKey KEnter [])) = do
  case st ^. currentState of
    Connecting -> 
      -- If we are already connecting, do nothing.
      pure ()
    Idle ->
      -- Ping the Up server with the AuthInfo
      let aInfo = formState (st ^. form)
       in liftIO $ writeBChan (st ^. reqChan) $ DoPing aInfo
      
  continue st

handleEvent st (AppEvent AConnect) = 
  continue (st & currentState .~ Connecting 
               & lastAttempt ?~ AConnect)
handleEvent st (AppEvent ATimeout) = 
  continue (st & currentState .~ Idle 
               & lastAttempt ?~ ATimeout)
handleEvent st (AppEvent ANotAuthorized) = 
  continue (st & currentState .~ Idle 
               & lastAttempt ?~ ANotAuthorized)
handleEvent st (AppEvent AInvalid) = 
  continue (st & currentState .~ Idle 
               & lastAttempt ?~ AInvalid)
handleEvent st (AppEvent (ASuccess a)) = 
  halt (st & currentState .~ Idle 
               & lastAttempt ?~ ASuccess a)

handleEvent st e = do 
  f' <- handleFormEvent e (st ^. form)
  continue $ st & form .~ f'

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
