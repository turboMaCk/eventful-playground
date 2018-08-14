{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CounterState
  (Client
  , ServerState
  , newConnection
  , broadcast
  , initialState
  , current
  , handleEvent
  )
  where

import Control.Concurrent (MVar)
import Counter (CounterStream, Counter)
import Control.Monad (forM_, forever)
import Counter (CounterEvent)
import qualified Counter
import qualified Control.Concurrent as Concurrent
import qualified Network.WebSockets as WS
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson


type Client = WS.Connection

data ServerState = ServerState
  { clients :: MVar [Client]
  , counter :: CounterStream
  }


initialState :: IO ServerState
initialState = do
  counter <- Counter.constructStream
  clients <- Concurrent.newMVar []
  pure (ServerState clients counter)


current :: ServerState -> IO Counter
current server =
  Counter.getCurrentState $ counter server


addClient :: Client -> [Client] -> [Client]
addClient client clients = client : clients


newConnection :: ServerState -> WS.Connection -> IO ()
newConnection state conn = do
  counterState <- current state
  Concurrent.modifyMVar_ (clients state) $ \clients -> do
        WS.sendTextData conn (Aeson.encode counterState)
        pure $ addClient conn clients
  WS.forkPingThread conn 30
  handle conn state


broadcast :: ServerState -> IO ()
broadcast state = do
    clients <- Concurrent.readMVar $ clients state
    -- counterState <- Counter.getCurrentState $ counter state
    forM_ clients $
      \conn -> do
        nState <- current state
        WS.sendTextData conn $ Aeson.encode nState


handle :: Client -> ServerState -> IO ()
handle conn state = forever $ do
  (msg :: Text.Text) <- WS.receiveData conn
  broadcast state


handleEvent :: CounterEvent -> ServerState -> IO Counter
handleEvent event state = do
  let stream = counter state
  Counter.handleEvent stream event
  broadcast state
  current state
