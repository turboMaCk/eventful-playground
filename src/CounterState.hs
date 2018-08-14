{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CounterState
  (Client
  , ServerState
  , newConnection
  , broadcast
  , initialState
  )
  where

import Control.Concurrent (MVar)
import Counter (CounterStream)
import Control.Monad (forM_, forever)
import Counter (CounterEvent)
import qualified Counter
import qualified Control.Concurrent as Concurrent
import qualified Network.WebSockets as WS
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson


type Client = WS.Connection

data ServerState = ServerState
  { clients :: [Client]
  , counter :: CounterStream
  }


initialState :: CounterStream -> ServerState
initialState = ServerState []


addClient :: Client -> ServerState -> ServerState
addClient client state = state { clients = client : clients state }


newConnection :: CounterStream -> MVar ServerState -> WS.Connection -> IO ()
newConnection counterStream state conn = do
  Concurrent.modifyMVar_ state $ \s -> do
        counterState <- Counter.getCurrentState counterStream
        WS.sendTextData conn (Text.pack $ show counterState)
        pure $ addClient conn s
  WS.forkPingThread conn 30
  handle conn state


broadcast :: CounterEvent -> MVar ServerState -> IO ()
broadcast event state = do
    unpacked <- Concurrent.readMVar state
    counterState <- Counter.getCurrentState $ counter unpacked
    forM_ (clients unpacked) $
      \conn -> WS.sendTextData conn $ Text.pack ""


handle :: Client -> MVar ServerState -> IO ()
handle conn state = forever $ do
  (msg :: Text.Text) <- WS.receiveData conn
  -- Concurrent.readMVar state >>= broadcast
  pure ()
