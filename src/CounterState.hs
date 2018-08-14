{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module CounterState
  ( Client
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
import Control.Exception (finally)
import qualified Counter
import qualified Control.Concurrent as Concurrent
import qualified Network.WebSockets as WS
import qualified Data.Aeson as Aeson


type Client = (Int, WS.Connection)


data ServerState = ServerState
  { clients :: MVar [Client]
  , counter :: CounterStream
  }


initialState :: IO ServerState
initialState = do
  counter' <- Counter.constructStream
  cs <- Concurrent.newMVar []
  pure (ServerState cs counter')


current :: ServerState -> IO Counter
current server =
  Counter.getCurrentState $ counter server


addClient :: WS.Connection -> [Client] -> (Int, [Client])
addClient conn [] = (0, [(0, conn)])
addClient conn xs@((prevId,_):_) = (newId, (newId, conn) : xs)
  where newId = prevId + 1


removeClient :: Int -> [Client] -> [Client]
removeClient connId = filter ((/= connId) . fst)


newConnection :: ServerState -> WS.Connection -> IO ()
newConnection state conn = do
  counterState <- current state
  cs <- Concurrent.readMVar $ clients state
  let (connId, newClients) = addClient conn cs

  -- update MVar
  Concurrent.modifyMVar_ (clients state) $ const $ pure newClients

  -- Disconnect user at the end of session
  flip finally (disconnect connId) $ do
        WS.forkPingThread conn 30
        WS.sendTextData conn (Aeson.encode counterState)
        handle conn state

  where
    disconnect connId =
      Concurrent.modifyMVar (clients state) $ \s ->
                let s' = removeClient connId s
                in pure (s', s')


broadcast :: ServerState -> IO ()
broadcast state = do
    cs <- Concurrent.readMVar $ clients state
    counter' <- current state
    forM_ cs $
      \(_, conn) -> do
        WS.sendTextData conn $ Aeson.encode counter'


handle :: WS.Connection -> ServerState -> IO ()
handle conn state = forever $ do
  msg <- WS.receiveData conn
  case Aeson.decode msg of
    Just event -> handleEvent state event
    -- @TODO: handle decoding error here
    Nothing -> current state


handleEvent :: ServerState -> CounterEvent -> IO Counter
handleEvent state event = do
  let stream = counter state
  Counter.handleEvent stream event
  broadcast state
  current state
