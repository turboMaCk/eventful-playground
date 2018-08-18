{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CounterState
  ( Client
  , ServerState
  , subscribeToState
  , initialState
  , current
  , getEvents
  , subscribeToEvents
  , handleEvent
  )
  where

import Control.Concurrent (MVar)
import Counter (CounterStream, Counter)
import Control.Monad (forM_, forever)
import Counter (CounterEvent)
import Control.Exception (finally)
import Eventful (VersionedStreamEvent)
import qualified Counter
import qualified Control.Concurrent as Concurrent
import qualified Network.WebSockets as WS
import qualified Data.Aeson as Aeson


type Client = (Int, WS.Connection)


data ServerState = ServerState
  { counterClients :: MVar [Client]
  , eventClients :: MVar [Client]
  , counter :: CounterStream
  }


initialState :: IO ServerState
initialState = do
  counter' <- Counter.constructStream
  cs <- Concurrent.newMVar []
  es <- Concurrent.newMVar []
  pure (ServerState cs es counter')


current :: ServerState -> IO Counter
current server =
  Counter.getCurrentState $ counter server


addClient :: WS.Connection -> [Client] -> (Int, [Client])
addClient conn [] = (0, [(0, conn)])
addClient conn xs@((prevId,_):_) = (newId, (newId, conn) : xs)
  where newId = prevId + 1


removeClient :: Int -> [Client] -> [Client]
removeClient connId = filter ((/= connId) . fst)


subscribeToState :: ServerState -> WS.Connection -> IO ()
subscribeToState state conn = do
  counterState <- current state
  cs <- Concurrent.readMVar $ counterClients state
  let (connId, newClients) = addClient conn cs

  -- update MVar
  Concurrent.modifyMVar_ (counterClients state) $ const $ pure newClients

  -- Disconnect user at the end of session
  flip finally (disconnect connId) $ do
        WS.forkPingThread conn 30
        WS.sendTextData conn (Aeson.encode counterState)
        handle conn state

  where
    disconnect connId =
      Concurrent.modifyMVar (counterClients state) $ \s ->
                let s' = removeClient connId s
                in pure (s', s')


subscribeToEvents :: ServerState -> WS.Connection -> IO ()
subscribeToEvents state conn = do
  events <- getEvents state
  es <- Concurrent.readMVar $ eventClients state
  let (connId, newClients) = addClient conn es

  -- update MVar
  Concurrent.modifyMVar_ (eventClients state) $ const $ pure newClients

  -- Disconnect user at the end of session
  flip finally (disconnect connId) $ do
        WS.sendTextData conn (Aeson.encode events)
        WS.forkPingThread conn 30

  where
    disconnect connId =
      Concurrent.modifyMVar (eventClients state) $ \s ->
                let s' = removeClient connId s
                in pure (s', s')


broadcastState :: ServerState -> IO ()
broadcastState state = do
  cs <- Concurrent.readMVar $ counterClients state
  counter' <- current state
  forM_ cs $
      \(_, conn) -> do
        WS.sendTextData conn $ Aeson.encode counter'


broadcastEvent :: CounterEvent -> ServerState -> IO ()
broadcastEvent event state = do
  es <- Concurrent.readMVar $ eventClients state
  forM_ es $
    \(_, conn) -> do
      WS.sendTextData conn $ Aeson.encode event


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
  broadcastState state
  broadcastEvent event state
  current state


getEvents :: ServerState -> IO [VersionedStreamEvent CounterEvent]
getEvents (ServerState { counter }) = do
  e' <- Counter.allEvents counter
  print e'
  pure e'
