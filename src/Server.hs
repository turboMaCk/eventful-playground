{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (start) where

import Servant
import Network.Wai.Handler.Warp (run)
import Counter (Counter, CounterEvent, CounterStream)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forM_, forever)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Servant.API.WebSocket (WebSocket)
import Control.Concurrent (MVar)
import qualified Data.Text as Text
import qualified Network.WebSockets as WS
import qualified Control.Concurrent as Concurrent
import qualified Counter

-- State

type Client = WS.Connection
type ServerState = [Client]

initialState :: ServerState
initialState = []

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

broadcast :: String -> MVar ServerState -> IO ()
broadcast message state = do
    clients <- Concurrent.readMVar state
    putStrLn message
    forM_ clients $ \conn -> WS.sendTextData conn (Text.pack message)

-- Api

type CounterApi = "counter" :> ReqBody '[JSON] CounterEvent :> Post '[JSON] Counter
             :<|> "counter" :> Get '[JSON] Counter
             :<|> "stream" :> WebSocket


counterApi :: Proxy CounterApi
counterApi = Proxy


counterApp :: CounterStream -> MVar ServerState -> Application
counterApp counterStream state =
  cors (const $ Just policy)
  $ serve counterApi
  $ server counterStream state
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ] }


server ::  CounterStream -> MVar ServerState -> Server CounterApi
server counterStream serverState = record :<|> current :<|> streamData
  where
    record :: CounterEvent -> Handler Counter
    record event = liftIO $ do
      _ <- Counter.handleEvent counterStream event
      state <- Counter.getCurrentState counterStream
      _ <- broadcast (show state) serverState

      pure state

    current :: Handler Counter
    current = do
      state <- liftIO $ Counter.getCurrentState counterStream
      pure state

    streamData :: MonadIO m => WS.Connection -> m ()
    streamData c = liftIO $ do
      Concurrent.modifyMVar_ serverState $ \s -> do
        state <- Counter.getCurrentState counterStream
        WS.sendTextData c (Text.pack $ show state)
        pure $ addClient c s
      forever $ WS.forkPingThread c 30 >> Concurrent.threadDelay 100000

    -- streamData :: MonadIO m => WS.Connection -> m ()
    -- streamData c = liftIO . forever $ do
    --   WS.forkPingThread c 10
    --   state <- Counter.getCurrentState counterStream
      -- WS.sendTextData c (Text.pack $ show state) >> Concurrent.threadDelay 1000000


start :: IO ()
start = do
  counterStream <- Counter.constructStream
  state <- Concurrent.newMVar initialState
  run 8081 $ counterApp counterStream state
