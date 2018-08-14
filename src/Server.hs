{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (start) where

import Servant
import Network.Wai.Handler.Warp (run)
import Counter (Counter, CounterEvent, CounterStream)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Servant.API.WebSocket (WebSocket)
import qualified CounterState as CS
import qualified Network.WebSockets as WS
import qualified Counter

-- Api

type CounterApi = "counter" :> ReqBody '[JSON] CounterEvent :> Post '[JSON] Counter
             :<|> "counter" :> Get '[JSON] Counter
             :<|> "stream" :> WebSocket


counterApi :: Proxy CounterApi
counterApi = Proxy


counterApp :: CounterStream -> CS.ServerState -> Application
counterApp counterStream state =
  cors (const $ Just policy)
  $ serve counterApi
  $ server counterStream state
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ] }


server :: CounterStream -> CS.ServerState -> Server CounterApi
server counterStream serverState = record :<|> current :<|> streamData
  where
    record :: CounterEvent -> Handler Counter
    record event = liftIO $ do
      Counter.handleEvent counterStream event
      state <- Counter.getCurrentState counterStream
      CS.broadcast serverState

      pure state

    current :: Handler Counter
    current = do
      state <- liftIO $ Counter.getCurrentState counterStream
      pure state

    streamData :: MonadIO m => WS.Connection -> m ()
    streamData = liftIO . CS.newConnection serverState


start :: IO ()
start = do
  state <- CS.initialState
  let counterStream = CS.getStream state
  run 8081 $ counterApp counterStream state
