{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (start) where

import Servant
import Network.Wai.Handler.Warp (run)
import Counter (Counter, CounterEvent)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Servant.API.WebSocket (WebSocket)
import qualified CounterState as CS
import qualified Network.WebSockets as WS


-- Api

type CounterApi = "counter" :> ReqBody '[JSON] CounterEvent :> Post '[JSON] Counter
             :<|> "counter" :> Get '[JSON] Counter
             :<|> "stream" :> WebSocket


counterApi :: Proxy CounterApi
counterApi = Proxy


counterApp :: CS.ServerState -> Application
counterApp state =
  cors (const $ Just policy)
  $ serve counterApi
  $ server state
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ] }


server :: CS.ServerState -> Server CounterApi
server serverState = record :<|> current :<|> joinStream
  where
    record :: CounterEvent -> Handler Counter
    record = liftIO . CS.handleEvent serverState

    current :: Handler Counter
    current = liftIO $ CS.current serverState

    joinStream :: MonadIO m => WS.Connection -> m ()
    joinStream = liftIO . CS.newConnection serverState


start :: IO ()
start = do
  state <- CS.initialState
  run 8081 $ counterApp state
