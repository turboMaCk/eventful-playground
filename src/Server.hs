{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (start) where

import Servant
import Network.Wai.Handler.Warp (run)
import Counter (Counter, CounterEvent, CounterStream)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Cors
import qualified Counter


type CounterApi = "counter" :> ReqBody '[JSON] CounterEvent :> Post '[JSON] Counter
             :<|> "counter" :> Get '[JSON] Counter


counterApi :: Proxy CounterApi
counterApi = Proxy


counterApp :: CounterStream -> Application
counterApp counterStream =
  cors (const $ Just policy)
  $ serve counterApi
  $ server counterStream
  where
    policy = simpleCorsResourcePolicy
            { corsRequestHeaders = [ "content-type" ] }


server :: CounterStream -> Server CounterApi
server counterStream = record :<|> current
  where
    record :: CounterEvent -> Handler Counter
    record event = do
      _ <- liftIO $ Counter.handleEvent counterStream event
      state <- liftIO $ Counter.getCurrentState counterStream
      pure state

    current :: Handler Counter
    current = do
      state <- liftIO $ Counter.getCurrentState counterStream
      pure state


start :: IO ()
start = do
  counterStream <- Counter.constructStream
  run 8081 $ counterApp counterStream
