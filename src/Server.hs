{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server (start) where

import Servant
import Network.Wai.Handler.Warp (run)
import Counter (Counter(..), CounterEvent)
-- import qualified Counter


type CounterApi = "counter" :> ReqBody '[JSON] CounterEvent :> Post '[JSON] Counter
             :<|> "counter" :> Get '[JSON] Counter


counterApi :: Proxy CounterApi
counterApi = Proxy


counterApp :: Application
counterApp = serve counterApi server


server :: Server CounterApi
server = record :<|> current
  where
    record :: CounterEvent -> Handler Counter
    record _ =
      pure $ Counter 0

    current :: Handler Counter
    current = pure $ Counter 0


start :: IO ()
start = run 8081 counterApp
