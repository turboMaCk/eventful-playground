{-# LANGUAGE OverloadedStrings #-}

module Counter
    ( Counter(..)
    , CounterEvent
    , run
    ) where

import Eventful (Projection(..), ExpectedVersion(..), EventStoreWriter, VersionedEventStoreReader, UUID)
import Control.Concurrent.STM (STM)
import Control.Monad (forever)
import Data.Aeson.Types (ToJSON, FromJSON, (.=), (.:))
import qualified Data.Aeson.Types as AT
import qualified Eventful
import qualified Control.Concurrent.STM as STM
import qualified Eventful.Store.Memory as ESM

-- MODEL


newtype Counter = Counter
  { unCounter :: Int
  }
  deriving (Show, Eq)


instance ToJSON Counter where
  toJSON (Counter state) = AT.object [ "state" .= state ]
  toEncoding (Counter state) = AT.pairs ("state" .= state)


-- UPDATE


data CounterEvent
  = CounterIncremented
  | CounterDecremented
  | CounterReset
  deriving (Show, Eq)


instance FromJSON CounterEvent where
  parseJSON obj@(AT.Object v) = (v .: "msg") >>=
    \str ->
        case str of
            AT.String "Increment" -> pure CounterIncremented
            AT.String "Decrement" -> pure CounterDecremented
            AT.String "Reset" -> pure CounterReset
            _ -> AT.typeMismatch "Event" obj
  parseJSON invalid = AT.typeMismatch "Event" invalid


handleCounterEvent :: Counter -> CounterEvent -> Counter
handleCounterEvent (Counter count) CounterIncremented = Counter (count + 1)
handleCounterEvent (Counter count) CounterDecremented = Counter (count - 1)
handleCounterEvent _ CounterReset = Counter 0


type CounterProjection = Projection Counter CounterEvent


counterProjection :: CounterProjection
counterProjection =
  Projection
  { projectionSeed = Counter 0
  , projectionEventHandler = handleCounterEvent
  }


-- Store

run :: IO ()
run = do
  -- First we need to create our in-memory event store.
  tvar <- ESM.eventMapTVar

  let
    writer = ESM.tvarEventStoreWriter tvar
    reader = ESM.tvarEventStoreReader tvar
    uuid = read "123e4567-e89b-12d3-a456-426655440000"

  putStrLn "Choose your command!"
  putStrLn "  * `+` - increment counter"
  putStrLn "  * `-` - decrement counter"
  putStrLn "  * `=` - see current state"
  putStrLn "  * `C-c` - exit"
  forever $ interactive uuid reader writer


interactive :: UUID -> VersionedEventStoreReader STM CounterEvent -> EventStoreWriter STM CounterEvent -> IO ()
interactive uuid reader writer = getLine >>= \input ->
  case input of
    "=" -> do
      state <- getCurrentState uuid reader
      print state
    "+" ->
      handleEvent uuid writer CounterIncremented
    "-" ->
      handleEvent uuid writer CounterDecremented
    _ ->
      putStrLn "UNKNOWN COMMAND"


getCurrentState :: UUID -> VersionedEventStoreReader STM CounterEvent -> IO Counter
getCurrentState uuid reader = do
  latestStreamProjection <- STM.atomically $
    Eventful.getLatestStreamProjection reader $
    Eventful.versionedStreamProjection uuid counterProjection
  pure $ Eventful.streamProjectionState latestStreamProjection


handleEvent :: UUID -> EventStoreWriter STM CounterEvent -> CounterEvent -> IO ()
handleEvent uuid writer event = do
  _ <- STM.atomically $ ESM.storeEvents writer AnyVersion uuid [event]
  putStrLn "event stored:"
  print event
