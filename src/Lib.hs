module Lib
    ( counterStoreExample
    ) where

import Eventful (Projection(..), ExpectedVersion(..), EventStoreWriter, VersionedEventStoreReader, UUID)
import Control.Concurrent.STM (STM, TVar)
import Control.Monad (forever)
import qualified Eventful
import qualified Control.Concurrent.STM as STM
import qualified Eventful.Store.Memory as ESM

-- MODEL


newtype Counter = Counter
  { unCounter :: Int
  }
  deriving (Show, Eq)


-- UPDATE

data CounterEvent
  = CounterIncremented
  | CounterDecremented
  | CounterReset
  deriving (Show, Eq)


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

counterStoreExample :: IO ()
counterStoreExample = do
  -- First we need to create our in-memory event store.
  tvar <- ESM.eventMapTVar

  let
    writer = ESM.tvarEventStoreWriter tvar
    reader = ESM.tvarEventStoreReader tvar

  -- Lets store some events. Note that the 'atomically' functions is how we
  -- execute STM actions.
  let
    uuid = read "123e4567-e89b-12d3-a456-426655440000"

  initial <- getCurrentState uuid reader
  print initial
  handleEvent uuid writer CounterIncremented
  handleEvent uuid writer CounterIncremented
  handleEvent uuid writer CounterDecremented
  final <- getCurrentState uuid reader
  print final



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
