module Lib
    ( counterStoreExample
    ) where

import Eventful (Projection(..), ExpectedVersion(..))
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
  = CounterIncremented Int
  | CounterDecremented Int
  | CounterReset
  deriving (Show, Eq)


handleCounterEvent :: Counter -> CounterEvent -> Counter
handleCounterEvent (Counter count) (CounterIncremented amount) =
  Counter (count + amount)
handleCounterEvent (Counter count) (CounterDecremented amount) =
  Counter (count - amount)
handleCounterEvent _ (CounterReset) =
  Counter 0


counterProjection :: Projection Counter CounterEvent
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
    events =
      [ CounterIncremented 3
      , CounterDecremented 1
      ]
  _ <- STM.atomically $ Eventful.storeEvents writer AnyVersion uuid events
  -- Now read the events back and print
  events' <- STM.atomically $ ESM.getEvents reader (Eventful.allEvents uuid)
  print events'
