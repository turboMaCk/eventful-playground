{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Counter
    ( Counter
    , CounterEvent
    , CounterStream
    , constructStream
    , getCurrentState
    , handleEvent
    , allEvents
    ) where

import Eventful (Projection(..), ExpectedVersion(..), EventStoreWriter, VersionedStreamEvent(..), StreamEvent(..)
                , VersionedEventStoreReader, EventVersion(..), UUID)
import Control.Concurrent.STM (STM)
import Data.Aeson.Types (ToJSON, FromJSON, (.=), (.:), toJSON, toEncoding)
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

instance (ToJSON a) => ToJSON (VersionedStreamEvent a) where
  toJSON (StreamEvent uuid (EventVersion v) event) =
    AT.object [ "uuid" .= show uuid
              , "version" .= show v
              , "event" .= toJSON event
              ]


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


instance ToJSON CounterEvent where
  toJSON event = AT.object [ "msg" .= show event ]
  toEncoding event = AT.pairs ( "state" .= show event )


handleCounterEvent :: Counter -> CounterEvent -> Counter
handleCounterEvent (Counter count) CounterIncremented = Counter (count + 1)
handleCounterEvent (Counter count) CounterDecremented = Counter (count - 1)
handleCounterEvent _ CounterReset = Counter 0


type CounterProjection = Projection Counter CounterEvent


counterProjection :: CounterProjection
counterProjection = Projection
  { projectionSeed = Counter 0
  , projectionEventHandler = handleCounterEvent
  }


-- Stream


data CounterStream = CounterStream
  { uuid :: UUID
  , reader :: VersionedEventStoreReader STM CounterEvent
  , writter :: EventStoreWriter STM CounterEvent
  }


getCurrentState :: CounterStream -> IO Counter
getCurrentState (CounterStream { uuid, reader }) = do
  latestStreamProjection <- STM.atomically $
    Eventful.getLatestStreamProjection reader $
    Eventful.versionedStreamProjection uuid counterProjection
  pure $ Eventful.streamProjectionState latestStreamProjection


handleEvent :: CounterStream -> CounterEvent -> IO ()
handleEvent (CounterStream { uuid, writter }) event = do
  _ <- STM.atomically $ ESM.storeEvents writter AnyVersion uuid [event]
  putStrLn "event stored:"
  print event


constructStream :: IO CounterStream
constructStream = do
  tvar <- ESM.eventMapTVar
  let
    w = ESM.tvarEventStoreWriter tvar
    r = ESM.tvarEventStoreReader tvar
    identi = read "123e4567-e89b-12d3-a456-426655440000"
  pure $ CounterStream identi r w


allEvents :: CounterStream -> IO [ VersionedStreamEvent CounterEvent]
allEvents (CounterStream { uuid, reader }) = do
  events' <- STM.atomically $ Eventful.getEvents reader $ Eventful.allEvents uuid
  pure events'
