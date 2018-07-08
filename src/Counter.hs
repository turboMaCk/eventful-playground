{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Counter
    ( Counter
    , CounterEvent
    , CounterStream
    , constructStream
    , getCurrentState
    , handleEvent
    , interactive
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


-- CLI


interactive :: IO ()
interactive = do
  putStrLn "Choose your command!"
  putStrLn "  * `+` - increment counter"
  putStrLn "  * `-` - decrement counter"
  putStrLn "  * `=` - see current state"
  putStrLn "  * `C-c` - exit"

  counterStream <- constructStream
  forever $ fromArg counterStream


fromArg :: CounterStream -> IO ()
fromArg stream = getLine >>= \input ->
  case input of
    "=" -> do
      state <- getCurrentState stream
      print state
    "+" ->
      handleEvent stream CounterIncremented
    "-" ->
      handleEvent stream CounterDecremented
    _ ->
      putStrLn "UNKNOWN COMMAND"
