-- | Logger provide system logs facility
module Butler.Logger (
    Logger,
    EventSeverity (..),
    Event (..),
    LBSLog (..),
    BSLog (..),
    newLogger,

    -- * Write event
    addEvent,

    -- * Read event
    readLogs,
    getLogsChan,
    waitLog,

    -- * Daemon
    stdoutLogger,
) where

import Data.Text qualified as Text

import Butler.Clock
import Butler.History
import Butler.Prelude

-- Lazy bytestring pretty debug encoding
newtype LBSLog = LBSLog LByteString

instance ToJSON LBSLog where
    toJSON (LBSLog lbs) = String (unsafeFrom lbs)

newtype BSLog = BSLog ByteString

instance ToJSON BSLog where
    toJSON (BSLog bs) = String (Text.take 128 $ Text.replace "\n" "\\n" $ unsafeFrom bs)

data Logger event = Logger
    { history :: History (Event event)
    -- ^ The recent events
    , events :: TChan (Event event)
    -- ^ The worker channel
    }

data EventSeverity = EventError | EventInfo | EventTrace
    deriving (Bounded, Eq, Ord, Show)

instance From EventSeverity Text where
    from = \case
        EventError -> "error"
        EventInfo -> "info"
        EventTrace -> "trace"

data Event event = Event
    { createdAt :: Time
    , severity :: EventSeverity
    , body :: event
    }
    deriving (Eq, Ord, Show)

newLogger :: Natural -> STM (Logger event)
newLogger size = Logger <$> newHistory size <*> newBroadcastTChan

getLogsChan :: Logger event -> STM (TChan (Event event))
getLogsChan logger = dupTChan logger.events

waitLog :: MonadIO m => Logger event -> Milli -> (event -> Bool) -> m (STM (WaitResult (Event event)))
waitLog logger timeLimit test = do
    readChannel <- atomically (getLogsChan logger)
    waitTransaction timeLimit (waitForLog readChannel)
  where
    waitForLog chan = do
        ev <- readTChan chan
        if test (ev.body)
            then pure ev
            else waitForLog chan

readLogs :: Logger event -> STM ([Event event])
readLogs logger = oldestHistory logger.history

stdoutLogger :: From event Text => MonadUnliftIO m => Logger event -> m Void
stdoutLogger logger = do
    (pastEvent, chan) <- atomically do
        (,) <$> readLogs logger <*> getLogsChan logger
    traverse_ putEvent pastEvent
    forever do
        putEvent =<< atomically (readTChan chan)
  where
    putEvent ev = mask_ $ putTextLn $ from ev.createdAt <> "\t" <> from ev.severity <> "\t" <> from ev.body

addEvent :: Logger event -> Time -> EventSeverity -> event -> STM ()
addEvent logger t s e = do
    let event = Event t s e
    -- add to history
    addHistory logger.history event
    -- broadcast the message
    writeTChan logger.events event
