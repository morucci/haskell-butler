-- | Clock provides timing facility
module Butler.Clock (
    Clock (..),
    newClock,

    -- * Read time
    Time,
    toEpoch,

    -- * Duration
    Milli (..),
    sleep,

    -- * stm helpers
    WaitResult (..),
    waitTransaction,
) where

import Butler.Prelude
import Data.UnixTime

data Clock = Clock
    { createdAt :: UnixTime
    , getTime :: IO Time
    }

newClock :: MonadIO m => m Clock
newClock = liftIO do
    start <- getUnixTime
    let readTime = do
            now <- getUnixTime
            let elapsed = now `diffUnixTime` start
                milli :: Int64
                milli =
                    (from $ elapsed.udtSeconds) * 1_000
                        + (from $ elapsed.udtMicroSeconds) `div` 1_000
            pure $ Time (unsafeFrom milli)
    pure $ Clock start readTime

-- | The number of milli secondes since the clock started.
newtype Time = Time Natural
    deriving (Eq, Ord, Show)
    deriving newtype (Serialise, Num, Real, Enum, Integral, ToJSON)

-- | A duration in millisecond
newtype Milli = Milli Natural deriving newtype (Num, Eq, Ord)

instance ToHtml Time where
    toHtml (Time n) = toHtml (show n <> "ms")

instance From Time ByteString where
  from = via @Text

instance From Time Milli where
    from (Time n) = Milli n

instance From Time Natural where
    from (Time n) = n

instance From Natural Time where
    from = Time

instance From Time Text where
    from (Time t) = from (show t) <> "ms"

toEpoch :: Clock -> Time -> UTCTime
toEpoch = error "TODO: add time duration to the starting date"

sleep :: MonadIO m => Milli -> m ()
sleep (Milli x) = liftIO $ threadDelay (unsafeFrom x * 1_000)

data WaitResult result = WaitCompleted result | WaitTimeout
    deriving (Show)

instance ToJSON result => ToJSON (WaitResult result) where
  toJSON = \case
    WaitCompleted res -> toJSON res
    WaitTimeout -> "timeout"

waitTransaction :: MonadIO m => Milli -> STM result -> m (STM (WaitResult result))
waitTransaction (Milli x) action = liftIO do
    delay <- registerDelay (unsafeFrom x * 1_000)
    pure $ (WaitCompleted <$> action) <|> (WaitTimeout <$ (check =<< readTVar delay))
