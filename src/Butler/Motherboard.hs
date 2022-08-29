module Butler.Motherboard (
    Motherboard (..),
    withMotherboard,

    -- * useful re-exports
    module Butler.Process,
    module Butler.Clock,
) where

import Butler.Buzzer
import Butler.Clock
import Butler.Events
import Butler.Logger
import Butler.Storage
import Butler.Prelude
import Butler.Process
import Butler.Processor
import Butler.Storage

data Motherboard = Motherboard
    { processor :: Processor
    , storage :: Storage
    , clock :: Clock
    , logger :: Logger SystemEvent
    , buzzer :: Buzzer
    } deriving (Generic)

withMotherboard :: (Motherboard -> IO a) -> IO a
withMotherboard action = withProcessor \processor -> do
    clock <- newClock
    logger <- atomically (newLogger 42)
    storage <- newStorage ".butler-storage"
    let buzzer = newBuzzer
        mb = Motherboard{..}
    mb.buzzer 440
    action mb
