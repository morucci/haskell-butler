module Butler.Motherboard (
    Motherboard (..),
    withMotherboard,

    -- * helpers
    createProcess,

    -- * useful re-exports
    module Butler.Process,
    module Butler.Clock,
) where

import Butler.Buzzer
import Butler.Clock
import Butler.Events
import Butler.Logger
import Butler.Prelude
import Butler.Process
import Butler.Processor
import Butler.Storage

data Motherboard = Motherboard
    { processor :: Processor
    , clock :: Clock
    , logger :: Logger SystemEvent
    , buzzer :: Buzzer
    }

createProcess :: Motherboard -> Maybe Process -> ProgramName -> ProcessAction -> IO Process
createProcess mb = startProcess mb.clock mb.logger mb.processor

withMotherboard :: (Motherboard -> IO a) -> IO a
withMotherboard action = withProcessor \processor -> do
    clock <- newClock
    logger <- atomically (newLogger 42)
    let buzzer = newBuzzer
        mb = Motherboard{..}
    mb.buzzer 440
    action mb
