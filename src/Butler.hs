module Butler where

import Butler.Clock
import Butler.Motherboard

demo :: IO ()
demo =
    withMotherboard \mb -> do
        sleep 1800_000
