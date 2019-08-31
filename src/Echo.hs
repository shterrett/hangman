module Echo where

import Prelude hiding (read)

import Control.Effect.Carrier (Carrier, Member)
import TeletypeEffect

echo :: (Carrier sig m, Member Teletype sig) => m ()
echo = do
    write ("> ")
    x <- read
    writeLine ("echo:> " <> x)
