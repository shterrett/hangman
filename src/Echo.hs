module Echo where

import Prelude hiding (read)

import Control.Effect.Carrier (Carrier, Member)
import Control.Monad (forever)
import TeletypeEffect
import WordsEffect

echo :: (Carrier sig m, Member TeletypeE sig) => m ()
echo = do
    write ("> ")
    x <- read
    writeLine ("echo:> " <> x)

echoWord :: ( Carrier sig m
            , Member WordsE sig
            , Member TeletypeE sig
            )
            => m ()
echoWord = forever do
    ws <- fetchWords
    w <- selectWord ws
    writeLine ("The selected word is " <> w)
    writeLine ("Enter the selected word")
    write ("> ")
    w' <- read
    if w' == w
      then writeLine "Success!"
      else writeLine "Failure :("
