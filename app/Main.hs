module Main where

import Control.Effect (runM)
import TeletypeEffect (runTeletypeIO)
import WordsEffect (runWordsIO)
import Echo (echoWord)

main :: IO ()
main = runM $ runTeletypeIO . runWordsIO $ echoWord
