module Main where

import Control.Effect (runM)
import GameLoop
import TeletypeEffect (runTeletypeIO)
import WordsEffect (runWordsIO)

main :: IO ()
main = runM $ runTeletypeIO . runWordsIO $ gameLoop
