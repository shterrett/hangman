module Main where

import Control.Effect (runM)
import TeletypeEffect (runTeletypeIO)
import Echo (echo)

main :: IO ()
main = runM $ runTeletypeIO echo
