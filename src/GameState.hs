module GameState where

import Data.Text (Text)
import Data.Set (Set)

data GameState =
    GameState { wordList :: [Text]
              , target :: Text
              , haystack :: Set Char
              , correctGuesses :: Set Char
              , incorrectGuesses :: Set Char
              }

data GuessError =
    Incorrect Char
    | AlreadyGuessed Char

data GuessResult =
    Continue
    | Win
    | Lose

maxMisses :: Int
maxMisses = 5
