module GameState where

import Control.Lens (makeLenses)
import Data.Text (Text)
import Data.Set (Set)

data GameState =
    GameState { _wordList :: [Text]
              , _target :: Text
              , _haystack :: Set Char
              , _correctGuesses :: Set Char
              , _incorrectGuesses :: Set Char
              }
makeLenses ''GameState

data GuessError =
    Incorrect Char
    | AlreadyGuessed Char

data GuessResult =
    Continue
    | Win
    | Lose

maxMisses :: Int
maxMisses = 5
