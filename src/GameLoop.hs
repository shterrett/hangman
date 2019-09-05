module GameLoop where

import Control.Effect.Carrier (Carrier, Member, Effect)
import Control.Effect.State
import Control.Monad.Loops (iterateUntil)
import Data.List (intersperse, sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GameState
import GuessEffect
import Prelude hiding (read)
import TeletypeEffect
import WordsEffect

gameLoop :: ( Carrier sig m
            , Effect sig
            , Member TeletypeE sig
            , Member WordsE sig
            ) => m ()
gameLoop = do
    ws <- fetchWords
    t <- selectWord ws
    let game = GameState { _wordList = ws
                         , _target = t
                         , _haystack = Set.fromList $ T.unpack t
                         , _correctGuesses = Set.empty
                         , _incorrectGuesses = Set.empty
                         , _errorMsg = Nothing
                         }
    result <- evalState game $ iterateUntil (/= Continue) gameStep
    case result of
      Continue -> writeLine "Error: game ended unexpectedly"
      Win -> writeLine "Congratulations! You won!"
      Lose -> writeLine "Sorry! You lost!"

gameStep :: ( Carrier sig m
            , Effect sig
            , Member (State GameState) sig
            , Member TeletypeE sig
            ) => m GuessResult
gameStep = do
    g :: GameState <- get
    writeLine $ showTarget g
    writeLine $ showIncorrect g
    write "Next Guess> "
    nextGuess <- read
    result <- guess nextGuess
    result <$ printError
  where printError =
          (_errorMsg <$> get)
            >>= \case
                  Just e -> writeLine e
                  Nothing -> pure ()

showTarget :: GameState -> Text
showTarget GameState {..} =
    mconcat $ obfuscate <$> T.unpack _target
  where obfuscate c = if Set.member c _correctGuesses
                        then T.singleton c
                        else unknown

showIncorrect :: GameState -> Text
showIncorrect GameState {..} =
    T.pack $ "Previous Guesses: " <> guesses
  where guesses = intersperse ' ' (sort $ Set.elems _incorrectGuesses)

unknown :: Text
unknown = "▮"
