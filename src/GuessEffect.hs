module GuessEffect where

import qualified Data.Set as Set
import Control.Effect
import Control.Effect.State
import GameState

guess :: (Member (State GameState) sig, Carrier sig m)
         => Char
         -> m GuessResult
guess c = do
    game :: GameState <- get
    case alreadyGuessed game c >>= checkGuess game of
      Right c' ->
        put $ game { correctGuesses = Set.insert c' (correctGuesses game) }
      Left (Incorrect c') ->
        put $ game { incorrectGuesses = Set.insert c' (incorrectGuesses game) }
      Left (AlreadyGuessed _) -> pure ()
    pure $ either id id $ loseGame game >>= winGame

alreadyGuessed :: GameState -> Char -> Either GuessError Char
alreadyGuessed g c =
    if Set.member c (Set.union (correctGuesses g) (incorrectGuesses g))
      then Left $ AlreadyGuessed c
      else Right c

checkGuess :: GameState -> Char -> Either GuessError Char
checkGuess g c =
    if Set.member c (haystack g)
      then Right c
      else Left $ Incorrect c

loseGame :: GameState -> Either GuessResult GameState
loseGame g =
    if Set.size (incorrectGuesses g) >= maxMisses
      then Left Lose
      else Right g

winGame :: GameState -> Either GuessResult GuessResult
winGame g = if (correctGuesses g) == (haystack g)
              then Right Win
              else Left Continue
