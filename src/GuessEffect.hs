module GuessEffect where

import qualified Data.Set as Set
import Control.Effect
import Control.Effect.State
import Control.Lens ((^.), (%~))
import GameState

guess :: (Member (State GameState) sig, Carrier sig m)
         => Char
         -> m GuessResult
guess c = do
    game :: GameState <- get
    case alreadyGuessed game c >>= checkGuess game of
      Right c' ->
        put $ insertChar game c' correctGuesses
      Left (Incorrect c') ->
        put $ insertChar game c' incorrectGuesses
      Left (AlreadyGuessed _) -> pure ()
    pure $ fromEither $ loseGame game >>= winGame
  where fromEither = either id id
        insertChar g ch l = (l %~ (Set.insert ch)) g

alreadyGuessed :: GameState -> Char -> Either GuessError Char
alreadyGuessed g c =
    if Set.member c (Set.union (g ^. correctGuesses) (g ^. incorrectGuesses))
      then Left $ AlreadyGuessed c
      else Right c

checkGuess :: GameState -> Char -> Either GuessError Char
checkGuess g c =
    if Set.member c (g ^. haystack)
      then Right c
      else Left $ Incorrect c

loseGame :: GameState -> Either GuessResult GameState
loseGame g =
    if Set.size (g ^. incorrectGuesses) >= maxMisses
      then Left Lose
      else Right g

winGame :: GameState -> Either GuessResult GuessResult
winGame g = if (g ^. correctGuesses) == (g ^. haystack)
              then Right Win
              else Left Continue
