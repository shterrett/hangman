module GuessEffect where

import Data.Char (isAlpha, toLower)
import qualified Data.Set as Set
import Control.Effect
import Control.Effect.State
import Control.Lens ((^.), (.~), (%~))
import Data.Text (Text)
import qualified Data.Text as T
import GameState
import Text.Read (readMaybe)

guess :: (Member (State GameState) sig, Carrier sig m)
         => Text
         -> m GuessResult
guess s = do
    game :: GameState <- get
    put $ clearError game
    case isChar s
         >>= alreadyGuessed game
         >>= checkGuess game of
      Right c ->
        put $ insertChar c correctGuesses game
      Left (Incorrect c) ->
        put $ insertChar c incorrectGuesses game
      Left (AlreadyGuessed c) ->
        put $ updateError ((T.pack $ show c) <> " already guessed") game
      Left NotChar ->
        put $ updateError "Guess must be a single character" game
    pure $ fromEither $ loseGame game >>= winGame
  where fromEither = either id id
        insertChar c l = (l %~ (Set.insert c))
        updateError msg = (errorMsg .~ (Just $ msg))
        clearError = (errorMsg .~ Nothing)

isChar :: Text -> Either GuessError Char
isChar = toEither . T.unpack
  where toEither = \case
                     c:"" | isAlpha c -> Right $ toLower c
                          | otherwise -> Left NotChar
                     _ -> Left NotChar

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
