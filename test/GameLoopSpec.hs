module GameLoopSpec where

import Control.Lens ((.~))
import GameLoop
import GameState
import qualified Data.Set as Set
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec =
    describe "printing game state" $ do
      let g = GameState { _wordList = []
                        , _target = "oxymoron"
                        , _haystack = Set.fromList "oxymoron"
                        , _correctGuesses = Set.empty
                        , _incorrectGuesses = Set.empty
                        , _errorMsg = Nothing
                        }
      it "prints the word with 'unknown' for unguessed letters" $ do
        showTarget g `shouldBe` (mconcat $ replicate 8 unknown)
      it "replaces all occurences of a guessed letter with that letter" $ do
        let g' = (correctGuesses .~ (Set.fromList "orn")) g
        showTarget g' `shouldBe` (mconcat [ "o"
                                          , unknown
                                          , unknown
                                          , unknown
                                          , "o"
                                          , "r"
                                          , "o"
                                          , "n"
                                          ])
      it "prints the incorrect guesses separated by a space" $ do
        let g' = (incorrectGuesses .~ (Set.fromList "abcdefg")) g
        showIncorrect g' `shouldBe` "Previous Guesses: " <> (T.pack $ "a b c d e f g")
