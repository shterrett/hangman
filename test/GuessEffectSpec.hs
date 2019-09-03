module GuessEffectSpec where

import Control.Lens ((.~))
import qualified Data.Set as Set
import Data.Text (Text)
import GuessEffect
import GameState
import Test.Hspec

spec :: Spec
spec = do
    describe "Validating single character" $ do
      it "accepts a valid character" $ do
        let a :: Text = "a"
        isChar a `shouldBe` Right 'a'
      it "lowercases the character" $ do
        let a :: Text = "A"
        isChar a `shouldBe` Right 'a'
      it "rejects multiple characters" $ do
        let ab :: Text = "ab"
        isChar ab `shouldBe` Left NotChar
      it "rejects numbers" $ do
        let five :: Text = "5"
        isChar five `shouldBe` Left NotChar
      it "rejects symbols" $ do
        let dash :: Text = "-"
        isChar dash `shouldBe` Left NotChar
    describe "character already guessed" $ do
      let g = GameState { _wordList = []
                        , _target = "oxymoron"
                        , _haystack = Set.fromList "oxymoron"
                        , _correctGuesses = Set.empty
                        , _incorrectGuesses = Set.empty
                        , _errorMsg = Nothing
                        }
      it "returns the char if it has not been guessed" $
        alreadyGuessed g 'a' `shouldBe` Right 'a'
      it "returns an error if the character has been guessed correctly" $ do
        let g' = (correctGuesses .~ Set.singleton 'a') g
        alreadyGuessed g' 'a' `shouldBe` Left (AlreadyGuessed 'a')
      it "returns an error if the character has been guessed incorrectly" $ do
        let g' = (incorrectGuesses .~ Set.singleton 'a') g
        alreadyGuessed g' 'a' `shouldBe` Left (AlreadyGuessed 'a')
    describe "checking guess" $ do
      let g = GameState { _wordList = []
                        , _target = "oxymoron"
                        , _haystack = Set.fromList "oxymoron"
                        , _correctGuesses = Set.empty
                        , _incorrectGuesses = Set.empty
                        , _errorMsg = Nothing
                        }
      it "returns the character if it is in the word" $
        checkGuess g 'o' `shouldBe` Right 'o'
      it "returns ag n error if the character is not in the word" $
        checkGuess g 'a' `shouldBe` Left (Incorrect 'a')
