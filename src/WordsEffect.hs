module WordsEffect where


import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Interpret
import Control.Monad.IO.Class
import Data.Kind
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics
import System.Random (randomRIO)

data WordsE (m :: Type -> Type) (k :: Type) =
    FetchWords ([Text] -> m k)
    | SelectWord [Text] (Text -> m k)
    deriving (Functor, Effect, HFunctor, Generic1)

fetchWords :: (Member WordsE sig, Carrier sig m) => m [Text]
fetchWords = send (FetchWords pure)

selectWord :: (Member WordsE sig, Carrier sig m) => [Text] -> m Text
selectWord ws = send (SelectWord ws pure)

runWordsIO :: MonadIO m => InterpretC WordsE m a -> m a
runWordsIO =
    runInterpret \case
      FetchWords k -> liftIO (T.lines <$> T.readFile "/usr/share/dict/words") >>= k
      SelectWord ws k -> do
        idx <- liftIO $ randomRIO (0, length ws)
        k (ws !! idx)
