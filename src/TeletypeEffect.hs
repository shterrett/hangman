module TeletypeEffect where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Interpret
import Control.Monad.IO.Class
import Data.Kind
import Data.Text (Text)
import qualified Data.Text.IO as T
import GHC.Generics
import System.IO (stdout, hFlush)

data TeletypeE (m :: Type -> Type) (k :: Type) =
    Read (Text -> m k)
    | WriteLine Text (m k)
    | Write Text (m k)
    deriving (Functor, Effect, HFunctor, Generic1)

read :: (Member TeletypeE sig, Carrier sig m) => m Text
read = send (Read pure)

write :: (Member TeletypeE sig, Carrier sig m) => Text -> m ()
write s = send (Write s (pure ()))

writeLine :: (Member TeletypeE sig, Carrier sig m) => Text -> m ()
writeLine s = send (WriteLine s (pure ()))

runTeletypeIO :: MonadIO m => InterpretC TeletypeE m a -> m a
runTeletypeIO =
    runInterpret \case
      Read k -> liftIO T.getLine >>= k
      Write s k -> toStdOut T.putStr s >> k
      WriteLine s k -> toStdOut T.putStrLn s >> k
  where toStdOut :: MonadIO m => (Text -> IO ()) -> Text -> m ()
        toStdOut f s = liftIO $ (f s) >> (hFlush stdout)
