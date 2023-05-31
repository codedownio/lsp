
module Language.LSP.Test.Session.Core where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson hiding (Error)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import Language.LSP.Test.Decoding
import Language.LSP.Test.Exceptions
import Language.LSP.Test.Types
import Language.LSP.VFS
import UnliftIO.Exception
import UnliftIO.Temporary


sendMessage :: (MonadLoggerIO m, MonadReader SessionContext m, ToJSON a) => a -> m ()
sendMessage msg = do
  h <- asks serverIn
  logMsg LogClient msg
  liftIO $ B.hPut h (addHeader $ encode msg) `catch` (liftIO . throwIO . MessageSendError (toJSON msg))

-- | Logs the message if the config specified it
logMsg :: (
  ToJSON a, MonadLoggerIO m, MonadReader SessionContext m
  ) => LogMsgType -> a -> m ()
logMsg t msg = do
  shouldLog <- asks $ logMessages . config
  when shouldLog $ do
    logDebugN $ T.pack (arrow ++ showPretty msg)

  where
    arrow
      | t == LogServer  = "<-- "
      | otherwise       = "--> "

    showPretty = B.unpack . encodePretty

initVFS' :: MonadUnliftIO m => (VFS -> m r) -> m r
initVFS' k = withSystemTempDirectory "haskell-lsp" $ \temp_dir -> k (VFS mempty temp_dir)
