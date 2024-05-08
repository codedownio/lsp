{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}

module Language.LSP.Test.Session (
  Session(..)
  , SessionConfig(..)
  , defaultConfig
  , SessionContext(..)
  , SessionState(..)
  , runSession'
  , modifyStatePure
  , modifyStatePure_
  , modifyStateM
  , ask
  , asks
  , sendMessage
  , updateState
  , logMsg
  , LogMsgType(..)
  , documentChangeUri
  ) where

import Control.Lens hiding (Empty)
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Protocol.Lens as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Test.Compat
import Language.LSP.Test.Decoding
import Language.LSP.Test.Process
import Language.LSP.Test.Session.Core
import Language.LSP.Test.Session.UpdateState
import Language.LSP.Test.Types
import System.IO
import System.Process (ProcessHandle())
import UnliftIO.Async
import UnliftIO.Concurrent hiding (yield, throwTo)
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Timeout

#if __GLASGOW_HASKELL__ == 806
import Control.Monad.Fail
#endif


-- | An internal version of 'runSession' that allows for a custom handler to listen to the server.
-- It also does not automatically send initialize and exit messages.
runSession' :: forall m a. (
  MonadLoggerIO m, MonadUnliftIO m, MonadThrow m
  ) => Handle -- ^ Server in
    -> Handle -- ^ Server out
    -> Maybe ProcessHandle -- ^ Server process
    -> SessionConfig
    -> ClientCapabilities
    -> FilePath -- ^ Root directory
    -> Session m () -- ^ To exit the Server properly
    -> Session m a
    -> m a
runSession' servIn servOut mServerProc config caps rootDir exitServer session = initVFS' $ \vfs -> do
  context <- SessionContext
    servIn
    <$> canonicalizePath rootDir
    <*> newChan
    <*> newMVar newRequestMap
    <*> newEmptyMVar
    <*> pure config
    <*> pure caps
    <*> newMVar (SessionState 0 vfs mempty False Nothing mempty mempty)

  let doShutdown = timeout (messageTimeout config * 10^(6 :: Int)) (runReaderT (unwrapSession exitServer) context) >>= \case
        Just () -> return ()
        Nothing -> logErrorN "Timeout when shutting down server"

  flip finally (whenJust mServerProc (teardownProcess config servIn servOut)) $
    withAsync (flip runReaderT context $ forwardServerMessages servOut) $ \_ ->
      flip finally doShutdown $
        flip withException (\(e :: SomeException) -> logErrorN ("Exception in session: " <> T.pack (show e))) $ do
          runReaderT (unwrapSession session) context


teardownProcess :: MonadLoggerIO m => SessionConfig -> Handle -> Handle -> ProcessHandle -> m ()
teardownProcess config servIn servOut sp = do
  -- Give the server some time to exit cleanly
  -- It makes the server hangs in windows so we have to avoid it
  logInfoN "Beginning to wait for process"
  gracefullyWaitForProcess (messageTimeout config * 10^(6 :: Int)) sp
  liftIO $ cleanupProcess (Just servIn, Just servOut, Nothing, sp)

-- | Read messages from the server output and write them to the messageChan
forwardServerMessages :: (MonadLoggerIO m, MonadUnliftIO m, MonadReader SessionContext m) => Handle -> m ()
forwardServerMessages serverOut = forever $ do
  ctx <- ask

  msgBytes <- liftIO $ getNextMessage serverOut

  msg <- modifyMVar (requestMap ctx) (\reqMap -> pure (decodeFromServerMsg reqMap msgBytes))

  logMsg LogServer msg

  catch (updateState msg) $ \(e :: SomeException) -> do
    logErrorN [i|Exception when updating state in response to message #{msg}: #{e}|]

  catch (respond msg) $ \(e :: SomeException) -> do
    logErrorN [i|Exception when doing automatic responses in response to message #{msg}: #{e}|]

  writeChan (messageChan ctx) msg

whenJust :: Monad m => Maybe t -> (t -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) f = f x

respond :: (MonadLoggerIO m, MonadReader SessionContext m) => FromServerMessage -> m ()
respond (FromServerMess SMethod_WindowWorkDoneProgressCreate req) =
  sendMessage $ TResponseMessage "2.0" (Just $ req ^. L.id) (Right Null)
respond (FromServerMess SMethod_WorkspaceApplyEdit r) = do
  sendMessage $ TResponseMessage "2.0" (Just $ r ^. L.id) (Right $ ApplyWorkspaceEditResult True Nothing Nothing)
respond _ = pure ()
