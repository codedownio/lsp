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
  , modifyStateM
  , ask
  , asks
  , sendMessage
  , updateState
  , logMsg
  , LogMsgType(..)
  , documentChangeUri
  )

where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Language.LSP.Test.Compat
import Language.LSP.Test.Decoding
import Language.LSP.Test.Exceptions
import Language.LSP.Test.Process
import Language.LSP.Test.Session.Core
import Language.LSP.Test.Session.UpdateState
import Language.LSP.Test.Types
import Language.LSP.Types.Capabilities
import System.IO
import System.Process (ProcessHandle())
import UnliftIO.Async
import UnliftIO.Concurrent hiding (yield, throwTo)
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Timeout


-- runSessionMonad :: forall m a. (MonadLoggerIO m, MonadThrow m) => SessionContext -> Session m a -> m a
-- runSessionMonad context (Session session) =
  -- where
  --   conduit :: StateT SessionState (ReaderT SessionContext m) a
  --   conduit = runConduit $ chanSource .| watchdog .| updateStateC .| runConduitParser (catchError session handler)

  --   handler :: ConduitParserException -> ConduitParser i (StateT SessionState (ReaderT SessionContext m)) b
  --   handler (Unexpected "ConduitParser.empty") = do
  --     lastMsg <- fromJust . lastReceivedMessage <$> get
  --     name <- getParserName
  --     liftIO $ throwIO (UnexpectedMessage (T.unpack name) lastMsg)
  --   handler e = throwIO e

  --   chanSource = do
  --     msg <- liftIO $ readChan (messageChan context)
  --     unless (ignoreLogNotifications (config context) && isLogNotification msg) $
  --       yield msg
  --     chanSource

  --   watchdog :: ConduitM SessionMessage FromServerMessage (StateT SessionState (ReaderT SessionContext m)) ()
  --   watchdog = Conduit.awaitForever $ \case
  --     ServerMessage sMsg -> yield sMsg
  --     TimeoutMessage tId -> do
  --       curId <- getCurTimeoutId
  --       when (curId == tId) $ (lastReceivedMessage <$> get) >>= throwIO . Timeout

-- | An internal version of 'runSession' that allows for a custom handler to listen to the server.
-- It also does not automatically send initialize and exit messages.
runSession' :: forall m a. (
  MonadLoggerIO m, MonadUnliftIO m, MonadThrow m
  ) => Handle -- ^ Server in
    -> Handle -- ^ Server out
    -> Maybe ProcessHandle -- ^ Server process
    -> (Handle -> SessionContext -> m ()) -- ^ Server listener
    -> SessionConfig
    -> ClientCapabilities
    -> FilePath -- ^ Root directory
    -> Session m () -- ^ To exit the Server properly
    -> Session m a
    -> m a
runSession' servIn servOut mServerProc servHandler config caps rootDir exitServer session = initVFS' $ \vfs -> do
  liftIO $ hSetBuffering servIn  NoBuffering
  liftIO $ hSetBuffering servOut NoBuffering

  -- Make sure that we don’t get any newline conversion or weird encoding issues.
  liftIO $ hSetBinaryMode servIn True
  liftIO $ hSetBinaryMode servOut True

  context <- SessionContext
    servIn
    <$> canonicalizePath rootDir
    <*> newChan
    <*> newMVar newRequestMap
    <*> newEmptyMVar
    <*> pure config
    <*> pure caps
    <*> newMVar (SessionState 0 vfs mempty False Nothing mempty mempty)

  let serverAndListenerFinalizer :: Async b -> m (Maybe ())
      serverAndListenerFinalizer asy = do
        finally (timeout (messageTimeout config * 10^(6 :: Int)) (runReaderT (unwrapSession exitServer) context)) $ do
          -- Make sure to kill the listener first, before closing
          -- handles etc via cleanupProcess
          cancel asy

          case mServerProc of
            Just sp -> do
              -- Give the server some time to exit cleanly
              -- It makes the server hangs in windows so we have to avoid it
              gracefullyWaitForProcess (messageTimeout config * 10^(6 :: Int)) sp
              liftIO $ cleanupProcess (Just servIn, Just servOut, Nothing, sp)
            _ -> pure ()

  withAsyncWithUnmask (\unmask -> unmask (servHandler servOut context)) $ \asy ->
    flip finally (serverAndListenerFinalizer asy) $ do
      -- If either the server handler or the session throw an exception, rethrow it synchronously
      sessionAsy <- async $ runReaderT (unwrapSession session) context
      waitEitherCatch asy sessionAsy >>= \case
        Left (Left e) -> cancel sessionAsy >> throwIO e
        Left (Right ()) -> cancel sessionAsy >> throwIO UnexpectedServerTermination
        Right (Left e) -> throwIO e
        Right (Right ret) -> return ret
