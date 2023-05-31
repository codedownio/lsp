{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.LSP.Test.Types (
  Session(..)
  , SessionConfig(..)
  , defaultConfig
  , SessionMessage(..)
  , SessionContext(..)
  , SessionState(..)
  , LogMsgType(..)

  , getCurTimeoutId
  , bumpTimeoutId
  )

where

import Control.Applicative
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except
import Control.Monad.Reader
#if __GLASGOW_HASKELL__ == 806
import Control.Monad.Fail
#endif
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Aeson hiding (Error)
import Data.Default
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Maybe
import Language.LSP.Types.Capabilities
import Language.LSP.Types
import Language.LSP.VFS
import Language.LSP.Test.Decoding
import Language.LSP.Test.Exceptions
import System.IO
import UnliftIO.Concurrent hiding (yield, throwTo)
import UnliftIO.Exception
import UnliftIO.IORef


-- | A session representing one instance of launching and connecting to a server.
--
-- You can send and receive messages to the server within 'Session' via
-- 'Language.LSP.Test.message',
-- 'Language.LSP.Test.sendRequest' and
-- 'Language.LSP.Test.sendNotification'.

newtype Session m a = Session (ReaderT SessionContext m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO, Alternative, MonadThrow, MonadReader SessionContext, MonadUnliftIO)

#if __GLASGOW_HASKELL__ >= 806
instance MonadIO m => MonadFail (Session m) where
  fail s = do
    lastMsg <- fromJust . lastReceivedMessage <$> (asks sessionState >>= readMVar)
    liftIO $ throwIO (UnexpectedMessage s lastMsg)
#endif

-- | Stuff you can configure for a 'Session'.
data SessionConfig = SessionConfig {
  messageTimeout :: Int
  -- ^ Maximum time to wait for a message in seconds, defaults to 60.
  , logStdErr      :: Bool
  -- ^ Redirect the server's stderr to this stdout, defaults to False.
  -- Can be overriden with @LSP_TEST_LOG_STDERR@.
  , logMessages    :: Bool
  -- ^ Trace the messages sent and received to stdout, defaults to False.
  -- Can be overriden with the environment variable @LSP_TEST_LOG_MESSAGES@.
  , logColor       :: Bool
  -- ^ Add ANSI color to the logged messages, defaults to True.
  , lspConfig      :: Maybe Value
  -- ^ The initial LSP config as JSON value, defaults to Nothing.
  , ignoreLogNotifications :: Bool
  -- ^ Whether or not to ignore 'Language.LSP.Types.ShowMessageNotification' and
  -- 'Language.LSP.Types.LogMessageNotification', defaults to False.
  --
  -- @since 0.9.0.0
  , initialWorkspaceFolders :: Maybe [WorkspaceFolder]
  -- ^ The initial workspace folders to send in the @initialize@ request.
  -- Defaults to Nothing.
  }

-- | The configuration used in 'Language.LSP.Test.runSession'.
defaultConfig :: SessionConfig
defaultConfig = SessionConfig 60 False False True Nothing False Nothing

instance Default SessionConfig where
  def = defaultConfig

data SessionMessage =
  ServerMessage FromServerMessage
  | TimeoutMessage Int
  deriving Show

data SessionContext = SessionContext {
  serverIn :: Handle
  , rootDir :: FilePath
  , messageChan :: Chan SessionMessage
  -- ^ Where all messages come through
  , curTimeoutId :: IORef Int
  -- ^ The current timeout we are waiting on
  -- Keep curTimeoutId in SessionContext, as its tied to messageChan
  , requestMap :: MVar RequestMap
  , initRsp :: MVar (ResponseMessage 'Initialize)
  , config :: SessionConfig
  , sessionCapabilities :: ClientCapabilities
  , sessionState :: MVar SessionState
  }

data SessionState = SessionState {
  curReqId :: !Int32
  , vfs :: !VFS
  , curDiagnostics :: !(Map.Map NormalizedUri [Diagnostic])
  , overridingTimeout :: !Bool
  , lastReceivedMessage :: !(Maybe FromServerMessage)
  -- ^ The last received message from the server.
  -- Used for providing exception information
  , curDynCaps :: !(Map.Map T.Text SomeRegistration)
  -- ^ The capabilities that the server has dynamically registered with us so far
  , curProgressSessions :: !(Set.Set ProgressToken)
  }

data LogMsgType = LogServer | LogClient
  deriving Eq

-- * Utilities for working with timeouts

getCurTimeoutId :: (MonadReader SessionContext m, MonadIO m) => m Int
getCurTimeoutId = asks curTimeoutId >>= liftIO . readIORef

-- Pass this the timeoutid you *were* waiting on
bumpTimeoutId :: (MonadReader SessionContext m, MonadIO m) => Int -> m ()
bumpTimeoutId prev = do
  v <- asks curTimeoutId
  -- when updating the curtimeoutid, account for the fact that something else
  -- might have bumped the timeoutid in the meantime
  liftIO $ atomicModifyIORef' v (\x -> (max x (prev + 1), ()))
