{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

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

import Colog.Core (LogAction (..), WithSeverity (..), Severity (..))
import Control.Lens hiding (Empty)
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader (ask)
import Control.Monad.Trans.State (StateT, runStateT, execState)
import qualified Control.Monad.Trans.State as State
import Data.Aeson hiding (Error, Null)
import Data.Aeson.Encode.Pretty
import Data.Aeson.Lens ()
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Conduit as Conduit
import Data.Conduit.Parser as Parser
import Data.Default
import Data.Either (partitionEithers)
import Data.Foldable
import Data.Function
import Data.IORef
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Row
import qualified Data.Set as Set
import Data.String (fromString)
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Builder as T
import Language.LSP.Protocol.Lens as L
import qualified Language.LSP.Protocol.Lens as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Message as LSP
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types as LSP
import Language.LSP.Test.Compat
import Language.LSP.Test.Decoding
import Language.LSP.Test.Exceptions
import Language.LSP.Test.Process
import Language.LSP.Test.Session.Core
import Language.LSP.Test.Session.UpdateState
import Language.LSP.Test.Types
import Language.LSP.VFS
import System.Console.ANSI
import System.Directory
import System.IO
import System.Process (ProcessHandle())
import System.Timeout ( timeout )
import UnliftIO.Async
import UnliftIO.Concurrent hiding (yield, throwTo)
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Timeout

#if __GLASGOW_HASKELL__ == 806
import Control.Monad.Fail
#endif

#ifndef mingw32_HOST_OS
import System.Process (waitForProcess)
#endif


-- | A session representing one instance of launching and connecting to a server.
--
-- You can send and receive messages to the server within 'Session' via
-- 'Language.LSP.Test.message',
-- 'Language.LSP.Test.sendRequest' and
-- 'Language.LSP.Test.sendNotification'.

newtype Session a = Session (ConduitParser FromServerMessage (StateT SessionState (ReaderT SessionContext IO)) a)
  deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadThrow)

#if __GLASGOW_HASKELL__ >= 806
instance MonadFail Session where
  fail s = do
    lastMsg <- fromJust . lastReceivedMessage <$> get
    liftIO $ throw (UnexpectedMessage s lastMsg)
#endif

-- | Stuff you can configure for a 'Session'.
data SessionConfig = SessionConfig
  { messageTimeout :: Int
  -- ^ Maximum time to wait for a message in seconds, defaults to 60.
  , logStdErr      :: Bool
  -- ^ Redirect the server's stderr to this stdout, defaults to False.
  -- Can be overriden with @LSP_TEST_LOG_STDERR@.
  , logMessages    :: Bool
  -- ^ Trace the messages sent and received to stdout, defaults to False.
  -- Can be overriden with the environment variable @LSP_TEST_LOG_MESSAGES@.
  , logColor       :: Bool
  -- ^ Add ANSI color to the logged messages, defaults to True.
  , lspConfig      :: Object
  -- ^ The initial LSP config as JSON object, defaults to the empty object.
  -- This should include the config section for the server if it has one, i.e. if
  -- the server has a 'mylang' config section, then the config should be an object
  -- with a 'mylang' key whose value is the actual config for the server. You
  -- can also include other config sections if your server may request those.
  , ignoreLogNotifications :: Bool
  -- ^ Whether or not to ignore @window/showMessage@ and @window/logMessage@ notifications
  -- from the server, defaults to True.
  , ignoreConfigurationRequests :: Bool
  -- ^ Whether or not to ignore @workspace/configuration@ requests from the server,
  -- defaults to True.
  , initialWorkspaceFolders :: Maybe [WorkspaceFolder]
  -- ^ The initial workspace folders to send in the @initialize@ request.
  -- Defaults to Nothing.
  }

-- | The configuration used in 'Language.LSP.Test.runSession'.
defaultConfig :: SessionConfig
defaultConfig = SessionConfig 60 False False True mempty True True Nothing

instance Default SessionConfig where
  def = defaultConfig

data SessionMessage = ServerMessage FromServerMessage
                    | TimeoutMessage Int
  deriving Show

data SessionContext = SessionContext
  {
    serverIn :: Handle
  , rootDir :: FilePath
  , messageChan :: Chan SessionMessage -- ^ Where all messages come through
  -- Keep curTimeoutId in SessionContext, as its tied to messageChan
  , curTimeoutId :: IORef Int -- ^ The current timeout we are waiting on
  , requestMap :: MVar RequestMap
  , initRsp :: MVar (TResponseMessage Method_Initialize)
  , config :: SessionConfig
  , sessionCapabilities :: ClientCapabilities
  }

class Monad m => HasReader r m where
  ask :: m r
  asks :: (r -> b) -> m b
  asks f = f <$> ask

instance HasReader SessionContext Session where
  ask  = Session (lift $ lift Reader.ask)

instance Monad m => HasReader r (ConduitM a b (StateT s (ReaderT r m))) where
  ask = lift $ lift Reader.ask

getCurTimeoutId :: (HasReader SessionContext m, MonadIO m) => m Int
getCurTimeoutId = asks curTimeoutId >>= liftIO . readIORef

-- Pass this the timeoutid you *were* waiting on
bumpTimeoutId :: (HasReader SessionContext m, MonadIO m) => Int -> m ()
bumpTimeoutId prev = do
  v <- asks curTimeoutId
  -- when updating the curtimeoutid, account for the fact that something else
  -- might have bumped the timeoutid in the meantime
  liftIO $ atomicModifyIORef' v (\x -> (max x (prev + 1), ()))

data SessionState = SessionState
  {
    curReqId :: !Int32
  , vfs :: !VFS
  , curDiagnostics :: !(Map.Map NormalizedUri [Diagnostic])
  , overridingTimeout :: !Bool
  -- ^ The last received message from the server.
  -- Used for providing exception information
  , lastReceivedMessage :: !(Maybe FromServerMessage)
  , curDynCaps :: !(Map.Map T.Text SomeRegistration)
  -- ^ The capabilities that the server has dynamically registered with us so
  -- far
  , curLspConfig :: Object
  , curProgressSessions :: !(Set.Set ProgressToken)
  , ignoringLogNotifications :: Bool
  , ignoringConfigurationRequests :: Bool
  }

class Monad m => HasState s m where
  get :: m s

  put :: s -> m ()

  modify :: (s -> s) -> m ()
  modify f = get >>= put . f

  modifyM :: (HasState s m, Monad m) => (s -> m s) -> m ()
  modifyM f = get >>= f >>= put

instance HasState SessionState Session where
  get = Session (lift State.get)
  put = Session . lift . State.put

instance Monad m => HasState s (StateT s m) where
  get = State.get
  put = State.put

instance (Monad m, (HasState s m)) => HasState s (ConduitM a b m)
 where
  get = lift get
  put = lift . put

instance (Monad m, (HasState s m)) => HasState s (ConduitParser a m)
 where
  get = lift get
  put = lift . put

runSessionMonad :: SessionContext -> SessionState -> Session a -> IO (a, SessionState)
runSessionMonad context state (Session session) = runReaderT (runStateT conduit state) context
  where
    conduit = runConduit $ chanSource .| watchdog .| updateStateC .| runConduitParser (catchError session handler)

    handler (Unexpected "ConduitParser.empty") = do
      lastMsg <- fromJust . lastReceivedMessage <$> get
      name <- getParserName
      liftIO $ throw (UnexpectedMessage (T.unpack name) lastMsg)

    handler e = throw e

    chanSource = do
      msg <- liftIO $ readChan (messageChan context)
      yield msg
      chanSource

    watchdog :: ConduitM SessionMessage FromServerMessage (StateT SessionState (ReaderT SessionContext IO)) ()
    watchdog = Conduit.awaitForever $ \msg -> do
      curId <- getCurTimeoutId
      case msg of
        ServerMessage sMsg -> yield sMsg
        TimeoutMessage tId -> when (curId == tId) $ lastReceivedMessage <$> get >>= throw . Timeout

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
    <*> newMVar False
    <*> pure config
    <*> pure caps
    <*> newMVar (SessionState 0 vfs mempty False Nothing mempty mempty)

  let doShutdown = do
        modifyMVar_ (isShuttingDown context) (const $ pure True)
        timeout (messageTimeout config * 10^(6 :: Int)) (runReaderT (unwrapSession exitServer) context) >>= \case
          Just () -> return ()
          Nothing -> logErrorN "Timeout when shutting down server"

  flip finally (whenJust mServerProc (teardownProcess config servIn servOut)) $
    withAsync (flip runReaderT context $ forwardServerMessages servOut) $ \_ ->
      flip finally doShutdown $
        flip withException (\(e :: SomeException) -> logErrorN ("Exception in session: " <> T.pack (show e))) $ do
          runReaderT (unwrapSession session) context

      let (errs, configs) = partitionEithers configsOrErrs

      -- we have to return exactly the number of sections requested, so if we can't find all of them then that's an error
      if null errs
      then sendMessage $ TResponseMessage "2.0" (Just $ r ^. L.id) (Right configs)
      else sendMessage @_ @(TResponseError Method_WorkspaceConfiguration) $
        TResponseError (InL LSPErrorCodes_RequestFailed) ("No configuration for requested sections: " <> (T.pack $ show errs)) Nothing
    _ -> pure ()
  unless ((ignoringLogNotifications state && isLogNotification msg) || (ignoringConfigurationRequests state && isConfigRequest msg)) $
    yield msg

  where

    isLogNotification (FromServerMess SMethod_WindowShowMessage _) = True
    isLogNotification (FromServerMess SMethod_WindowLogMessage _) = True
    isLogNotification (FromServerMess SMethod_WindowShowDocument _) = True
    isLogNotification _ = False

    isConfigRequest (FromServerMess SMethod_WorkspaceConfiguration _) = True
    isConfigRequest _ = False

updateStateC :: ConduitM FromServerMessage FromServerMessage (StateT SessionState (ReaderT SessionContext IO)) ()
updateStateC = awaitForever $ \msg -> do
  state <- get @SessionState
  updateState msg
  case msg of
    FromServerMess SMethod_WindowWorkDoneProgressCreate req ->
      sendMessage $ TResponseMessage "2.0" (Just $ req ^. L.id) (Right Null)
    FromServerMess SMethod_WorkspaceApplyEdit r -> do
      sendMessage $ TResponseMessage "2.0" (Just $ r ^. L.id) (Right $ ApplyWorkspaceEditResult True Nothing Nothing)
    FromServerMess SMethod_WorkspaceConfiguration r -> do
      let requestedSections = mapMaybe (\i -> i ^? L.section . _Just) $ r ^. L.params . L.items
      let o = curLspConfig state
      -- check for each requested section whether we have it
      let configsOrErrs = (flip fmap) requestedSections $ \section ->
            case o ^. at (fromString $ T.unpack section) of
              Just config -> Right config
              Nothing -> Left section

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

  case msg of
    FromServerMess SMethod_WindowLogMessage (TNotificationMessage { _params=(LogMessageParams level text) }) ->
      -- Give a more concise log message for window/logMessage notifications
      logMsg LogServer ("window/logMessage: (" <> T.fromText (T.pack (show level)) <> ") " <> T.fromText text)
    _ -> logMsg LogServer (encodePrettyToTextBuilder msg)

  catch (updateState msg) $ \(e :: SomeException) -> do
    logErrorN [i|Exception when updating state in response to message #{msg}: #{e}|]

  -- Auto-respond to some message types (unless shutdown command has been sent)
  withMVar (isShuttingDown ctx) $ \shuttingDown ->
    unless (shuttingDown) $
      catch (autoRespond msg) $ \(e :: SomeException) -> do
        logErrorN [i|Exception when doing automatic responses in response to message #{msg}: #{e}|]

  writeChan (messageChan ctx) msg

whenJust :: Monad m => Maybe t -> (t -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) f = f x

-- | Automatically respond to some common message types
autoRespond :: (MonadLoggerIO m, MonadReader SessionContext m) => FromServerMessage -> m ()
autoRespond (FromServerMess SMethod_WindowWorkDoneProgressCreate req) =
  sendMessage $ TResponseMessage "2.0" (Just $ req ^. L.id) (Right Null)
autoRespond (FromServerMess SMethod_WorkspaceApplyEdit r) = do
  sendMessage $ TResponseMessage "2.0" (Just $ r ^. L.id) (Right $ ApplyWorkspaceEditResult True Nothing Nothing)
autoRespond _ = pure ()
