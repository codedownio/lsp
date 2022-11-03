{-# LANGUAGE CPP               #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Language.LSP.Test.Session
  ( Session(..)
  , SessionConfig(..)
  , defaultConfig
  , SessionMessage(..)
  , SessionContext(..)
  , SessionState(..)
  , runSession'
  , get
  , put
  , modify
  , modifyM
  , ask
  , asks
  , sendMessage
  , updateState
  , withTimeout
  , getCurTimeoutId
  , bumpTimeoutId
  , logMsg
  , LogMsgType(..)
  , documentChangeUri
  )

where

import Colog.Core (LogAction (..), WithSeverity (..), Severity (..))
import Control.Applicative
import Control.Exception (throw)
import Control.Lens hiding (List, Empty)
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except
#if __GLASGOW_HASKELL__ == 806
import Control.Monad.Fail
#endif
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader (ask)
import Control.Monad.Trans.State (StateT, runStateT, execState)
import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson hiding (Error)
import Data.Aeson.Encode.Pretty
import Data.Conduit as Conduit
import Data.Conduit.Parser as Parser
import Data.Default
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Function
import Language.LSP.Types.Capabilities
import Language.LSP.Types
import Language.LSP.Types.Lens
import qualified Language.LSP.Types.Lens as LSP
import Language.LSP.VFS
import Language.LSP.Test.Compat
import Language.LSP.Test.Decoding
import Language.LSP.Test.Exceptions
import System.Console.ANSI
import System.IO
import System.Process (ProcessHandle())
#ifndef mingw32_HOST_OS
import System.Process (waitForProcess)
#endif
import UnliftIO.Concurrent hiding (yield, throwTo)
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.IORef
import UnliftIO.Timeout

-- | A session representing one instance of launching and connecting to a server.
--
-- You can send and receive messages to the server within 'Session' via
-- 'Language.LSP.Test.message',
-- 'Language.LSP.Test.sendRequest' and
-- 'Language.LSP.Test.sendNotification'.

newtype Session m a = Session (ConduitParser FromServerMessage (StateT SessionState (ReaderT SessionContext m)) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO, Alternative, MonadThrow)

instance MonadLogger m => MonadLogger (ConduitParser FromServerMessage m) where
  monadLoggerLog loc src level msg = lift $ monadLoggerLog loc src level msg

instance MonadLoggerIO m => MonadLoggerIO (ConduitParser FromServerMessage m) where
  askLoggerIO = lift askLoggerIO

#if __GLASGOW_HASKELL__ >= 806
instance MonadIO m => MonadFail (Session m) where
  fail s = do
    lastMsg <- fromJust . lastReceivedMessage <$> get
    liftIO $ throwIO (UnexpectedMessage s lastMsg)
#endif

-- | Stuff you can configure for a 'Session'.
data SessionConfig = SessionConfig
  { messageTimeout :: Int  -- ^ Maximum time to wait for a message in seconds, defaults to 60.
  , logStdErr      :: Bool
  -- ^ Redirect the server's stderr to this stdout, defaults to False.
  -- Can be overriden with @LSP_TEST_LOG_STDERR@.
  , logMessages    :: Bool
  -- ^ Trace the messages sent and received to stdout, defaults to False.
  -- Can be overriden with the environment variable @LSP_TEST_LOG_MESSAGES@.
  , logColor       :: Bool -- ^ Add ANSI color to the logged messages, defaults to True.
  , lspConfig      :: Maybe Value -- ^ The initial LSP config as JSON value, defaults to Nothing.
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
  , initRsp :: MVar (ResponseMessage Initialize)
  , config :: SessionConfig
  , sessionCapabilities :: ClientCapabilities
  }

class Monad m => HasReader r m where
  ask :: m r
  asks :: (r -> b) -> m b
  asks f = f <$> ask

instance Monad m => HasReader SessionContext (Session m) where
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
  , curProgressSessions :: !(Set.Set ProgressToken)
  }

class Monad m => HasState s m where
  get :: m s

  put :: s -> m ()

  modify :: (s -> s) -> m ()
  modify f = get >>= put . f

  modifyM :: (HasState s m, Monad m) => (s -> m s) -> m ()
  modifyM f = get >>= f >>= put

instance Monad m => HasState SessionState (Session m) where
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

runSessionMonad :: forall m a. (MonadLoggerIO m, MonadThrow m) => SessionContext -> SessionState -> Session m a -> m (a, SessionState)
runSessionMonad context state (Session session) = runReaderT (runStateT conduit state) context
  where
    conduit :: StateT SessionState (ReaderT SessionContext m) a
    conduit = runConduit $ chanSource .| watchdog .| updateStateC .| runConduitParser (catchError session handler)

    handler (Unexpected "ConduitParser.empty") = do
      lastMsg <- fromJust . lastReceivedMessage <$> get
      name <- getParserName
      liftIO $ throw (UnexpectedMessage (T.unpack name) lastMsg)

    handler e = throw e

    chanSource = do
      logDebugN "BEGINNING OF chanSource"
      msg <- liftIO $ readChan (messageChan context)
      logDebugN ("GOT MSG: " <> T.pack (show msg))
      unless (ignoreLogNotifications (config context) && isLogNotification msg) $ do
        logDebugN "YIELDING"
        yield msg
        logDebugN "YIELDED"
      chanSource

    isLogNotification (ServerMessage (FromServerMess SWindowShowMessage _)) = True
    isLogNotification (ServerMessage (FromServerMess SWindowLogMessage _)) = True
    isLogNotification (ServerMessage (FromServerMess SWindowShowDocument _)) = True
    isLogNotification _ = False

    watchdog :: ConduitM SessionMessage FromServerMessage (StateT SessionState (ReaderT SessionContext m)) ()
    watchdog = Conduit.awaitForever $ \case
      ServerMessage sMsg -> do
        logDebugN ("watchdog got msg: " <> T.pack (show sMsg))
        yield sMsg
      TimeoutMessage tId -> do
        curId <- getCurTimeoutId
        when (curId == tId) $ lastReceivedMessage <$> get >>= throw . Timeout

-- | An internal version of 'runSession' that allows for a custom handler to listen to the server.
-- It also does not automatically send initialize and exit messages.
runSession' :: forall m a. (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m)
            => Handle -- ^ Server in
            -> Handle -- ^ Server out
            -> Maybe ProcessHandle -- ^ Server process
            -> (Handle -> SessionContext -> IO ()) -- ^ Server listener
            -> SessionConfig
            -> ClientCapabilities
            -> FilePath -- ^ Root directory
            -> Session m () -- ^ To exit the Server properly
            -> Session m a
            -> m a
runSession' serverIn serverOut mServerProc serverHandler config caps rootDir exitServer session = do
  absRootDir <- canonicalizePath rootDir

  liftIO $ hSetBuffering serverIn  NoBuffering
  liftIO $ hSetBuffering serverOut NoBuffering
  -- This is required to make sure that we don’t get any
  -- newline conversion or weird encoding issues.
  liftIO $ hSetBinaryMode serverIn True
  liftIO $ hSetBinaryMode serverOut True

  reqMap <- newMVar newRequestMap
  messageChan <- newChan
  timeoutIdVar <- newIORef 0
  initRsp <- newEmptyMVar

  mainThreadId <- liftIO myThreadId

  withRunInIO $ \runInIO -> do
    let context = SessionContext serverIn absRootDir messageChan timeoutIdVar reqMap initRsp config caps
        initState vfs = SessionState 0 vfs mempty False Nothing mempty mempty

        runSession'' :: Session m b -> m (b, SessionState)
        runSession'' ses = liftIO $ initVFS $ \vfs -> runInIO $ runSessionMonad context (initState vfs) ses

        errorHandler = throwTo mainThreadId :: SessionException -> IO ()

        msgTimeoutMs = messageTimeout config * 10^6

        serverAndListenerFinalizer :: ThreadId -> m (Maybe ((), SessionState))
        serverAndListenerFinalizer tid = do
          logDebugN "DOING serverAndListenerFinalizer"
          finally (timeout (2 * msgTimeoutMs) (logDebugN "Doing runSession'' exitServer" >> runSession'' exitServer)) $ do
            logDebugN "DONE with initial timeout"

            -- Make sure to kill the listener first, before closing
            -- handles etc via cleanupProcess
            liftIO $ killThread tid

            case mServerProc of
              Just sp -> do
                -- Give the server some time to exit cleanly
                -- It makes the server hangs in windows so we have to avoid it
#ifndef mingw32_HOST_OS
                timeout (3 * msgTimeoutMs) (liftIO $ waitForProcess sp)
#endif
                liftIO $ cleanupProcess (Just serverIn, Just serverOut, Nothing, sp)
              _ -> pure ()

    (fst <$>) $ bracket (liftIO $ forkIOWithUnmask $ \unmask -> unmask $ catch (serverHandler serverOut context) errorHandler)
                        (runInIO . serverAndListenerFinalizer)
                        (const $ runInIO $ runSession'' session)

updateStateC :: MonadLoggerIO m => ConduitM FromServerMessage FromServerMessage (StateT SessionState (ReaderT SessionContext m)) ()
updateStateC = awaitForever $ \msg -> do
  logDebugN ("updateStateC got MSG: " <> T.pack (show msg))
  updateState msg
  respond msg
  logDebugN "updateStateC yielding MSG"
  yield msg
  where
    respond :: (MonadLoggerIO m, HasReader SessionContext m) => FromServerMessage -> m ()
    respond (FromServerMess SWindowWorkDoneProgressCreate req) =
      sendMessage $ ResponseMessage "2.0" (Just $ req ^. LSP.id) (Right Empty)
    respond (FromServerMess SWorkspaceApplyEdit r) = do
      sendMessage $ ResponseMessage "2.0" (Just $ r ^. LSP.id) (Right $ ApplyWorkspaceEditResponseBody True Nothing Nothing)
    respond _ = pure ()


-- extract Uri out from DocumentChange
-- didn't put this in `lsp-types` because TH was getting in the way
documentChangeUri :: DocumentChange -> Uri
documentChangeUri (InL x) = x ^. textDocument . uri
documentChangeUri (InR (InL x)) = x ^. uri
documentChangeUri (InR (InR (InL x))) = x ^. oldUri
documentChangeUri (InR (InR (InR x))) = x ^. uri

updateState :: (MonadLoggerIO m, HasReader SessionContext m, HasState SessionState m)
            => FromServerMessage -> m ()
updateState (FromServerMess SProgress req) = case req ^. params . value of
  Begin _ ->
    modify $ \s -> s { curProgressSessions = Set.insert (req ^. params . token) $ curProgressSessions s }
  End _ ->
    modify $ \s -> s { curProgressSessions = Set.delete (req ^. params . token) $ curProgressSessions s }
  _ -> pure ()

-- Keep track of dynamic capability registration
updateState (FromServerMess SClientRegisterCapability req) = do
  let List newRegs = (\sr@(SomeRegistration r) -> (r ^. LSP.id, sr)) <$> req ^. params . registrations
  modify $ \s ->
    s { curDynCaps = Map.union (Map.fromList newRegs) (curDynCaps s) }

updateState (FromServerMess SClientUnregisterCapability req) = do
  let List unRegs = (^. LSP.id) <$> req ^. params . unregisterations
  modify $ \s ->
    let newCurDynCaps = foldr' Map.delete (curDynCaps s) unRegs
    in s { curDynCaps = newCurDynCaps }

updateState (FromServerMess STextDocumentPublishDiagnostics n) = do
  let List diags = n ^. params . diagnostics
      doc = n ^. params . uri
  modify $ \s ->
    let newDiags = Map.insert (toNormalizedUri doc) diags (curDiagnostics s)
      in s { curDiagnostics = newDiags }

updateState (FromServerMess SWorkspaceApplyEdit r) = do

  -- First, prefer the versioned documentChanges field
  allChangeParams <- case r ^. params . edit . documentChanges of
    Just (List cs) -> do
      mapM_ (checkIfNeedsOpened . documentChangeUri) cs
      -- replace the user provided version numbers with the VFS ones + 1
      -- (technically we should check that the user versions match the VFS ones)
      cs' <- traverseOf (traverse . _InL . textDocument) bumpNewestVersion cs
      return $ mapMaybe getParamsFromDocumentChange cs'
    -- Then fall back to the changes field
    Nothing -> case r ^. params . edit . changes of
      Just cs -> do
        mapM_ checkIfNeedsOpened (HashMap.keys cs)
        concat <$> mapM (uncurry getChangeParams) (HashMap.toList cs)
      Nothing ->
        error "WorkspaceEdit contains neither documentChanges nor changes!"

  modifyM $ \s -> do
    let newVFS = flip execState (vfs s) $ changeFromServerVFS logger r
    return $ s { vfs = newVFS }

  let groupedParams = groupBy (\a b -> a ^. textDocument == b ^. textDocument) allChangeParams
      mergedParams = map mergeParams groupedParams

  -- TODO: Don't do this when replaying a session
  forM_ mergedParams (sendMessage . NotificationMessage "2.0" STextDocumentDidChange)

  -- Update VFS to new document versions
  let sortedVersions = map (sortBy (compare `on` (^. textDocument . version))) groupedParams
      latestVersions = map ((^. textDocument) . last) sortedVersions

  forM_ latestVersions $ \(VersionedTextDocumentIdentifier uri v) ->
    modify $ \s ->
      let oldVFS = vfs s
          update (VirtualFile oldV file_ver t) = VirtualFile (fromMaybe oldV v) (file_ver +1) t
          newVFS = oldVFS & vfsMap . ix (toNormalizedUri uri) %~ update
      in s { vfs = newVFS }

  where
        logger = LogAction $ \(WithSeverity msg sev) -> case sev of { Error -> error $ show msg; _ -> pure () }
        checkIfNeedsOpened uri = do
          oldVFS <- vfs <$> get

          -- if its not open, open it
          unless (has (vfsMap . ix (toNormalizedUri uri)) oldVFS) $ do
            let fp = fromJust $ uriToFilePath uri
            contents <- liftIO $ T.readFile fp
            let item = TextDocumentItem (filePathToUri fp) "" 0 contents
                msg = NotificationMessage "2.0" STextDocumentDidOpen (DidOpenTextDocumentParams item)
            sendMessage msg

            modifyM $ \s -> do
              let newVFS = flip execState (vfs s) $ openVFS logger msg
              return $ s { vfs = newVFS }

        getParamsFromTextDocumentEdit :: TextDocumentEdit -> DidChangeTextDocumentParams
        getParamsFromTextDocumentEdit (TextDocumentEdit docId (List edits)) = do
          DidChangeTextDocumentParams docId (List $ map editToChangeEvent edits)

        editToChangeEvent :: TextEdit |? AnnotatedTextEdit -> TextDocumentContentChangeEvent
        editToChangeEvent (InR e) = TextDocumentContentChangeEvent (Just $ e ^. range) Nothing (e ^. newText)
        editToChangeEvent (InL e) = TextDocumentContentChangeEvent (Just $ e ^. range) Nothing (e ^. newText)

        getParamsFromDocumentChange :: DocumentChange -> Maybe DidChangeTextDocumentParams
        getParamsFromDocumentChange (InL textDocumentEdit) = Just $ getParamsFromTextDocumentEdit textDocumentEdit
        getParamsFromDocumentChange _ = Nothing

        bumpNewestVersion (VersionedTextDocumentIdentifier uri _) =
          head <$> textDocumentVersions uri

        -- For a uri returns an infinite list of versions [n,n+1,n+2,...]
        -- where n is the current version
        textDocumentVersions uri = do
          vfs <- vfs <$> get
          let curVer = fromMaybe 0 $ vfs ^? vfsMap . ix (toNormalizedUri uri) . lsp_version
          pure $ map (VersionedTextDocumentIdentifier uri . Just) [curVer + 1..]

        textDocumentEdits uri edits = do
          vers <- textDocumentVersions uri
          pure $ map (\(v, e) -> TextDocumentEdit v (List [InL e])) $ zip vers edits

        getChangeParams uri (List edits) = do
          map <$> pure getParamsFromTextDocumentEdit <*> textDocumentEdits uri (reverse edits)

        mergeParams :: [DidChangeTextDocumentParams] -> DidChangeTextDocumentParams
        mergeParams params = let events = concat (toList (map (toList . (^. contentChanges)) params))
                              in DidChangeTextDocumentParams (head params ^. textDocument) (List events)
updateState _ = return ()

sendMessage :: (MonadLoggerIO m, HasReader SessionContext m, ToJSON a) => a -> m ()
sendMessage msg = do
  h <- serverIn <$> ask
  logDebugN "In sendMessage, about to logMsg"
  logMsg LogClient msg
  logDebugN "In sendMessage, did logMsg"
  liftIO $ B.hPut h (addHeader $ encode msg) `catch` (liftIO . throwIO . MessageSendError (toJSON msg))
  logDebugN "In sendMessage, did hPut"

-- | Execute a block f that will throw a 'Language.LSP.Test.Exception.Timeout' exception
-- after duration seconds. This will override the global timeout
-- for waiting for messages to arrive defined in 'SessionConfig'.
withTimeout :: MonadIO m => Int -> Session m a -> Session m a
withTimeout duration f = do
  chan <- asks messageChan
  timeoutId <- getCurTimeoutId
  modify $ \s -> s { overridingTimeout = True }
  tid <- liftIO $ forkIO $ do
    threadDelay (duration * 1000000)
    writeChan chan (TimeoutMessage timeoutId)
  res <- f
  liftIO $ killThread tid
  bumpTimeoutId timeoutId
  modify $ \s -> s { overridingTimeout = False }
  return res

data LogMsgType = LogServer | LogClient
  deriving Eq

-- | Logs the message if the config specified it
logMsg :: (ToJSON a, MonadLoggerIO m, HasReader SessionContext m)
       => LogMsgType -> a -> m ()
logMsg t msg = do
  shouldLog <- asks $ logMessages . config
  shouldColor <- asks $ logColor . config
  when shouldLog $ do
    -- when shouldColor $ setSGR [SetColor Foreground Dull color]
    logDebugN $ T.pack (arrow ++ showPretty msg)
    -- when shouldColor $ setSGR [Reset]

  where arrow
          | t == LogServer  = "<-- "
          | otherwise       = "--> "
        color
          | t == LogServer  = Magenta
          | otherwise       = Cyan

        showPretty = B.unpack . encodePretty
