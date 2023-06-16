{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Language.LSP.Test
Description : A functional testing framework for LSP servers.
Maintainer  : luke_lau@icloud.com
Stability   : experimental
Portability : non-portable

Provides the framework to start functionally testing
<https://github.com/Microsoft/language-server-protocol Language Server Protocol servers>.
You should import "Language.LSP.Types" alongside this.
-}
module Language.LSP.Test (
  -- * Sessions
  Session
  , runSession
  , runSessionWithConfig
  , runSessionWithConfigCustomProcess
  , runSessionWithHandles
  , runSessionWithHandles'
  -- ** Config
  , SessionConfig(..)
  , defaultConfig
  , C.fullCaps
  -- ** Exceptions
  , module Language.LSP.Test.Exceptions
  -- * Sending
  , request
  , request_
  , sendRequest
  , sendNotification
  , sendResponse
  -- * Receiving
  , module Language.LSP.Test.Parsing
  -- * Utilities
  -- | Quick helper functions for common tasks.

  -- ** Initialization
  , initializeResponse
  -- ** Documents
  , createDoc
  , openDoc
  , openDoc'
  , closeDoc
  , changeDoc
  , documentContents
  , getDocumentEdit
  , getDocUri
  , getVersionedDoc
  -- ** Symbols
  , getDocumentSymbols
  -- ** Diagnostics
  , waitForDiagnostics
  , waitForDiagnosticsSource
  , noDiagnostics
  , getCurrentDiagnostics
  , getIncompleteProgressSessions
  -- ** Commands
  , executeCommand
  -- ** Code Actions
  , getCodeActions
  , getAllCodeActions
  , executeCodeAction
  -- ** Completions
  , getCompletions
  -- ** References
  , getReferences
  -- ** Definitions
  , getDeclarations
  , getDefinitions
  , getTypeDefinitions
  , getImplementations
  -- ** Renaming
  , rename
  -- ** Hover
  , getHover
  -- ** Highlights
  , getHighlights
  -- ** Formatting
  , formatDoc
  , formatRange
  -- ** Edits
  , applyEdit
  -- ** Code lenses
  , getCodeLenses
  -- ** Call hierarchy
  , prepareCallHierarchy
  , incomingCalls
  , outgoingCalls
  -- ** SemanticTokens
  , getSemanticTokens
  -- ** Capabilities
  , getRegisteredCapabilities
  -- ** Custom requests
  , getCustomRequest
  ) where

import Control.Applicative.Combinators
import UnliftIO.Concurrent
import Control.Exception (throw)
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.State (execState)
import Control.Applicative
import Control.Lens hiding ((.=), List, Empty)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Row
import qualified Data.Text.IO as T
import Data.Aeson hiding (Null)
import Data.Default
import Data.List
import Data.Maybe
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Lens hiding (id, capabilities, message, executeCommand, applyEdit, rename, to)
import qualified Language.LSP.Protocol.Lens as L
import qualified Language.LSP.Protocol.Capabilities as C
import Language.LSP.VFS
import Language.LSP.Test.Compat
import Language.LSP.Test.Decoding
import Language.LSP.Test.Exceptions
import Language.LSP.Test.Parsing
import Language.LSP.Test.Session
import Language.LSP.Test.Server
import Prelude hiding (pred)
import System.Environment
import System.IO
import System.FilePath
import System.Process (ProcessHandle, CreateProcess)
import qualified System.FilePath.Glob as Glob
import UnliftIO.Directory
import UnliftIO.Exception

-- | Starts a new session.
--
-- > runSession "hie" fullCaps "path/to/root/dir" $ do
-- >   doc <- openDoc "Desktop/simple.hs" "haskell"
-- >   diags <- waitForDiagnostics
-- >   let pos = Position 12 5
-- >       params = TextDocumentPositionParams doc
-- >   hover <- request STextdocumentHover params
runSession :: (
  MonadLoggerIO m, MonadUnliftIO m, MonadThrow m, Alternative m
  ) => CreateProcess -- ^ The command to run the server.
    -> ClientCapabilities -- ^ The capabilities that the client should declare.
    -> FilePath -- ^ The filepath to the root directory for the session.
    -> Session m a -- ^ The session to run.
    -> m a
runSession = runSessionWithConfig def

-- | Starts a new session with a custom configuration.
runSessionWithConfig :: (
  MonadLoggerIO m, MonadUnliftIO m, MonadThrow m, Alternative m
  ) => SessionConfig -- ^ Configuration options for the session.
    -> CreateProcess -- ^ The command to run the server.
    -> ClientCapabilities -- ^ The capabilities that the client should declare.
    -> FilePath -- ^ The filepath to the root directory for the session.
    -> Session m a -- ^ The session to run.
    -> m a
runSessionWithConfig = runSessionWithConfigCustomProcess id

-- | Starts a new session with a custom configuration and server 'CreateProcess'.
runSessionWithConfigCustomProcess :: (
  MonadLoggerIO m, MonadUnliftIO m, MonadThrow m, Alternative m
  ) => (CreateProcess -> CreateProcess) -- ^ Tweak the 'CreateProcess' used to start the server.
    -> SessionConfig -- ^ Configuration options for the session.
    -> CreateProcess -- ^ The base CreateProcess run the server.
    -> ClientCapabilities -- ^ The capabilities that the client should declare.
    -> FilePath -- ^ The filepath to the root directory for the session.
    -> Session m a -- ^ The session to run.
    -> m a
runSessionWithConfigCustomProcess modifyCreateProcess config' serverExe caps root session = do
  conf <- liftIO $ envOverrideConfig config'
  withServer serverExe (logStdErr conf) modifyCreateProcess $ \servIn servOut servProc ->
    runSessionWithHandles' (Just servProc) servIn servOut conf caps root session

-- | Starts a new session, using the specified handles to communicate with the
-- server. You can use this to host the server within the same process.
-- An example with lsp might look like:
--
-- > (hinRead, hinWrite) <- createPipe
-- > (houtRead, houtWrite) <- createPipe
-- >
-- > forkIO $ void $ runServerWithHandles hinRead houtWrite serverDefinition
-- > runSessionWithHandles hinWrite houtRead defaultConfig fullCaps "." $ do
-- >   -- ...
runSessionWithHandles :: (
  MonadLoggerIO m, MonadUnliftIO m, MonadThrow m, Alternative m
  ) => Handle -- ^ The input handle
    -> Handle -- ^ The output handle
    -> SessionConfig
    -> ClientCapabilities -- ^ The capabilities that the client should declare.
    -> FilePath -- ^ The filepath to the root directory for the session.
    -> Session m a -- ^ The session to run.
    -> m a
runSessionWithHandles = runSessionWithHandles' Nothing


runSessionWithHandles' :: forall m a. (
  MonadLoggerIO m, MonadUnliftIO m, MonadThrow m, Alternative m
  ) => Maybe ProcessHandle
    -> Handle -- ^ The input handle
    -> Handle -- ^ The output handle
    -> SessionConfig
    -> ClientCapabilities -- ^ The capabilities that the client should declare.
    -> FilePath -- ^ The filepath to the root directory for the session.
    -> Session m a -- ^ The session to run.
    -> m a
runSessionWithHandles' servProc servIn servOut config' caps root session = do
  pid <- liftIO getCurrentProcessID
  absRootDir <- liftIO $ canonicalizePath root

  conf <- liftIO $ envOverrideConfig config'

  let initializeParams = InitializeParams Nothing
                                          -- Narrowing to Int32 here, but it's unlikely that a PID will
                                          -- be outside the range
                                          (InL $ fromIntegral pid)
                                          (Just (#name .== "lsp-text" .+ #version .== (Just CURRENT_PACKAGE_VERSION)))
                                          (Just $ T.pack absRootDir)
                                          Nothing
                                          (InL $ filePathToUri absRootDir)
                                          caps
                                          (lspConfig config')
                                          (Just TraceValues_Off)
                                          (fmap InL $ initialWorkspaceFolders conf)
  runSession' servIn servOut servProc conf caps root exitServer $ do
    -- Wrap the session around initialize and shutdown calls
    initReqId <- sendRequest SMethod_Initialize initializeParams

    -- Because messages can be sent in between the request and response,
    -- collect them and then...
    (inBetween, initRspMsg) <- manyTill_ anyMessage (responseForId SMethod_Initialize initReqId)

    case initRspMsg ^. L.result of
      Left err -> logErrorN $ T.pack ("Error while initializing: " ++ show err)
      Right _ -> pure ()

    initRspVar <- initRsp <$> ask
    liftIO $ putMVar initRspVar initRspMsg
    sendNotification SMethod_Initialized InitializedParams

    case lspConfig conf of
      Just cfg -> sendNotification SMethod_WorkspaceDidChangeConfiguration (DidChangeConfigurationParams cfg)
      Nothing -> return ()

    -- ... relay them back to the user Session so they can match on them!
    -- As long as they are allowed.
    forM_ inBetween checkLegalBetweenMessage
    msgChan <- asks messageChan
    liftIO $ writeList2Chan msgChan inBetween

    -- Run the actual test
    session

  where
    -- | Asks the server to shutdown and exit politely
    exitServer :: Session m ()
    exitServer = request_ SMethod_Shutdown Nothing >> sendNotification SMethod_Exit Nothing

-- | Check environment variables to override the config
envOverrideConfig :: SessionConfig -> IO SessionConfig
envOverrideConfig cfg = do
  logMessages' <- fromMaybe (logMessages cfg) <$> checkEnv "LSP_TEST_LOG_MESSAGES"
  logStdErr' <- fromMaybe (logStdErr cfg) <$> checkEnv "LSP_TEST_LOG_STDERR"
  return $ cfg { logMessages = logMessages', logStdErr = logStdErr' }
  where checkEnv :: String -> IO (Maybe Bool)
        checkEnv s = fmap convertVal <$> lookupEnv s
        convertVal "0" = False
        convertVal _ = True

-- | The current text contents of a document.
documentContents :: MonadLoggerIO m => TextDocumentIdentifier -> Session m T.Text
documentContents doc = do
  vfs' <- vfs <$> (asks sessionState >>= readMVar)
  let Just file = vfs' ^. vfsMap . at (toNormalizedUri (doc ^. uri))
  return (virtualFileText file)

-- | Parses an ApplyEditRequest, checks that it is for the passed document
-- and returns the new content
getDocumentEdit :: (MonadLoggerIO m, MonadUnliftIO m) => TextDocumentIdentifier -> Session m T.Text
getDocumentEdit doc = do
  req <- message SMethod_WorkspaceApplyEdit

  unless (checkDocumentChanges req || checkChanges req) $
    liftIO $ throwIO (IncorrectApplyEditRequest (show req))

  documentContents doc
  where
    checkDocumentChanges req =
      let maybeDocs = fmap (fmap documentChangeUri) (req ^. params . edit . documentChanges)
      in case maybeDocs of
        Just docs -> (doc ^. uri) `elem` docs
        Nothing -> False
    checkChanges req =
      let mMap = req ^. L.params . L.edit . L.changes
        in maybe False (Map.member (doc ^. L.uri)) mMap

-- | Sends a request to the server and waits for its response.
-- Will skip any messages in between the request and the response
-- @
-- rsp <- request STextDocumentDocumentSymbol params
-- @
-- Note: will skip any messages in between the request and the response.
request :: (MonadLoggerIO n, MonadUnliftIO n, Alternative n) => SClientMethod m -> MessageParams m -> Session n (TResponseMessage m)
request m = sendRequest m >=> skipManyTill anyMessage . responseForId m

-- | The same as 'sendRequest', but discard the response.
request_ :: (MonadLoggerIO n, MonadUnliftIO n, Alternative n) => SClientMethod (m :: Method 'ClientToServer 'Request) -> MessageParams m -> Session n ()
request_ p = void . request p

-- | Sends a request to the server. Unlike 'request', this doesn't wait for the response.
sendRequest :: (
  MonadLoggerIO n, MonadUnliftIO n, Alternative n
  ) => SClientMethod m -- ^ The request method.
    -> MessageParams m -- ^ The request parameters.
    -> Session n (LspId m) -- ^ The id of the request that was sent.
sendRequest meth ps = do
  lspId <- IdInt <$> modifyStatePure (\c -> (c { curReqId = curReqId c + 1 }, curReqId c))

  let mess = TRequestMessage "2.0" lspId meth ps

  -- Update the request map
  reqMap <- requestMap <$> ask
  liftIO $ modifyMVar_ reqMap $
    \r -> return $ fromJust $ updateRequestMap r lspId meth

  ~() <- case splitClientMethod meth of
    IsClientReq -> sendMessage mess
    IsClientEither -> sendMessage $ ReqMess mess

  return lspId

-- | Sends a notification to the server.
sendNotification :: (
  MonadLoggerIO n, MonadUnliftIO n, Alternative n
  ) => SClientMethod (m :: Method 'ClientToServer 'Notification) -- ^ The notification method.
    -> MessageParams m -- ^ The notification parameters.
    -> Session n ()
-- Open a virtual file if we send a did open text document notification
sendNotification SMethod_TextDocumentDidOpen ps = do
  let n = TNotificationMessage "2.0" SMethod_TextDocumentDidOpen ps
  modifyStatePure_ (\s -> (s { vfs = flip execState (vfs s) $ openVFS mempty n }))
  sendMessage n

-- Close a virtual file if we send a close text document notification
sendNotification SMethod_TextDocumentDidClose ps = do
  let n = TNotificationMessage "2.0" SMethod_TextDocumentDidClose ps
  ss <- asks sessionState
  oldVFS <- vfs <$> readMVar ss
  let newVFS = flip execState oldVFS $ closeVFS mempty n
  modifyMVar_ ss (\s -> pure (s { vfs = newVFS }))
  sendMessage n

sendNotification SMethod_TextDocumentDidChange ps = do
  let n = TNotificationMessage "2.0" SMethod_TextDocumentDidChange ps
  modifyStatePure_ (\s -> s {vfs = flip execState (vfs s) $ changeFromClientVFS mempty n})
  sendMessage n

sendNotification meth ps =
  case splitClientMethod meth of
    IsClientNot -> sendMessage (TNotificationMessage "2.0" meth ps)
    IsClientEither -> sendMessage (NotMess $ TNotificationMessage "2.0" meth ps)

-- | Sends a response to the server.
sendResponse :: (MonadLoggerIO n, ToJSON (ErrorData m)) => ToJSON (MessageResult m) => TResponseMessage m -> Session n ()
sendResponse = sendMessage

-- | Returns the initialize response that was received from the server.
-- The initialize requests and responses are not included the session,
-- so if you need to test it use this.
initializeResponse :: MonadLoggerIO m => Session m (TResponseMessage Method_Initialize)
initializeResponse = ask >>= (liftIO . readMVar) . initRsp

-- | /Creates/ a new text document. This is different from 'openDoc'
-- as it sends a workspace/didChangeWatchedFiles notification letting the server
-- know that a file was created within the workspace, __provided that the server
-- has registered for it__, and the file matches any patterns the server
-- registered for.
-- It /does not/ actually create a file on disk, but is useful for convincing
-- the server that one does exist.
--
-- @since 11.0.0.0
createDoc :: (
  MonadLoggerIO m, MonadUnliftIO m, Alternative m
  ) => FilePath -- ^ The path to the document to open, __relative to the root directory__.
    -> T.Text -- ^ The text document's language identifier, e.g. @"haskell"@.
    -> T.Text -- ^ The content of the text document to create.
    -> Session m TextDocumentIdentifier -- ^ The identifier of the document just created.
createDoc file langId contents = do
  dynCaps <- curDynCaps <$> (asks sessionState >>= readMVar)
  rootDir <- asks rootDir
  caps <- asks sessionCapabilities
  absFile <- liftIO $ canonicalizePath (rootDir </> file)
  let pred :: SomeRegistration -> [TRegistration 'Method_WorkspaceDidChangeWatchedFiles]
      pred (SomeRegistration r@(TRegistration _ SMethod_WorkspaceDidChangeWatchedFiles _)) = [r]
      pred _ = mempty
      regs = concatMap pred $ Map.elems dynCaps
      watchHits :: FileSystemWatcher -> Bool
      watchHits (FileSystemWatcher (GlobPattern (InL (Pattern pattern))) kind) =
        -- If WatchKind is excluded, defaults to all true as per spec
        fileMatches (T.unpack pattern) && containsCreate (fromMaybe WatchKind_Create kind)
      -- TODO: Relative patterns
      watchHits _ = False

      fileMatches pattern = Glob.match (Glob.compile pattern) relOrAbs
        -- If the pattern is absolute then match against the absolute fp
        where relOrAbs
                | isAbsolute pattern = absFile
                | otherwise = file

      regHits :: TRegistration Method_WorkspaceDidChangeWatchedFiles -> Bool
      regHits reg = foldl' (\acc w -> acc || watchHits w) False (reg ^. L.registerOptions . _Just . L.watchers)

      clientCapsSupports =
          caps ^? L.workspace . _Just . L.didChangeWatchedFiles . _Just . L.dynamicRegistration . _Just
            == Just True
      shouldSend = clientCapsSupports && foldl' (\acc r -> acc || regHits r) False regs

  when shouldSend $
    sendNotification SMethod_WorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams $
      [ FileEvent (filePathToUri (rootDir </> file)) FileChangeType_Created ]
  openDoc' file langId contents

-- | Opens a text document that /exists on disk/, and sends a
-- textDocument/didOpen notification to the server.
openDoc :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => FilePath -> T.Text -> Session m TextDocumentIdentifier
openDoc file langId = do
  ctx <- ask
  let fp = rootDir ctx </> file
  contents <- liftIO $ T.readFile fp
  openDoc' file langId contents

-- | This is a variant of `openDoc` that takes the file content as an argument.
-- Use this is the file exists /outside/ of the current workspace.
openDoc' :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => FilePath -> T.Text -> T.Text -> Session m TextDocumentIdentifier
openDoc' file langId contents = do
  ctx <- ask
  let fp = rootDir ctx </> file
      u = filePathToUri fp
      tdi = TextDocumentItem u langId 0 contents
  sendNotification SMethod_TextDocumentDidOpen (DidOpenTextDocumentParams tdi)
  pure $ TextDocumentIdentifier u

-- | Closes a text document and sends a textDocument/didOpen notification to the server.
closeDoc :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> Session m ()
closeDoc docId = do
  let params = DidCloseTextDocumentParams (TextDocumentIdentifier (docId ^. L.uri))
  sendNotification SMethod_TextDocumentDidClose params

-- | Changes a text document and sends a textDocument/didOpen notification to the server.
changeDoc :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> [TextDocumentContentChangeEvent] -> Session m ()
changeDoc docId changes' = do
  verDoc <- getVersionedDoc docId
  let ps = DidChangeTextDocumentParams (verDoc & L.version +~ 1) changes'
  sendNotification SMethod_TextDocumentDidChange ps

-- | Gets the Uri for the file corrected to the session directory.
getDocUri :: MonadLoggerIO m => FilePath -> Session m Uri
getDocUri file = do
  ctx <- ask
  let fp = rootDir ctx </> file
  return $ filePathToUri fp

-- | Waits for diagnostics to be published and returns them.
waitForDiagnostics :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => Session m [Diagnostic]
waitForDiagnostics = do
  diagsNot <- skipManyTill anyMessage (message SMethod_TextDocumentPublishDiagnostics)
  return $ diagsNot ^. params . L.diagnostics

-- | The same as 'waitForDiagnostics', but will only match a specific
-- 'Language.LSP.Types._source'.
waitForDiagnosticsSource :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => String -> Session m [Diagnostic]
waitForDiagnosticsSource src = do
  diags <- waitForDiagnostics
  let res = filter matches diags
  if null res
    then waitForDiagnosticsSource src
    else return res
  where
    matches :: Diagnostic -> Bool
    matches d = d ^. L.source == Just (T.pack src)

-- | Expects a 'PublishDiagnosticsNotification' and throws an
-- 'UnexpectedDiagnostics' exception if there are any diagnostics
-- returned.
noDiagnostics :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => Session m ()
noDiagnostics = do
  diagsNot <- message SMethod_TextDocumentPublishDiagnostics
  when (diagsNot ^. L.params . L.diagnostics /= []) $ liftIO $ throwIO UnexpectedDiagnostics

-- | Returns the symbols in a document.
getDocumentSymbols :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> Session m (Either [SymbolInformation] [DocumentSymbol])
getDocumentSymbols doc = do
  TResponseMessage _ rspLid res <- request SMethod_TextDocumentDocumentSymbol (DocumentSymbolParams Nothing Nothing doc)
  case res of
    Right (InL xs) -> return (Left xs)
    Right (InR (InL xs)) -> return (Right xs)
    Right (InR (InR _)) -> return (Right [])
    Left err -> throwIO (UnexpectedResponseError (SomeLspId $ fromJust rspLid) err)

-- | Returns the code actions in the specified range.
getCodeActions :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> Range -> Session m [Command |? CodeAction]
getCodeActions doc range' = do
  ctx <- getCodeActionContextInRange doc range'
  rsp <- request SMethod_TextDocumentCodeAction (CodeActionParams Nothing Nothing doc range' ctx)

  case rsp ^. L.result of
    Right (InL xs) -> return xs
    Right (InR _) -> return []
    Left err -> throwIO (UnexpectedResponseError (SomeLspId $ fromJust $ rsp ^. L.id) err)

-- | Returns all the code actions in a document by
-- querying the code actions at each of the current
-- diagnostics' positions.
getAllCodeActions :: forall m. (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> Session m [Command |? CodeAction]
getAllCodeActions doc = do
  ctx <- getCodeActionContext doc

  foldM (go ctx) [] =<< getCurrentDiagnostics doc

  where
    go :: CodeActionContext -> [Command |? CodeAction] -> Diagnostic -> Session m [Command |? CodeAction]
    go ctx acc diag = do
      TResponseMessage _ rspLid res <- request SMethod_TextDocumentCodeAction (CodeActionParams Nothing Nothing doc (diag ^. L.range) ctx)

      case res of
        Left e -> throw (UnexpectedResponseError (SomeLspId $ fromJust rspLid) e)
        Right (InL cmdOrCAs) -> pure (acc ++ cmdOrCAs)
        Right (InR _) -> pure acc

getCodeActionContextInRange :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> Range -> Session m CodeActionContext
getCodeActionContextInRange doc caRange = do
  curDiags <- getCurrentDiagnostics doc
  let diags = [ d | d@Diagnostic{_range=range} <- curDiags
                  , overlappingRange caRange range]
  return $ CodeActionContext diags Nothing Nothing
  where
    overlappingRange :: Range -> Range -> Bool
    overlappingRange (Range s e) range' = positionInRange' s range' || positionInRange' e range'

    -- Note: this seems to be slightly different from the positionInRange from lsp-types
    positionInRange' :: Position -> Range -> Bool
    positionInRange' (Position pl po) (Range (Position sl so) (Position el eo)) =
         pl >  sl && pl <  el
      || pl == sl && pl == el && po >= so && po <= eo
      || pl == sl && po >= so
      || pl == el && po <= eo

getCodeActionContext :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> Session m CodeActionContext
getCodeActionContext doc = do
  curDiags <- getCurrentDiagnostics doc
  return $ CodeActionContext curDiags Nothing Nothing

-- | Returns the current diagnostics that have been sent to the client.
-- Note that this does not wait for more to come in.
getCurrentDiagnostics :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> Session m [Diagnostic]
getCurrentDiagnostics doc = fromMaybe [] . Map.lookup (toNormalizedUri $ doc ^. L.uri) . curDiagnostics <$> (asks sessionState >>= readMVar)

-- | Returns the tokens of all progress sessions that have started but not yet ended.
getIncompleteProgressSessions :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => Session m (Set.Set ProgressToken)
getIncompleteProgressSessions = curProgressSessions <$> (asks sessionState >>= readMVar)

-- | Executes a command.
executeCommand :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => Command -> Session m ()
executeCommand cmd = do
  let args = decode $ encode $ fromJust $ cmd ^. L.arguments
      execParams = ExecuteCommandParams Nothing (cmd ^. L.command) args
  void $ sendRequest SMethod_WorkspaceExecuteCommand execParams

-- | Executes a code action.
-- Matching with the specification, if a code action
-- contains both an edit and a command, the edit will
-- be applied first.
executeCodeAction :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => CodeAction -> Session m ()
executeCodeAction action = do
  maybe (return ()) handleEdit $ action ^. L.edit
  maybe (return ()) executeCommand $ action ^. L.command

  where handleEdit :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => WorkspaceEdit -> Session m ()
        handleEdit e =
          -- Its ok to pass in dummy parameters here as they aren't used
          let req = TRequestMessage "" (IdInt 0) SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing e)
            in updateState (FromServerMess SMethod_WorkspaceApplyEdit req)

-- | Adds the current version to the document, as tracked by the session.
getVersionedDoc :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> Session m VersionedTextDocumentIdentifier
getVersionedDoc (TextDocumentIdentifier u) = do
  vfs <- vfs <$> (asks sessionState >>= readMVar)
  let ver = vfs ^? vfsMap . ix (toNormalizedUri u) . to virtualFileVersion
  return (VersionedTextDocumentIdentifier u (fromMaybe 0 ver))

-- | Applys an edit to the document and returns the updated document version.
applyEdit :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> TextEdit -> Session m VersionedTextDocumentIdentifier
applyEdit doc edit' = do
  verDoc <- getVersionedDoc doc

  caps <- asks sessionCapabilities

  let supportsDocChanges = fromMaybe False $ caps ^? L.workspace . _Just . L.workspaceEdit . _Just . L.documentChanges . _Just

  let wEdit = if supportsDocChanges
      then
        let docEdit = TextDocumentEdit (review _versionedTextDocumentIdentifier verDoc) [InL edit']
        in WorkspaceEdit Nothing (Just [InL docEdit]) Nothing
      else
        let changes = Map.singleton (doc ^. L.uri) [edit']
        in WorkspaceEdit (Just changes) Nothing Nothing

  let req = TRequestMessage "" (IdInt 0) SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wEdit)
  updateState (FromServerMess SMethod_WorkspaceApplyEdit req)

  -- version may have changed
  getVersionedDoc doc

-- | Returns the completions for the position in the document.
getCompletions :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> Position -> Session m [CompletionItem]
getCompletions doc pos = do
  rsp <- request SMethod_TextDocumentCompletion (CompletionParams doc pos Nothing Nothing Nothing)

  case getResponseResult rsp of
    InL items -> return items
    InR (InL c) -> return $ c ^. L.items
    InR (InR _) -> return []

-- | Returns the references for the position in the document.
getReferences :: (
  MonadLoggerIO m, MonadUnliftIO m, Alternative m
  ) => TextDocumentIdentifier -- ^ The document to lookup in.
    -> Position -- ^ The position to lookup.
    -> Bool -- ^ Whether to include declarations as references.
    -> Session m [Location] -- ^ The locations of the references.
getReferences doc pos inclDecl =
  let ctx = ReferenceContext inclDecl
      params = ReferenceParams doc pos Nothing Nothing ctx
  in absorbNull . getResponseResult <$> request SMethod_TextDocumentReferences params

-- | Returns the declarations(s) for the term at the specified position.
getDeclarations :: (
  MonadLoggerIO m, MonadUnliftIO m, Alternative m
  ) => TextDocumentIdentifier -- ^ The document the term is in.
    -> Position -- ^ The position the term is at.
    -> Session m (Declaration |? [DeclarationLink] |? Null)
getDeclarations doc pos = do
  rsp <- request SMethod_TextDocumentDeclaration (DeclarationParams doc pos Nothing Nothing)
  pure $ getResponseResult rsp

-- | Returns the definition(s) for the term at the specified position.
getDefinitions :: (
  MonadLoggerIO m, MonadUnliftIO m, Alternative m
  ) => TextDocumentIdentifier -- ^ The document the term is in.
    -> Position -- ^ The position the term is at.
    -> Session m (Definition |? [DefinitionLink] |? Null)
getDefinitions doc pos = do
  rsp <- request SMethod_TextDocumentDefinition (DefinitionParams doc pos Nothing Nothing)
  pure $ getResponseResult rsp

-- | Returns the type definition(s) for the term at the specified position.
getTypeDefinitions :: (
  MonadLoggerIO m, MonadUnliftIO m, Alternative m
  ) => TextDocumentIdentifier -- ^ The document the term is in.
    -> Position -- ^ The position the term is at.
    -> Session m (Definition |? [DefinitionLink] |? Null)
getTypeDefinitions doc pos = do
  rsp <- request SMethod_TextDocumentTypeDefinition (TypeDefinitionParams doc pos Nothing Nothing)
  pure $ getResponseResult rsp

-- | Returns the type definition(s) for the term at the specified position.
getImplementations :: (
  MonadLoggerIO m, MonadUnliftIO m, Alternative m
  ) => TextDocumentIdentifier -- ^ The document the term is in.
    -> Position -- ^ The position the term is at.
    -> Session m (Definition |? [DefinitionLink] |? Null)
getImplementations doc pos = do
  rsp <- request SMethod_TextDocumentImplementation (ImplementationParams doc pos Nothing Nothing)
  pure $ getResponseResult rsp

-- | Renames the term at the specified position.
rename :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> Position -> String -> Session m ()
rename doc pos newName = do
  let ps = RenameParams Nothing doc pos (T.pack newName)
  rsp <- request SMethod_TextDocumentRename ps
  let wEdit = getResponseResult rsp
  case nullToMaybe wEdit of
    Just e -> do
      let req = TRequestMessage "" (IdInt 0) SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing e)
      updateState (FromServerMess SMethod_WorkspaceApplyEdit req)
    Nothing -> pure ()

-- | Returns the hover information at the specified position.
getHover :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> Position -> Session m (Maybe Hover)
getHover doc pos =
  let params = HoverParams doc pos Nothing
  in nullToMaybe . getResponseResult <$> request SMethod_TextDocumentHover params

-- | Returns the highlighted occurrences of the term at the specified position
getHighlights :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> Position -> Session m [DocumentHighlight]
getHighlights doc pos =
  let params = DocumentHighlightParams doc pos Nothing Nothing
  in absorbNull . getResponseResult <$> request SMethod_TextDocumentDocumentHighlight params

-- | Checks the response for errors and throws an exception if needed.
-- Returns the result if successful.
getResponseResult :: (ToJSON (ErrorData m)) => TResponseMessage m -> MessageResult m
getResponseResult rsp =
  case rsp ^. L.result of
    Right x -> x
    Left err -> throw $ UnexpectedResponseError (SomeLspId $ fromJust $ rsp ^. L.id) err

-- | Applies formatting to the specified document.
formatDoc :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> FormattingOptions -> Session m ()
formatDoc doc opts = do
  let params = DocumentFormattingParams Nothing doc opts
  edits <- absorbNull . getResponseResult <$> request SMethod_TextDocumentFormatting params
  applyTextEdits doc edits

-- | Applies formatting to the specified range in a document.
formatRange :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> FormattingOptions -> Range -> Session m ()
formatRange doc opts range = do
  let params = DocumentRangeFormattingParams Nothing doc range opts
  edits <- absorbNull . getResponseResult <$> request SMethod_TextDocumentRangeFormatting params
  applyTextEdits doc edits

applyTextEdits :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> [TextEdit] -> Session m ()
applyTextEdits doc edits =
  let wEdit = WorkspaceEdit (Just (Map.singleton (doc ^. L.uri) edits)) Nothing Nothing
      -- Send a dummy message to updateState so it can do bookkeeping
      req = TRequestMessage "" (IdInt 0) SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wEdit)
  in updateState (FromServerMess SMethod_WorkspaceApplyEdit req)

-- | Returns the code lenses for the specified document.
getCodeLenses :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> Session m [CodeLens]
getCodeLenses tId = do
  rsp <- request SMethod_TextDocumentCodeLens (CodeLensParams Nothing Nothing tId)
  pure $ absorbNull $ getResponseResult rsp

-- | Pass a param and return the response from `prepareCallHierarchy`
prepareCallHierarchy :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => CallHierarchyPrepareParams -> Session m [CallHierarchyItem]
prepareCallHierarchy = resolveRequestWithListResp SMethod_TextDocumentPrepareCallHierarchy

incomingCalls :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => CallHierarchyIncomingCallsParams -> Session m [CallHierarchyIncomingCall]
incomingCalls = resolveRequestWithListResp SMethod_CallHierarchyIncomingCalls

outgoingCalls :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => CallHierarchyOutgoingCallsParams -> Session m [CallHierarchyOutgoingCall]
outgoingCalls = resolveRequestWithListResp SMethod_CallHierarchyOutgoingCalls

-- | Send a request and receive a response with list.
resolveRequestWithListResp :: forall (m :: Method ClientToServer Request) a n . (
  ToJSON (ErrorData m)
  , MessageResult m ~ ([a] |? Null)
  , MonadLoggerIO n
  , MonadUnliftIO n
  , Alternative n
  ) => SMethod m
    -> MessageParams m
    -> Session n [a]
resolveRequestWithListResp method params = do
  rsp <- request method params
  pure $ absorbNull $ getResponseResult rsp

-- | Pass a param and return the response from `prepareCallHierarchy`
getSemanticTokens :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> Session m (SemanticTokens |? Null)
getSemanticTokens doc = do
  let ps = SemanticTokensParams Nothing Nothing doc
  rsp <- request SMethod_TextDocumentSemanticTokensFull ps
  pure $ getResponseResult rsp

-- | Returns a list of capabilities that the server has requested to /dynamically/
-- register during the 'Session'.
--
-- @since 0.11.0.0
getRegisteredCapabilities :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => Session m [SomeRegistration]
getRegisteredCapabilities = Map.elems . curDynCaps <$> (asks sessionState >>= readMVar)

-- | Returns the result of running a custom LSP command.
-- For example: rust-analyzer has a command called "rust-analyzer/analyzerStatus"
--
-- @since 0.12.0.0
getCustomRequest :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => T.Text -> Value -> Session m Value
getCustomRequest methodName ps = Prelude.error "TODO"
  -- getResponseResult <$> request (SCustomMethod methodName) ps
