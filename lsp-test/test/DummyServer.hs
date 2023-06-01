{-# LANGUAGE TypeInType #-}

module DummyServer where

import Control.Monad
import Control.Monad.Reader
import Data.Aeson hiding (defaultOptions)
import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.List (isSuffixOf)
import Data.String
import Language.LSP.Server
import Language.LSP.Types
import System.Directory
import System.FilePath
import System.IO
import System.Process
import UnliftIO
import UnliftIO.Concurrent


withDummyServer :: ((Handle, Handle) -> IO ()) -> IO ()
withDummyServer f = do
  (hinRead, hinWrite) <- createPipe
  (houtRead, houtWrite) <- createPipe

  handlerEnv <- HandlerEnv <$> newEmptyMVar <*> newEmptyMVar
  let definition = ServerDefinition
        { doInitialize = \env _req -> pure $ Right env
        , defaultConfig = ()
        , onConfigurationChange = const $ pure $ Right ()
        , staticHandlers = handlers
        , interpretHandler = \env ->
            Iso (\m -> runLspT env (runReaderT m handlerEnv)) liftIO
        , options = defaultOptions {executeCommandCommands = Just ["doAnEdit"]}
        }

  bracket
    (forkIO $ void $ runServerWithHandles mempty mempty hinRead houtWrite definition)
    killThread
    (const $ f (hinWrite, houtRead))


data HandlerEnv = HandlerEnv {
  relRegToken :: MVar (RegistrationToken 'WorkspaceDidChangeWatchedFiles)
  , absRegToken :: MVar (RegistrationToken 'WorkspaceDidChangeWatchedFiles)
  }

handlers :: Handlers (ReaderT HandlerEnv (LspM ()))
handlers =
  mconcat
    [ notificationHandler SInitialized $
        \_noti ->
          sendNotification SWindowLogMessage $
            LogMessageParams MtLog "initialized"
    , requestHandler STextDocumentHover $
        \_req responder ->
          responder $
            Right $
              Just $
                Hover (HoverContents (MarkupContent MkPlainText "hello")) Nothing
    , requestHandler STextDocumentDocumentSymbol $
        \_req responder ->
          responder $
            Right $
              InL $
                List
                  [ DocumentSymbol
                      "foo"
                      Nothing
                      SkObject
                      Nothing
                      Nothing
                      (mkRange 0 0 3 6)
                      (mkRange 0 0 3 6)
                      Nothing
                  ]
     , notificationHandler STextDocumentDidOpen $
        \noti -> do
          let NotificationMessage _ _ (DidOpenTextDocumentParams doc) = noti
              TextDocumentItem uri _ _ _ = doc
              Just fp = uriToFilePath uri
              diag =
                Diagnostic
                  (mkRange 0 0 0 1)
                  (Just DsWarning)
                  (Just (InL 42))
                  (Just "dummy-server")
                  "Here's a warning"
                  Nothing
                  Nothing
          withRunInIO $
            \runInIO -> do
              when (".hs" `isSuffixOf` fp) $
                void $
                  forkIO $
                    do
                      threadDelay (2 * 10^(6 :: Int))
                      runInIO $
                        sendNotification STextDocumentPublishDiagnostics $
                          PublishDiagnosticsParams uri Nothing (List [diag])
              -- also act as a registerer for workspace/didChangeWatchedFiles
              when (".register" `isSuffixOf` fp) $
                do
                  let regOpts =
                        DidChangeWatchedFilesRegistrationOptions $
                          List
                            [ FileSystemWatcher
                                "*.watch"
                                (Just (WatchKind True True True))
                            ]
                  Just token <- runInIO $
                    registerCapability SWorkspaceDidChangeWatchedFiles regOpts $
                      \_noti ->
                        sendNotification SWindowLogMessage $
                          LogMessageParams MtLog "got workspace/didChangeWatchedFiles"
                  runInIO $ asks relRegToken >>= \v -> putMVar v token
              when (".register.abs" `isSuffixOf` fp) $
                do
                  curDir <- getCurrentDirectory
                  let regOpts =
                        DidChangeWatchedFilesRegistrationOptions $
                          List
                            [ FileSystemWatcher
                                (fromString $ curDir </> "*.watch")
                                (Just (WatchKind True True True))
                            ]
                  Just token <- runInIO $
                    registerCapability SWorkspaceDidChangeWatchedFiles regOpts $
                      \_noti ->
                        sendNotification SWindowLogMessage $
                          LogMessageParams MtLog "got workspace/didChangeWatchedFiles"
                  runInIO $ asks absRegToken >>= \v -> putMVar v token
              -- also act as an unregisterer for workspace/didChangeWatchedFiles
              when (".unregister" `isSuffixOf` fp) $
                do
                  Just token <- runInIO $ asks relRegToken >>= tryReadMVar
                  runInIO $ unregisterCapability token
              when (".unregister.abs" `isSuffixOf` fp) $
                do
                  Just token <- runInIO $ asks absRegToken >>= tryReadMVar
                  runInIO $ unregisterCapability token

      -- this handler is used by the
      -- "text document VFS / sends back didChange notifications (documentChanges)" test
    , notificationHandler STextDocumentDidChange $ \noti -> do
        let NotificationMessage _ _ params = noti
        void $ sendNotification (SCustomMethod "custom/textDocument/didChange") (toJSON params)

     , requestHandler SWorkspaceExecuteCommand $ \req resp -> do
       case req of
        RequestMessage _ _ _ (ExecuteCommandParams Nothing "doAnEdit" (Just (List [val]))) -> do
          let
            Success docUri = fromJSON val
            edit = List [TextEdit (mkRange 0 0 0 5) "howdy"]
            params =
              ApplyWorkspaceEditParams (Just "Howdy edit") $
                WorkspaceEdit (Just (HM.singleton docUri edit)) Nothing Nothing
          resp $ Right Null
          void $ sendRequest SWorkspaceApplyEdit params (const (pure ()))
        RequestMessage _ _ _ (ExecuteCommandParams Nothing "doAVersionedEdit" (Just (List [val]))) -> do
          let
            Success versionedDocUri = fromJSON val
            edit = List [InL (TextEdit (mkRange 0 0 0 5) "howdy")]
            documentEdit = TextDocumentEdit versionedDocUri edit
            params =
              ApplyWorkspaceEditParams (Just "Howdy edit") $
                WorkspaceEdit Nothing (Just (List [InL documentEdit])) Nothing
          resp $ Right Null
          void $ sendRequest SWorkspaceApplyEdit params (const (pure ()))
        RequestMessage _ _ _ (ExecuteCommandParams _ name _) ->
          error $ "unsupported command: " <> show name
     , requestHandler STextDocumentCodeAction $ \req resp -> do
        let RequestMessage _ _ _ params = req
            CodeActionParams _ _ _ _ cactx = params
            CodeActionContext diags _ = cactx
            codeActions = fmap diag2ca diags
            diag2ca d =
              CodeAction
                "Delete this"
                Nothing
                (Just (List [d]))
                Nothing
                Nothing
                Nothing
                (Just (Command "" "deleteThis" Nothing))
                Nothing
        resp $ Right $ InR <$> codeActions
     , requestHandler STextDocumentCompletion $ \_req resp -> do
        let res = CompletionList True (List [item])
            item =
              CompletionItem
                "foo"
                Nothing
                (Just CiConstant)
                (Just (List []))
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
        resp $ Right $ InR res
     , requestHandler STextDocumentPrepareCallHierarchy $ \req resp -> do
        let RequestMessage _ _ _ params = req
            CallHierarchyPrepareParams _ pos _ = params
            Position x y = pos
            item =
              CallHierarchyItem
                "foo"
                SkMethod
                Nothing
                Nothing
                (Uri "")
                (Range (Position 2 3) (Position 4 5))
                (Range (Position 2 3) (Position 4 5))
                Nothing
        if x == 0 && y == 0
          then resp $ Right Nothing
          else resp $ Right $ Just $ List [item]
     , requestHandler SCallHierarchyIncomingCalls $ \req resp -> do
        let RequestMessage _ _ _ params = req
            CallHierarchyIncomingCallsParams _ _ item = params
        resp $ Right $ Just $
          List [CallHierarchyIncomingCall item (List [Range (Position 2 3) (Position 4 5)])]
     , requestHandler SCallHierarchyOutgoingCalls $ \req resp -> do
        let RequestMessage _ _ _ params = req
            CallHierarchyOutgoingCallsParams _ _ item = params
        resp $ Right $ Just $
          List [CallHierarchyOutgoingCall item (List [Range (Position 4 5) (Position 2 3)])]
     , requestHandler STextDocumentSemanticTokensFull $ \_req resp -> do
        let tokens = makeSemanticTokens def [SemanticTokenAbsolute 0 1 2 SttType []]
        case tokens of
          Left t -> resp $ Left $ ResponseError InternalError t Nothing
          Right toks -> resp $ Right $ Just toks
    ]
