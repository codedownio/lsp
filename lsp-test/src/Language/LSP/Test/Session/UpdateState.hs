{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Language.LSP.Test.Session.UpdateState (
  updateState
  , modifyStatePure
  , modifyStatePure_
  , modifyStateM

  , documentChangeUri
  ) where

import Colog.Core (LogAction (..), WithSeverity (..), Severity (..))
import Control.Lens hiding (List, Empty)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.State (execState)
import Data.Foldable
import Data.Function
import qualified Data.HashMap.Strict as HashMap
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text.IO as T
import Language.LSP.Test.Session.Core
import Language.LSP.Test.Types
import Language.LSP.Types
import qualified Language.LSP.Types.Lens as LSP
import Language.LSP.Types.Lens hiding (cancel, contents, edits, item)
import Language.LSP.VFS
import UnliftIO.Concurrent hiding (yield, throwTo)


updateState :: (
  MonadLoggerIO m, MonadUnliftIO m, MonadReader SessionContext m
  ) => FromServerMessage -> m ()
updateState (FromServerMess SProgress req) = case req ^. params . value of
  Begin _ -> modifyStatePure_ $ \s -> s { curProgressSessions = Set.insert (req ^. params . token) $ curProgressSessions s }
  End _ -> modifyStatePure_ $ \s -> s { curProgressSessions = Set.delete (req ^. params . token) $ curProgressSessions s }
  _ -> pure ()

-- Keep track of dynamic capability registration
updateState (FromServerMess SClientRegisterCapability req) = do
  let List newRegs = (\sr@(SomeRegistration r) -> (r ^. LSP.id, sr)) <$> req ^. params . registrations
  ss <- asks sessionState
  modifyMVar_ ss $ \s -> pure (s { curDynCaps = Map.union (Map.fromList newRegs) (curDynCaps s) })

updateState (FromServerMess SClientUnregisterCapability req) = do
  let List unRegs = (^. LSP.id) <$> req ^. params . unregisterations
  modifyStatePure_ $ \s ->
    let newCurDynCaps = foldr' Map.delete (curDynCaps s) unRegs
    in s { curDynCaps = newCurDynCaps }

updateState (FromServerMess STextDocumentPublishDiagnostics n) = do
  let List diags = n ^. params . diagnostics
      doc = n ^. params . uri
  modifyStatePure_ $ \s ->
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

  modifyStateM $ \s -> do
    let newVFS = flip execState (vfs s) $ changeFromServerVFS logger r
    return $ s { vfs = newVFS }

  let groupedParams = groupBy (\a b -> a ^. textDocument == b ^. textDocument) allChangeParams
      mergedParams = map mergeParams groupedParams

  -- TODO: Don't do this when replaying a session
  forM_ mergedParams (sendMessage . NotificationMessage "2.0" STextDocumentDidChange)

  -- Update VFS to new document versions
  let sortedVersions = map (sortBy (compare `on` (^. textDocument . version))) groupedParams
      latestVersions = map ((^. textDocument) . last) sortedVersions

  forM_ latestVersions $ \(VersionedTextDocumentIdentifier u v) ->
    modifyStatePure_ $ \s ->
      let oldVFS = vfs s
          update (VirtualFile oldV file_ver t) = VirtualFile (fromMaybe oldV v) (file_ver +1) t
          newVFS = oldVFS & vfsMap . ix (toNormalizedUri u) %~ update
      in s { vfs = newVFS }

  where
        logger = LogAction $ \(WithSeverity msg sev) -> case sev of { Error -> error $ show msg; _ -> pure () }
        checkIfNeedsOpened u = do
          oldVFS <- vfs <$> (asks sessionState >>= readMVar)

          -- if its not open, open it
          unless (has (vfsMap . ix (toNormalizedUri u)) oldVFS) $ do
            let fp = fromJust $ uriToFilePath u
            contents <- liftIO $ T.readFile fp
            let item = TextDocumentItem (filePathToUri fp) "" 0 contents
                msg = NotificationMessage "2.0" STextDocumentDidOpen (DidOpenTextDocumentParams item)
            sendMessage msg

            modifyStateM $ \s -> do
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

        bumpNewestVersion (VersionedTextDocumentIdentifier u _) =
          head <$> textDocumentVersions u

        -- For a uri returns an infinite list of versions [n,n+1,n+2,...]
        -- where n is the current version
        textDocumentVersions u = do
          vfs' <- vfs <$> (asks sessionState >>= readMVar)
          let curVer = fromMaybe 0 $ vfs' ^? vfsMap . ix (toNormalizedUri u) . lsp_version
          pure $ map (VersionedTextDocumentIdentifier u . Just) [curVer + 1..]

        textDocumentEdits u edits = do
          vers <- textDocumentVersions u
          pure $ map (\(v, e) -> TextDocumentEdit v (List [InL e])) $ zip vers edits

        getChangeParams u (List edits) = do
          map getParamsFromTextDocumentEdit <$> textDocumentEdits u (reverse edits)

        mergeParams :: [DidChangeTextDocumentParams] -> DidChangeTextDocumentParams
        mergeParams ps = let events = concat (toList (map (toList . (^. contentChanges)) ps))
                         in DidChangeTextDocumentParams (head ps ^. textDocument) (List events)
updateState _ = return ()


modifyStatePure :: (MonadUnliftIO m, MonadReader SessionContext m) => (SessionState -> (SessionState, a)) -> m a
modifyStatePure f = do
  ss <- asks sessionState
  modifyMVar ss (pure . f)

modifyStatePure_ :: (MonadUnliftIO m, MonadReader SessionContext m) => (SessionState -> SessionState) -> m ()
modifyStatePure_ f = do
  ss <- asks sessionState
  modifyMVar_ ss (pure . f)

modifyStateM :: (MonadUnliftIO m, MonadReader SessionContext m) => (SessionState -> m SessionState) -> m ()
modifyStateM f = do
  ss <- asks sessionState
  modifyMVar_ ss f

-- extract Uri out from DocumentChange
-- didn't put this in `lsp-types` because TH was getting in the way
documentChangeUri :: DocumentChange -> Uri
documentChangeUri (InL x) = x ^. textDocument . uri
documentChangeUri (InR (InL x)) = x ^. uri
documentChangeUri (InR (InR (InL x))) = x ^. oldUri
documentChangeUri (InR (InR (InR x))) = x ^. uri
