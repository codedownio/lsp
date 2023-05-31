{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.LSP.Test.Parsing (
  -- $receiving
  satisfy
  , satisfyMaybe
  , message
  , response
  , responseForId
  , customRequest
  , customNotification
  , anyRequest
  , anyResponse
  , anyNotification
  , anyMessage
  , loggingNotification
  , publishDiagnosticsNotification
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.Text as T
import Data.Typeable
import Language.LSP.Test.Session
import Language.LSP.Types
import UnliftIO.Concurrent

-- $receiving
-- To receive a message, specify the method of the message to expect:
--
-- @
-- msg1 <- message SWorkspaceApplyEdit
-- msg2 <- message STextDocumentHover
-- @
--
-- 'Language.LSP.Test.Session' is actually just a parser
-- that operates on messages under the hood. This means that you
-- can create and combine parsers to match specific sequences of
-- messages that you expect.
--
-- For example, if you wanted to match either a definition or
-- references request:
--
-- > defOrImpl = message STextDocumentDefinition
-- >          <|> message STextDocumentReferences
--
-- If you wanted to match any number of telemetry
-- notifications immediately followed by a response:
--
-- @
-- logThenDiags =
--  skipManyTill (message STelemetryEvent)
--               anyResponse
-- @

-- | Consumes and returns the next message, if it satisfies the specified predicate.
--
-- @since 0.5.2.0
satisfy :: (MonadLoggerIO m, MonadUnliftIO m) => (FromServerMessage -> Bool) -> Session m FromServerMessage
satisfy pred = satisfyMaybe (\msg -> if pred msg then Just msg else Nothing)

-- | Consumes and returns the result of the specified predicate if it returns `Just`.
--
-- @since 0.6.1.0
satisfyMaybe :: (MonadLoggerIO m, MonadUnliftIO m) => (FromServerMessage -> Maybe a) -> Session m a
satisfyMaybe pred = satisfyMaybeM (pure . pred)

satisfyMaybeM :: (MonadLoggerIO m, MonadUnliftIO m) => (FromServerMessage -> Session m (Maybe a)) -> Session m a
satisfyMaybeM pred = do
  skipTimeout <- overridingTimeout <$> (asks sessionState >>= readMVar)
  timeoutId <- getCurTimeoutId
  mtid <-
    if skipTimeout
    then pure Nothing
    else Just <$> do
      chan <- asks messageChan
      timeout <- asks (messageTimeout . config)
      liftIO $ forkIOWithUnmask $ \unmask -> unmask $ do
        threadDelay (timeout * 1000000)
        writeChan chan (TimeoutMessage timeoutId)

  x <- undefined -- Session await

  forM_ mtid $ \tid -> do
    bumpTimeoutId timeoutId
    liftIO $ killThread tid

  ss <- asks sessionState
  modifyMVar_ ss $ \s -> pure $ s { lastReceivedMessage = Just x }

  res <- pred x

  case res of
    Just a -> do
      logMsg LogServer x
      return a
    Nothing -> undefined -- empty

-- | Matches a request or a notification coming from the server.
-- Doesn't match Custom Messages
message :: (MonadLoggerIO n, MonadUnliftIO n) => SServerMethod m -> Session n (ServerMessage m)
message (SCustomMethod _) = error "message can't be used with CustomMethod, use customRequest or customNotification instead"
message m1 = satisfyMaybe $ \case
  FromServerMess m2 msg -> do
    res <- mEqServer m1 m2
    case res of
      Right HRefl -> pure msg
      Left _f -> Nothing
  _ -> Nothing

customRequest :: (MonadLoggerIO m, MonadUnliftIO m) => T.Text -> Session m (ServerMessage (CustomMethod :: Method FromServer Request))
customRequest m = satisfyMaybe $ \case
  FromServerMess m1 msg -> case splitServerMethod m1 of
    IsServerEither -> case msg of
      ReqMess _ | m1 == SCustomMethod m -> Just msg
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

customNotification :: (MonadLoggerIO m, MonadUnliftIO m) => T.Text -> Session m (ServerMessage (CustomMethod :: Method FromServer Notification))
customNotification m = satisfyMaybe $ \case
  FromServerMess m1 msg -> case splitServerMethod m1 of
    IsServerEither -> case msg of
      NotMess _ | m1 == SCustomMethod m -> Just msg
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

-- | Matches if the message is a notification.
anyNotification :: (MonadLoggerIO m, MonadUnliftIO m) => Session m FromServerMessage
anyNotification = satisfy $ \case
  FromServerMess m msg -> case splitServerMethod m of
    IsServerNot -> True
    IsServerEither -> case msg of
      NotMess _ -> True
      _ -> False
    _ -> False
  FromServerRsp _ _ -> False

-- | Matches if the message is a request.
anyRequest :: (MonadLoggerIO m, MonadUnliftIO m) => Session m FromServerMessage
anyRequest = satisfy $ \case
  FromServerMess m _ -> case splitServerMethod m of
    IsServerReq -> True
    _ -> False
  FromServerRsp _ _ -> False

-- | Matches if the message is a response.
anyResponse :: (MonadLoggerIO m, MonadUnliftIO m) => Session m FromServerMessage
anyResponse = satisfy $ \case
  FromServerMess _ _ -> False
  FromServerRsp _ _ -> True

-- | Matches a response coming from the server.
response :: (MonadLoggerIO n, MonadUnliftIO n) => SMethod (m :: Method FromClient Request) -> Session n (ResponseMessage m)
response m1 = satisfyMaybe $ \case
  FromServerRsp m2 msg -> do
    HRefl <- runEq mEqClient m1 m2
    pure msg
  _ -> Nothing

-- | Like 'response', but matches a response for a specific id.
responseForId :: (MonadLoggerIO n, MonadUnliftIO n) => SMethod (m :: Method FromClient Request) -> LspId m -> Session n (ResponseMessage m)
responseForId m lid = do
  satisfyMaybe $ \msg -> do
    case msg of
      FromServerMess _ _ -> Nothing
      FromServerRsp m' rspMsg@(ResponseMessage _ lid' _) -> do
        HRefl <- runEq mEqClient m m'
        guard (Just lid == lid')
        pure rspMsg

-- | Matches any type of message.
anyMessage :: (MonadLoggerIO m, MonadUnliftIO m) => Session m FromServerMessage
anyMessage = satisfy (const True)

-- | Matches if the message is a log message notification or a show message notification/request.
loggingNotification :: (MonadLoggerIO m, MonadUnliftIO m) => Session m FromServerMessage
loggingNotification = satisfy shouldSkip
  where
    shouldSkip (FromServerMess SWindowLogMessage _) = True
    shouldSkip (FromServerMess SWindowShowMessage _) = True
    shouldSkip (FromServerMess SWindowShowMessageRequest _) = True
    shouldSkip (FromServerMess SWindowShowDocument _) = True
    shouldSkip _ = False

isLogNotification (ServerMessage (FromServerMess SWindowLogMessage _)) = True
isLogNotification (ServerMessage (FromServerMess SWindowShowMessage _)) = True
isLogNotification (ServerMessage (FromServerMess SWindowShowMessageRequest _)) = True
isLogNotification (ServerMessage (FromServerMess SWindowShowDocument _)) = True
isLogNotification _ = False

-- | Matches a 'Language.LSP.Types.TextDocumentPublishDiagnostics'
-- (textDocument/publishDiagnostics) notification.
publishDiagnosticsNotification :: (MonadLoggerIO m, MonadUnliftIO m) => Session m (Message TextDocumentPublishDiagnostics)
publishDiagnosticsNotification =
  satisfyMaybe $ \msg -> case msg of
    FromServerMess STextDocumentPublishDiagnostics diags -> Just diags
    _ -> Nothing
