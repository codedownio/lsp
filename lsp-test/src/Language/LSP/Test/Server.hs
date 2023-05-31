
module Language.LSP.Test.Server (withServer) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.Text as T
import System.IO
import UnliftIO.Async
import UnliftIO.Process


withServer :: (
  MonadLoggerIO m, MonadUnliftIO m
  ) => CreateProcess -> Bool -> (CreateProcess -> CreateProcess) -> (Handle -> Handle -> ProcessHandle -> m a) -> m a
withServer baseCreateProcess logStdErr modifyCreateProcess f = do
  let createProc = baseCreateProcess {
        std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , create_group = True
        }

  withCreateProcess (modifyCreateProcess createProc) $ \(Just serverIn) (Just serverOut) (Just serverErr) serverProc -> do
    -- Need to continuously consume stderr else it gets blocked
    -- Can't pass NoStream either to std_err
    liftIO $ hSetBuffering serverErr NoBuffering
    liftIO $ hSetBinaryMode serverErr True
    let errSinkThread = forever $ liftIO (hGetLine serverErr) >>= when logStdErr . logDebugN . T.pack
    withAsync errSinkThread $ \_ -> do
      f serverIn serverOut serverProc
