module Language.LSP.Test.Server (withServer) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.Text as T
import Language.LSP.Test.Compat
import System.IO
import System.Process hiding (withCreateProcess)
import UnliftIO.Async


withServer :: (
  MonadLoggerIO m, MonadUnliftIO m
  ) => String -> Bool -> (CreateProcess -> CreateProcess) -> (Handle -> Handle -> ProcessHandle -> m a) -> m a
withServer serverExe logStdErr modifyCreateProcess f = do
  -- TODO Probably should just change runServer to accept
  -- separate command and arguments
  let cmd:args = words serverExe
  let createProc = (proc cmd args) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

  withRunInIO $ \runInIO ->
    withCreateProcess (modifyCreateProcess createProc) $ \(Just serverIn) (Just serverOut) (Just serverErr) serverProc -> do
      liftIO $ hSetBuffering serverIn NoBuffering
      liftIO $ hSetBuffering serverOut NoBuffering

      -- Need to continuously consume stderr else it gets blocked
      -- Can't pass NoStream either to std_err
      liftIO $ hSetBuffering serverErr NoBuffering
      liftIO $ hSetBinaryMode serverErr True
      let errSinkThread = forever $ liftIO (hGetLine serverErr) >>= when logStdErr . logDebugN . T.pack
      runInIO $ withAsync errSinkThread $ \_ ->
        f serverIn serverOut serverProc
