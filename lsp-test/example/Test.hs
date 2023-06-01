{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative.Combinators
import Control.Monad.IO.Class
import Control.Monad.Logger
import Language.LSP.Test
import Language.LSP.Types

main = runStderrLoggingT $ runSession undefined fullCaps "test/data/" $ do
  doc <- openDoc "Rename.hs" "haskell"

  -- Use your favourite favourite combinators.
  skipManyTill loggingNotification (count 1 publishDiagnosticsNotification)

  -- Send requests and notifications and receive responses
  rsp <- request STextDocumentDocumentSymbol $
    DocumentSymbolParams Nothing Nothing doc
  liftIO $ print rsp

  -- Or use one of the helper functions
  getDocumentSymbols doc >>= liftIO . print
