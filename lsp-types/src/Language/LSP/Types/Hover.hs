{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
module Language.LSP.Types.Hover where

import           Data.Aeson.TH
import           Data.Text                      ( Text )

import           Language.LSP.Types.Common
import           Language.LSP.Types.Location
import           Language.LSP.Types.MarkupContent
import           Language.LSP.Types.Progress
import           Language.LSP.Types.TextDocument
import           Language.LSP.Types.Utils


-- -------------------------------------

data HoverClientCapabilities =
  HoverClientCapabilities
    { _dynamicRegistration :: Maybe Bool
    , _contentFormat :: Maybe (List MarkupKind)
    } deriving (Show, Read, Eq)
deriveJSON lspOptions ''HoverClientCapabilities

makeExtendingDatatype "HoverOptions" [''WorkDoneProgressOptions] []
deriveJSON lspOptions ''HoverOptions

makeExtendingDatatype "HoverRegistrationOptions" [''TextDocumentRegistrationOptions, ''HoverOptions] []
deriveJSON lspOptions ''HoverRegistrationOptions

makeExtendingDatatype "HoverParams" [''TextDocumentPositionParams, ''WorkDoneProgressParams] []
deriveJSON lspOptions ''HoverParams

-- -------------------------------------

data LanguageString =
  LanguageString
    { _language :: Text
    , _value    :: Text
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''LanguageString

{-# DEPRECATED MarkedString, PlainString, CodeString "Use MarkupContent instead, since 3.3.0 (11/24/2017)" #-}
data MarkedString =
    PlainString Text
  | CodeString LanguageString
    deriving (Eq,Read,Show)

deriveJSON lspOptionsUntagged ''MarkedString

-- -------------------------------------

data HoverContents =
    HoverContentsMS (MarkedString |? List MarkedString)
  | HoverContents   MarkupContent
  deriving (Read,Show,Eq)

deriveJSON lspOptionsUntagged ''HoverContents

-- -------------------------------------

instance Semigroup HoverContents where
  h1 <> h2 = HoverContents (convert h1 <> convert h2)
    where
      convert :: HoverContents -> MarkupContent
      convert (HoverContentsMS (InL ms)) = toMarkupContent ms
      convert (HoverContentsMS (InR (List mss))) = mconcat $ fmap toMarkupContent mss
      convert (HoverContents mc) = mc

instance Monoid HoverContents where
  mempty = HoverContentsMS (InR (List []))

toMarkupContent :: MarkedString -> MarkupContent
toMarkupContent (PlainString s) = unmarkedUpContent s
toMarkupContent (CodeString (LanguageString lang s)) = markedUpContent lang s

-- -------------------------------------

data Hover =
  Hover
    { _contents :: HoverContents
    , _range    :: Maybe Range
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''Hover
