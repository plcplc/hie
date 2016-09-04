{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Hie.Ui.DownloadLink where

import Data.Comp
import Data.Comp.Show
import Data.Dynamic.PolyDyn
import Hie.Ui.Types
import Reflex.Dom
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as BS64
import qualified Data.Map as M

data UiDownloadLink e = UiDownloadLink
  deriving (Eq, Functor, Foldable, Traversable)

instance EqF UiDownloadLink where
  eqF _ _ = True

instance ShowF UiDownloadLink where
  showF _ = "UiDownloadLink"

instance UiSelectable UiDownloadLink where

  uiIdentifier _ = "DownloadLink"
  enumerateUi = [UiDownloadLink]

uiDownloadLink :: (UiDownloadLink :<: uidomain) => Term uidomain
uiDownloadLink = Term $ inj UiDownloadLink

uiDownloadLinkImpl ::
  forall uidomain t m.
  (
    MonadWidget t m,
    UiDownloadLink :<: uidomain
  ) =>
  UiImpl uidomain t m
uiDownloadLinkImpl = UiImpl go
  where
    go :: UiImplK t uidomain m UiDownloadLink BS.ByteString
    go _ _ _ x = do
      attrsDyn <- mapDyn (hrefAttr . unMatch) x
      elDynAttr "a" attrsDyn $ do
        text "download"
      return (noAction, never)

    hrefAttr :: BS.ByteString -> M.Map String String
    hrefAttr = M.singleton "href" . ("data:application/octet-stream;base64,"++) . BS8.unpack . BS64.encode
