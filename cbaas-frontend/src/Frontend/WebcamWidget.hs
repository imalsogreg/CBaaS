{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend.WebcamWidget where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Ref (Ref)
import qualified Data.Aeson as A
import Data.Foldable
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import GHCJS.DOM
import GHCJS.DOM.Window
import GHCJS.DOM.Navigator
import Reflex.Dom
import GHC.IORef (IORef)
import GHCJS.DOM.Document
import GHCJS.DOM.Types
import GHCJS.DOM.HTMLVideoElement
import GHCJS.DOM.MediaStream
import GHCJS.DOM.URL
import GHCJS.DOM.URLUtils
#ifdef ghcjs_HOST_OS
import GHCJS.Marshal
import qualified Data.JSString as JS
import GHCJS.Types (jsval)
import qualified JavaScript.Object as O
#endif

webcamWidget :: ( DomBuilder t m
                , DomBuilderSpace m ~ GhcjsDomSpace
                , MonadFix m
                , MonadHold t m
                , PostBuild t m
                , MonadIO (Performable m)
                , PerformEvent t (Performable m)
                , PerformEvent t m
                )
              => Document -> Dynamic t (Map.Map T.Text T.Text) -> m (El t)
webcamWidget doc extraVideoAttrs = mdo
#ifdef ghcjs_HOST_OS
  pb <- getPostBuild

  vidAttrs <- holdDyn Nothing streamUrl >>= mapDyn
    (\u -> "autoplay" =: "true" <> maybe mempty ("src" =:) u)

  vid <- fst <$> elDynAttr' "video" vidAttrs (return ())
  streamUrl <- performEvent $ ffor pb $ \() -> liftIO $ do
    Just win <- currentWindow
    Just nav <- getNavigator win
    uAgent   <- getUserAgent nav
    let htmlVid  = castToHTMLVideoElement (_el_element vid)
    dict <- Dictionary <$> toJSVal_aeson (A.object [T.pack "video" A..= ("true" :: String)])
    if "Chrome" `JS.isInfixOf` uAgent
              then getUserMedia nav (Just dict) >>= \s -> createObjectURLStream' (Just s)
              else fmap (Just . T.pack . JS.unpack) js_mozGetUserMedia
  return vid
#else
    Prelude.error "Only supported by ghcjs"
#endif


#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "console.log(Math.pow(10,2));" mytest :: IO ()
dictionaryFromMap :: Map.Map String String -> IO Dictionary
dictionaryFromMap m = do
  dict <- O.create
  sequence_ $ flip Map.mapWithKey m $ \k v -> do
    vj <- toJSVal (JS.pack k)
    O.setProp (JS.pack k) vj dict
  let jv = jsval dict
  fromJSValUnchecked jv

foreign import javascript interruptible  "navigator.mediaDevices.getUserMedia({'video':true}).then(function(stream){ var u = window.URL.createObjectURL(stream); $c(u); });"
  js_mozGetUserMedia :: IO JS.JSString

foreign import javascript unsafe "URL[\"createObjectURL\"]($1)"
        js_createObjectURLStream' ::
        Nullable MediaStream -> IO (Nullable JS.JSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/URL.createObjectURL Mozilla URL.createObjectURL documentation>
createObjectURLStream' ::
                      (FromJSString result) =>
                        Maybe MediaStream -> IO (Maybe result)
createObjectURLStream' stream
  = (fromMaybeJSString <$>
         (js_createObjectURLStream' (maybeToNullable stream)))
#endif
