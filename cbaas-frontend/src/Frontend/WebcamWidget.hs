{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.WebcamWidget where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.Foldable
import qualified Data.JSString as JS
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import GHCJS.DOM
import GHCJS.DOM.Window
import GHCJS.DOM.Navigator
import Reflex.Dom
import GHCJS.DOM.Document
import GHCJS.DOM.Types
import GHCJS.DOM.HTMLVideoElement
import GHCJS.DOM.MediaStream
import GHCJS.DOM.URL
import GHCJS.DOM.URLUtils
import GHCJS.Marshal
import GHCJS.Types (jsval)
#ifdef ghcjs_HOST_OS
import qualified JavaScript.Object as O
#endif

webcamWidget :: MonadWidget t m => m ()
webcamWidget = mdo
  pb <- getPostBuild
  doc <- askDocument

  vidAttrs <- holdDyn Nothing streamUrl >>= mapDyn
    (\u -> "autoplay" =: "true" <> maybe mempty ("src" =:) u)

  vid <- fst <$> elDynAttr' "video" vidAttrs (return ())
  streamUrl <- performEvent $ ffor pb $ \() -> liftIO $ do
    w' <- liftIO currentWindow
    case w' of
      Nothing -> Prelude.print "window error" >> Prelude.error "window error"
      Just win -> do
        n' <- liftIO $ getNavigator win
        case n' of
          Just nav -> do
            let htmlVid  = castToHTMLVideoElement (_el_element vid)
            dict <- Dictionary <$> toJSVal_aeson (A.object [T.pack "video" A..= "true"])
            stream <- getUserMedia nav (Just dict)
            createObjectURLStream' (Just stream) -- TODO: How to get at global URL object?

  performEvent_ $ (liftIO $ Prelude.print "pb") <$ pb
  return ()

foreign import javascript unsafe "console.log(Math.pow(10,2));" mytest :: IO ()

dictionaryFromMap :: Map.Map String String -> IO Dictionary
dictionaryFromMap m = do
  dict <- O.create
  sequence_ $ flip Map.mapWithKey m $ \k v -> do
    vj <- toJSVal (JS.pack k)
    O.setProp (JS.pack k) vj dict
  let jv = jsval dict
  fromJSValUnchecked jv
  -- fmap Dictionary $ toJSVal dict

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
