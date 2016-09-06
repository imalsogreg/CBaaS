{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Frontend.ImageWidget where

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import Control.Monad.Fix (MonadFix)
import Data.Bool
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Codec.Picture as JP
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Base64.Lazy as BL64
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as BS
import Data.Default
import qualified Data.Text as T
import GHCJS.DOM.CanvasRenderingContext2D
import GHCJS.DOM.EventM
import GHCJS.DOM.HTMLCanvasElement
import GHCJS.DOM.HTMLImageElement
import GHCJS.DOM.Types (Document, HTMLInputElement, HTMLImageElement, IsGObject, castToHTMLVideoElement)
import GHCJS.DOM.FileReader
import GHCJS.DOM.File
import GHCJS.DOM.FileList
import Text.Read
#ifdef ghcjs_HOST_OS
import Data.JSString
import GHCJS.Marshal
import GHCJS.Types (jsval, JSString)
import GHCJS.DOM.Types (CanvasStyle(..))
#endif
import Model
import Frontend.Canvas
import Frontend.WebcamWidget

import Reflex.Dom hiding (EventName)

-------------------------------------------------------------------------------
type Img = JP.Image JP.PixelRGBA8

-------------------------------------------------------------------------------
-- data ImgSource = ImgUrl | FileUpload | Webcam
--   deriving (Eq, Show)

data InputImageSource = FileSource | WebcamSource | DrawSource | NoSource
  deriving (Eq, Show)

data ImageInputWidgetConfig t = ImageInputWidgetConfig
  { imageInputWidgetConfig_initialInputImageSource :: InputImageSource
  , imageInputWidgetConfig_setInputImageSource :: Event t InputImageSource
  , imageInputWidgetConfig_topGeometry :: (Int, Int)
  }

instance Reflex t => Default (ImageInputWidgetConfig t) where
  def = ImageInputWidgetConfig DrawSource never (320,240)

data ImageInputWidget t = ImageInputWidget
  { imageInputWidget_image         :: Dynamic t Img
  , imageInputWidget_screenScaling :: Dynamic t Double
  }

imageInputWidget :: forall t m.(PostBuild t m,
                                DomBuilder t m,
                                MonadIO m,
                                MonadFix m,
                                MonadIO (Performable m),
                                TriggerEvent t m,
                                PerformEvent t m,
                                HasWebView m,
                                MonadHold t m,
                                HasWebView (Performable m),
                                PerformEvent t (Performable m),
                                DomBuilderSpace m ~ GhcjsDomSpace)
                 => Document
                 -> ImageInputWidgetConfig t
                 -> m (ImageInputWidget t)
imageInputWidget doc (ImageInputWidgetConfig src0 dSrc (wid,hei)) = do
  canv <- elAttr "div" ("class" =: "image-input" <> "style" =: ("width: " <> tShow wid <> "px;" <> "height:" <> tShow hei <> "px;")) $ mdo


    canv <- divClass "canvas-container" $ elAttr "div" ("class" =: "canvas-area" <> "style" =: "position:relative") $ mdo
      canv <- drawingArea okToDraw (() <$ cameraActions) never def {_drawingAreaConfig_geom = (wid,hei)}
      clicks :: Event t (Event t ()) <- dyn $ ffor (fmap (== WebcamSource) imgSrc) $ \b ->
        if b
        then elAttr "div" ("class" =: "camera-area" <>
                           "style" =: "position:absolute; top: 10px; left: 10px;") $ do
              elAttr "div" ("class" =: "camera-area-contents" <> "style" =: "position:relative;") $ do
                wc <- webcamWidget doc (constDyn $ "style" =: ("width: " <> tShow (wid `div` 5) <>
                                                               "px; " <>
                                                               "position: absolute; left:0px; top: 0px;"))
                b  <- iconButton "mail forward" (constDyn $ "style" =: ("position: absolute; left: 0px; top: 0px;" <>
                                                                        " color: white; text-shadow: 0px 0px 2px black;"))
                performEvent_ $ ffor b $ \() -> do
                  liftIO $ drawImageFromVideoScaled ctx (Just (castToHTMLVideoElement $ _element_raw  wc)) (0 :: Float) (0 :: Float) (320 :: Float) (240 :: Float)
                return b
        else return never
      clicks' <- holdDyn never clicks
      cameraActions <- delay 0 $ switchPromptlyDyn clicks'
      return canv


    (canvasActions, imgSrc) <- divClass "input-bar" $ mdo

      let barAttrs = ffor visible $ \b -> ("class" =: "input-select ui secondary pointing menu input-bar-items") <> bool ("style" =: "display:none;") mempty b
      (imgSrcSet, canvasActions) <- elDynAttr "div" barAttrs  $ do
         let itemClass target = fmap (("class" =:) . bool "item" "item active" . (==target)) imgSrc
         fs     <- fmap (FileSource   <$ ) $ iconButton "file" (itemClass FileSource)
         wc     <- fmap (WebcamSource <$ ) $ iconButton "photo" (itemClass WebcamSource)
         dr     <- fmap (DrawSource   <$ ) $ iconButton "write" (itemClass DrawSource)
         imgSrc <- holdDyn src0 $ leftmost [dSrc, fs, wc, dr]
         canvasEvents <- dyn $ ffor imgSrc $ \case
           WebcamSource -> blank
           FileSource -> text "TODO"
           DrawSource -> drawingElements ctx
           NoSource   -> blank
         return (imgSrc, canvasEvents)

      visible :: Dynamic t Bool <- toggle True =<< (domEvent Click) . fst <$> elAttr' "dyn" ("class" =: "input-bar-hideaway") (dynText (bool "◀" "▶" <$> visible))

      -- return canv

      return (canvasActions, imgSrcSet) -- TODO is this return value needed

    -- afterCanvasActions <- delay 0 flatActions
    let okToDraw = fmap (== DrawSource) imgSrc
    -- flatActions :: Event t CanvasAction <- fmap switchPromptlyDyn $ holdDyn never canvasActions
    let canvEl = castToHTMLCanvasElement $ _element_raw $ _drawingArea_el canv
    Just ctx :: Maybe CanvasRenderingContext2D <- liftIO $ fromJSVal =<< getContext canvEl ("2d" :: JSString)
    -- performEvent_ $ (fmap (\a -> liftIO $ a canvEl ctx) flatActions)
    return canv
  img <- holdDyn defImg =<< performEvent (ffor (updated $ _drawingArea_image canv) $ \_ ->
                                             liftIO $ canvasGetImg (castToHTMLCanvasElement $ _element_raw $ _drawingArea_el canv))
  return $ ImageInputWidget img undefined

-- type CanvasAction = HTMLCanvasElement -> CanvasRenderingContext2D -> IO ()

drawingElements :: (PostBuild t m,
                    DomBuilder t m,
                                MonadIO m,
                                MonadFix m,
                                MonadIO (Performable m),
                                TriggerEvent t m,
                                PerformEvent t m,
                                HasWebView m,
                                MonadHold t m,
                                HasWebView (Performable m),
                                PerformEvent t (Performable m),
                                DomBuilderSpace m ~ GhcjsDomSpace) => CanvasRenderingContext2D -> m ()
drawingElements ctx = do
  color  <- updated . value <$> textInput def { _textInputConfig_inputType = "color"}-- TODO actual color picker
  performEvent_ $ ffor color (\c -> setStrokeStyle ctx (Just $ CanvasStyle $ jsval ((jsPack $ T.unpack c :: JSString))))
  penWid <- (fmap (fromMaybe (3 :: Float) . readMaybe . T.unpack) . value) <$> textInput def { _textInputConfig_attributes = constDyn ("style" =: "width: 50px;")}--TODO: actual width picker
  performEvent_ $ ffor (updated penWid) (\c -> setLineWidth ctx c)
  blank

#ifdef ghcjs_HOST_OS
jsPack = Data.JSString.pack
#else
jsPack = undefined
#endif

-- TODO implement for real
iconButton :: (PostBuild t m,
               DomBuilder t m,
                                MonadIO m,
                                MonadFix m,
                                MonadIO (Performable m),
                                TriggerEvent t m,
                                PerformEvent t m,
                                HasWebView m,
                                MonadHold t m,
                                HasWebView (Performable m),
                                PerformEvent t (Performable m),
                                DomBuilderSpace m ~ GhcjsDomSpace) => T.Text -> Dynamic t (Map.Map T.Text T.Text) -> m (Event t ())
iconButton iconName topAttrs = do
  (d,_) <- elDynAttr' "div" topAttrs $ elAttr "i" ("class" =: (iconName <> " icon")) blank
  return (domEvent Click d)

-- -------------------------------------------------------------------------------
-- data ImageWidgetConfig t = ImageWidgetConfig
--   { imageWidgetConfig_initialImage  :: Img
--   , imageWidgetConfig_setImage      :: Event t Img
--   , imageWidgetConfig_setBaseImage  :: Event t Img
--   , imageWidgetConfig_initialSource :: ImgSource
--   , imageWidgetConfig_setSource     :: Event t ImgSource
--   }

-- -------------------------------------------------------------------------------
-- instance Reflex t => Default (ImageWidgetConfig t) where
--   def = ImageWidgetConfig defImg never never ImgUrl never

-- -------------------------------------------------------------------------------
-- data ImageWidget t = ImageWidget
--   { imageWidget_image     :: Dynamic t Img
--   , imageWidget_baseImage :: Dynamic t Img
--   }

-------------------------------------------------------------------------------
defImg :: Img
defImg = JP.generateImage (\_ _ -> JP.PixelRGBA8 0 0 1 1) 10 10

-- imagedataToValue :: ImageData -> IO Img
-- imagedataToValue d = do
--   bytes <- imagedataGetData 
--   let Right imgBytes = (B64.decode . T.encodeUtf8 . snd $ T.breakOnEnd "base64," d)
--       Right img = (JP.decodeImage imgBytes)
--   return . ELit TModelImage . VImage . ModelImage $ JP.convertRGBA8 img

-- TODO: Move some of this logic into Model (or a new ModelImage module)
canvasGetImg :: HTMLCanvasElement -> IO Img
canvasGetImg canv = do
  putStrLn "CANVAS GET IMG"
  d <- toDataURL canv (Just "image/jpeg" :: Maybe String)
  let Right imgBytes = (B64.decode . T.encodeUtf8 . snd $ T.breakOnEnd "base64," d)
      Right img = (JP.decodeImage imgBytes)
  return $ JP.convertRGBA8 img

-- #ifdef ghcjs_HOST_OS
-- foreign import javascript unsafe "($1).data;"
--   js_imagedataGetData :: ImageData -> UInt8ClampedArray
-- #endif

-------------------------------------------------------------------------------
fileImageLoader :: forall t m .(PostBuild t m,
                                DomBuilder t m,
                                MonadIO m,
                                MonadFix m,
                                MonadIO (Performable m),
                                TriggerEvent t m,
                                PerformEvent t m,
                                HasWebView m,
                                MonadHold t m,
                                HasWebView (Performable m),
                                PerformEvent t (Performable m),
                                DomBuilderSpace m ~ GhcjsDomSpace)
                => m (Event t (Either String Img, T.Text))
fileImageLoader = do
  fls :: Event t File <- (fmapMaybe viewSingleton . updated . value) <$>
                         fileInput def

#ifdef ghcjs_HOST_OS

  reader <- liftIO $ newFileReader
  performEvent_ $ ffor fls $ \f -> liftIO $ do
    readAsDataURL reader (Just f)
  imgs <- wrapDomEvent reader (`on` load) $ do
    res :: T.Text <- liftIO $ fromJSValUnchecked =<< getResult reader
    let img = fmap JP.convertRGBA8 (JP.decodeImage =<<
                                    (B64.decode .
                                     T.encodeUtf8 .
                                     snd . T.breakOnEnd (T.pack "base64,") $
                                     res))
    return (img,res)

#else

  -- TODO: Implement file reading in webkitgtk case
  imgs <- performEvent $ ffor fls $ \f -> liftIO $ do
    fname :: String <- GHCJS.DOM.File.getName f
    print fname
    undefined

#endif

  return imgs

-------------------------------------------------------------------------------
viewSingleton :: [a] -> Maybe a
viewSingleton    [x] =  Just x
viewSingleton     _  =  Nothing

-- -------------------------------------------------------------------------------
-- -- imageWidget :: MonadWidget t m => ImageWidgetConfig t -> m (ImageWidget t)
-- imageWidget (ImageWidgetConfig img0 dImg base0 dBase iSrcType) =
--   divClass "image-widget" $ mdo
--     baseImg <- holdDyn undefined undefined
--     undefined
--     undefined

displayImg' :: (DomBuilderSpace m ~ GhcjsDomSpace,
                HasDomEvent t (Element EventResult GhcjsDomSpace t) 'LoadTag,
                MonadIO (Performable m),
                DomBuilder t m,
                PostBuild t m,
                PerformEvent t m) => ModelImage -> m () 
displayImg' (ModelImage jp) = do
  let dataUrl = ("data:image/jpeg;base64," <>) . T.decodeUtf8 . B64.encode . BSL.toStrict . JP.encodeJpegAtQuality 100 . JP.convertImage . JP.pixelMap JP.dropTransparency $ jp
  elAttr "img" ("src" =: dataUrl) blank

-------------------------------------------------------------------------------
-- displayImg :: MonadWidget t m => Dynamic t T.Text -> m ()
displayImg ::(DomBuilderSpace m ~ GhcjsDomSpace,
                         HasDomEvent t (Element EventResult GhcjsDomSpace t) 'LoadTag,
                         MonadIO (Performable m),
                         DomBuilder t m,
                         PostBuild t m,
                         PerformEvent t m) => Dynamic t T.Text -> m ()
displayImg dImgUrl = do
  pb <- getPostBuild

  imgEl   <- fst <$> elDynAttr' "img" (ffor dImgUrl (\src -> "src" =: src
                                    <> "style" =: "display:none;")) (return ())
  let htmlImg = castToHTMLImageElement (_element_raw imgEl)

  natSize <- performEvent $ ffor (domEvent Load imgEl) $ \() -> do
    (,) <$> getNaturalWidth htmlImg <*> getNaturalHeight htmlImg

  canv <- fst <$> el' "canvas" (return ())
  let htmlCanv = castToHTMLCanvasElement (_element_raw canv)

  performEvent_ $ ffor natSize $ \(w,h) -> liftIO $ do
    -- print "DRAW"
    GHCJS.DOM.HTMLCanvasElement.setWidth  htmlCanv w
    GHCJS.DOM.HTMLCanvasElement.setHeight htmlCanv h
#ifdef ghcjs_HOST_OS
    ctx <- fromJSValUnchecked =<< getContext htmlCanv ("2d" :: String)
#else
    ctx <- undefined -- TODO - when webkitgtk supports getting 2d context
#endif
    drawImage ctx (Just htmlImg) 0 0

-------------------------------------------------------------------------------
#ifndef ghcjs_HOST_OS
readAsBinaryString = error "readAsBinaryString only defined in ghcjs"

data JSVal
data FileReader
-- data CanvasRenderingContext2D
data UIEvent
class IsBlob a

instance IsBlob File

class FromJSVal a


readAsDataURL :: IsBlob blob => FileReader -> Maybe blob -> IO ()
readAsDataURL = error "readAsDataURL is only available to ghcjs"

-- getContext :: HTMLCanvasElement -> String -> IO JSVal
-- getContext = undefined

castToCanvasRenderingContext2D :: IsGObject obj => obj -> CanvasRenderingContext2D
castToCanvasRenderingContext2D = undefined

drawImage :: CanvasRenderingContext2D -> Maybe HTMLImageElement
          -> Float -> Float -> IO ()
drawImage = undefined

fromJSValUnchecked :: FromJSVal a => JSVal -> IO a
fromJSValUnchecked = undefined

newFileReader :: IO FileReader
newFileReader = undefined

load :: EventName FileReader UIEvent
load = undefined

-- fromJSVal = undefined
getResult = undefined

drawImageFromVideo = undefined
drawImageFromVideoScaled = undefined

toDataURL :: HTMLCanvasElement -> Maybe String -> IO T.Text
toDataURL = error "toDataUrl only available in javascript"

#endif
