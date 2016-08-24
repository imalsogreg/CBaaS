{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Frontend.ImageWidget where

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
  def = ImageInputWidgetConfig DrawSource never (640,480)

data ImageInputWidget t = ImageInputWidget
  { imageInputWidget_image         :: Dynamic t Img
  , imageInputWidget_screenScaling :: Dynamic t Double
  }

imageInputWidget :: forall t m.(MonadWidget t m, PerformEvent t (Performable m))
                 => Document
                 -> ImageInputWidgetConfig t
                 -> m (ImageInputWidget t)
imageInputWidget doc (ImageInputWidgetConfig src0 dSrc (wid,hei)) = do
  elAttr "div" ("class" =: "image-input" <>
                "style" =: ("width:"  <> tShow wid <> "px; height:" <> tShow hei <> "px; left:200px;")) $ mdo
    canv <- drawingArea okToDraw (() <$ afterCanvasActions) never def
    (canvasActions, imgSrc) <- divClass "input-bar" $ do
      imgSrcSet <- divClass "input-select" $ do
        fs     <- fmap (FileSource   <$ ) $ iconButton "file"
        wc     <- fmap (WebcamSource <$ ) $ iconButton "camera"
        dr     <- fmap (DrawSource   <$ ) $ iconButton "pencil"
        holdDyn src0 $ leftmost [dSrc, fs, wc, dr]
      canvasEvents <- dyn $ ffor imgSrcSet $ \case
        WebcamSource -> do
          b  <- iconButton "snap"
          wc <- webcamWidget doc (constDyn mempty)
          -- let snapPic = tagDyn ()
          return $ ffor b $ \() _ ctx -> do
            drawImageFromVideo ctx (Just (castToHTMLVideoElement $ _element_raw  wc)) (100 :: Float) (100 :: Float)
        FileSource -> text "TODO" >> return never
        DrawSource -> text "TODO" >> return never
        NoSource   -> text "TODO" >> return never
      return (canvasEvents, imgSrcSet) -- TODO is this return value needed
    afterCanvasActions <- delay 0 flatActions
    let okToDraw = fmap (== DrawSource) imgSrc
    flatActions :: Event t CanvasAction <- fmap switchPromptlyDyn $ holdDyn never canvasActions
    let canvEl = castToHTMLCanvasElement $ _element_raw $ _drawingArea_el canv
    Just ctx :: Maybe CanvasRenderingContext2D <- liftIO $ fromJSVal =<< getContext canvEl ("2d" :: JSString)
    performEvent_ $ (fmap (\a -> liftIO $ a canvEl ctx) flatActions)
    blank
  return $ ImageInputWidget undefined undefined

type CanvasAction = HTMLCanvasElement -> CanvasRenderingContext2D -> IO ()

drawingElements :: MonadWidget t m => El t -> CanvasRenderingContext2D -> m (Event t CanvasAction)
drawingElements target ctx = do
  penIsDown <- holdDyn False $ leftmost [False <$ domEvent Mouseup target
                                        ,True <$ domEvent Mousedown target]
  color  <- updated . value <$> textInput def -- TODO actual color picker
  performEvent_ $ ffor color (\c -> setFillStyle ctx (Just $ CanvasStyle $ jsval ((jsPack $ T.unpack c :: JSString))))
  -- performEvent_ $ ffor color $ \c -> setFillStyle ctx (Just $ jsval ("rgb(0,0,0)" :: JSString))
  penWid <- (fmap (fromMaybe (3 :: Int) . readMaybe . T.unpack) . value) <$> textInput def --TODO: actual width picker
  undefined -- TODO this should be the touch-enabled drawing area

#ifdef ghcjs_HOST_OS
jsPack = Data.JSString.pack
#else
jsPack = undefined
#endif

-- TODO implement for real
iconButton :: (MonadWidget t m) => T.Text -> m (Event t ())
iconButton iconName = do
  (d,_) <- elAttr' "div" ("class" =: "icon-button") $ text iconName
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


-------------------------------------------------------------------------------
fileImageLoader :: forall t m .MonadWidget t m
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
#endif
