{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.ImageWidget where

import Data.Monoid
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
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
import GHCJS.DOM.Types (HTMLInputElement, HTMLImageElement, IsGObject)
import GHCJS.DOM.FileReader
import GHCJS.DOM.File
import GHCJS.DOM.FileList
#ifdef ghcjs_HOST_OS
import GHCJS.Marshal
#endif

import Reflex.Dom hiding (EventName)

-------------------------------------------------------------------------------
type Img = JP.Image JP.PixelRGBA8

-------------------------------------------------------------------------------
data ImgSource = ImgUrl | FileUpload | Webcam
  deriving (Eq, Show)

-------------------------------------------------------------------------------
data ImageWidgetConfig t = ImageWidgetConfig
  { imageWidgetConfig_initialImage  :: Img
  , imageWidgetConfig_setImage      :: Event t Img
  , imageWidgetConfig_setBaseImage  :: Event t Img
  , imageWidgetConfig_initialSource :: ImgSource
  , imageWidgetConfig_setSource     :: Event t ImgSource
  }

-------------------------------------------------------------------------------
instance Reflex t => Default (ImageWidgetConfig t) where
  def = ImageWidgetConfig defImg never never ImgUrl never

-------------------------------------------------------------------------------
data ImageWidget t = ImageWidget
  { imageWidget_image     :: Dynamic t Img
  , imageWidget_baseImage :: Dynamic t Img
  }

-------------------------------------------------------------------------------
defImg :: Img
defImg = JP.generateImage (\_ _ -> JP.PixelRGBA8 0 0 1 1) 10 10


-------------------------------------------------------------------------------
fileImageLoader :: forall t m .MonadWidget t m
                => m (Event t (Either String Img, T.Text))
fileImageLoader = do
  fls :: Event t File <- (fmapMaybe viewSingleton . updated . value) <$> fileInput def

  #ifdef ghcjs_HOST_OS

  reader <- liftIO $ newFileReader
  performEvent_ $ ffor fls $ \f -> liftIO $ do
    print "READ"
    readAsDataURL reader (Just f)
  imgs <- wrapDomEvent reader (`on` load) $ do
    liftIO $ print "LOAD"
    res :: T.Text <- liftIO $ fromJSValUnchecked =<< getResult reader
    liftIO $ print res
    let img = fmap JP.convertRGBA8 (JP.decodeImage =<< (B64.decode . T.encodeUtf8 . snd . T.breakOnEnd (T.pack "base64,") $ res))
    liftIO . print $ case img of
      Right i -> "RIGHT"
      Left  e -> e
    return (img,res)

  #else

  -- let x = GHCJS.DOM.File.getName undefined :: IO Int
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

-------------------------------------------------------------------------------
imageWidget :: MonadWidget t m => ImageWidgetConfig t -> m (ImageWidget t)
imageWidget (ImageWidgetConfig img0 dImg base0 dBase iSrcType) =
  divClass "image-widget" $ mdo
    baseImg <- holdDyn undefined undefined
    undefined
    undefined

-------------------------------------------------------------------------------
displayImg :: MonadWidget t m => Dynamic t T.Text -> m ()
displayImg dImgUrl = do
  pb <- getPostBuild

  imgAttrs <- forDyn dImgUrl $ \src -> "src" =: T.unpack src
                                    <> "style" =: "display:none;"

  imgEl   <- fst <$> elDynAttr' "img" imgAttrs (return ())
  let htmlImg = castToHTMLImageElement (_el_element imgEl)

  natSize <- performEvent $ ffor (domEvent Load imgEl) $ \() -> do
    (,) <$> getNaturalWidth htmlImg <*> getNaturalHeight htmlImg

  canv <- fst <$> el' "canvas" (return ())
  let htmlCanv = castToHTMLCanvasElement (_el_element canv)

  performEvent_ $ ffor natSize $ \(w,h) -> liftIO $ do
    print "DRAW"
    GHCJS.DOM.HTMLCanvasElement.setWidth  htmlCanv w
    GHCJS.DOM.HTMLCanvasElement.setHeight htmlCanv h
  #ifdef ghcjs_HOST_OS
    ctx <- fromJSValUnchecked =<< getContext htmlCanv "2d"
  #else
    ctx <- undefined -- TODO - when webkitgtk supports getting 2d context
  #endif
    drawImage ctx (Just htmlImg) 0 0

-------------------------------------------------------------------------------
#ifndef ghcjs_HOST_OS
readAsBinaryString = error "readAsBinaryString only defined in ghcjs"

data JSVal
data FileReader
data CanvasRenderingContext2D
data UIEvent
class IsBlob a

instance IsBlob File

class FromJSVal a

readAsDataURL :: IsBlob blob => FileReader -> Maybe blob -> IO ()
readAsDataURL = error "readAsDataURL is only available to ghcjs"

getContext :: HTMLCanvasElement -> String -> IO JSVal
getContext = undefined

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

fromJSVal = undefined
getResult = undefined
#endif
