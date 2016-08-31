{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Frontend.Canvas where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import Data.Bitraversable
import Data.Foldable
import Data.Default
import Data.Maybe
import Data.String (IsString(..))
import Data.Traversable
import Data.Monoid
import qualified Data.Text as T
import Data.Time
import Reflex.Dom hiding (restore, save, preventDefault)
import qualified GHCJS.DOM.MouseEvent as MouseEvent
#ifdef ghcjs_HOST_OS
import GHCJS.DOM.ImageData (newImageData')
import GHCJS.DOM.Touch
import GHCJS.DOM.TouchEvent
import GHCJS.DOM.TouchList
import Data.JSString (JSString, pack)
import           GHCJS.Marshal (fromJSVal)
import GHCJS.Types (jsval)
import           GHCJS.DOM.ClientRect (getTop, getLeft)
import           GHCJS.DOM.Types hiding (Event, Element)
import           GHCJS.DOM.Element (getBoundingClientRect)
#else
import GHCJS.DOM.Types hiding (Event)
#endif
import           GHCJS.DOM.EventM
import qualified Data.Map as Map
import GHCJS.DOM.HTMLCanvasElement
import GHCJS.DOM.CanvasRenderingContext2D
import           GHCJS.DOM.Element (touchStart, mouseDown,
                                    mouseMove, touchEnd, touchMove, mouseUp,focus)

-- data CanvasConfig t = CanvasConfig
--   { canvasConfig_act :: Event t (CanvasElement -> CanvasRenderingContext2D -> IO ())
--   , canvasConfig_geometry :: (Int,Int)
--   }

-- data Canvas t = Canvas
--   { canvas_rawCanvas :: HTMLCanvas
--   }

-- canvas :: MonadWidget t m => CanvasConfig t -> m (Canvas t)
-- canvas (CanvasConfig act (wid, hei)) = do
--   canvEl <- elAttr' "canvas" ("width" =: tShow wid <> "height" =: tShow hei) blank
--   let rawCanvas = castToHTMLCanvasElement (_element_raw canvEl)
--   ctx <- liftIO $ getContext rawCanvas ("2d" :: String)
--   performEvent_ (act rawCanvas ctx)
--   return $ Canvas rawCanvas

-------------------------------------------------------------------------------
data DrawingAreaConfig t = DrawingAreaConfig
  { _drawingAreaConfig_clear  :: Event t ()
  , _drawingAreaConfig_radius :: Behavior t Double
  , _drawingAreaConfig_color  :: Behavior t String
  , _drawingAreaConfig_undo   :: Event t ()
  , _drawingAreaConfig_send   :: Event t ()
  , _drawingAreaConfig_geom   :: (Int, Int)
  }

defDAC :: Reflex t => DrawingAreaConfig t
defDAC = DrawingAreaConfig never (constant 10) (constant "black") never never (320,240)

instance Reflex t => Default (DrawingAreaConfig t) where
  def = defDAC

data DrawingArea t = DrawingArea
  { _drawingArea_el      :: El t
  , _drawingArea_image   :: Dynamic t ImageData
  }

type TouchId = Word
type ScreenCoord = (Int,Int)
type ImageCord   = (Double,Double)

data WidgetTouches t = WidgetTouches
  { _widgetTouches_touchStarts     :: Event   t (Map.Map TouchId ScreenCoord)
  , _widgetTouches_touchMoves      :: Event   t (Map.Map TouchId ScreenCoord)
  , _widgetTouches_touchEnds       :: Event   t (Map.Map TouchId ScreenCoord)
  , _widgetTouches_currentStrokes  :: Dynamic t (Map.Map TouchId [ScreenCoord])
  , _widgetTouches_finishedStrokes :: Dynamic t [[ScreenCoord]]
  }

anyTouch :: Reflex t => WidgetTouches t -> Event t ()
anyTouch (WidgetTouches s m e _ _) = traceEvent "AnyTouch" $ leftmost [() <$ s, () <$ m, () <$ e]

-- Auxiliary tag
data PointAction = PointsStart | PointsEnd | PointsMove | PointsClear
  deriving (Eq)

widgetTouches :: (PostBuild t m,
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
              => El t
              -> Dynamic t Bool
              -> Event t () -- Event to clear the touches history.
                            -- TODO: Finisging a stroke should be an event with the finished strokes,
                            -- rather than a dynamic that holds them until manual clearing like this.
              -> m (WidgetTouches t)
widgetTouches el okToDraw clears = do

  let e = _element_raw el

  starts      <- wrapDomEvent e (`on` touchStart) (cbStartOrEnd e)
  mousestarts <- wrapDomEvent e (`on` mouseDown)  (mouseHandler e)
  moves       <- wrapDomEvent e (`on` touchMove)  (cbStartOrEnd e)
  mousemoves  <- wrapDomEvent e (`on` mouseMove)  (mouseHandler e)
  ends        <- wrapDomEvent e (`on` touchEnd)   (cbStartOrEnd e)
  mouseends   <- wrapDomEvent e (`on` mouseUp)    (mouseHandler e)

  mouseisdown <- holdDyn False (leftmost [True <$ mousestarts, False <$ mouseends])

  strokes <- foldDyn modifyStrokes (mempty, mempty)
             (gate (current okToDraw ) $
              leftmost [fmap (PointsStart,) starts
                       ,fmap (PointsStart,) mousestarts
                       ,fmap (PointsMove,) moves
                       ,fmap (PointsMove,) (gate (current mouseisdown) mousemoves)
                       ,fmap (PointsEnd,) ends
                       ,fmap (PointsEnd,) mouseends
                       ,(PointsClear, mempty) <$ clears
                       ])

  let currents  = uniqDyn $ fmap fst strokes
      finisheds = uniqDyn $ fmap snd strokes


  return $ WidgetTouches starts moves ends currents finisheds

  where
    -- cbStartOrEnd :: Element -> EventM e TouchEvent (Map.Map TouchId ScreenCoord)
    cbStartOrEnd clientEl = do
      preventDefault
      e <- event
      Just cr <- getBoundingClientRect clientEl
      x0 <- floor <$> getLeft cr
      y0 <- floor <$> getTop  cr
      Just tl <- liftIO $ getChangedTouches e
      liftIO $ touchListToTCMap x0 y0 tl

    -- mouseHandler :: Element -> EventM e MouseEvent (Map.Map TouchId ScreenCoord)
    mouseHandler clientEl = do
      preventDefault
      e <- event
      Just cr <- getBoundingClientRect clientEl
      x0 <- floor <$> getLeft cr
      y0 <- floor <$> getTop  cr
      (x,y) <- bisequence (MouseEvent.getClientX e, MouseEvent.getClientY e)
      return $ 0 =: (x - x0, y - y0)

    touchListToList :: TouchList -> IO [Touch]
    touchListToList tl = do
      n  <- getLength tl
      catMaybes <$> forM [0 .. pred n] (item tl)

    touchListToMap :: TouchList -> IO (Map.Map TouchId Touch)
    touchListToMap tl = fmap Map.fromList $ touchListToList tl >>=
      mapM (\t -> fmap (,t) (getIdentifier t))

    touchListToTCMap :: Int -> Int -> TouchList -> IO (Map.Map TouchId ScreenCoord)
    touchListToTCMap x0 y0 tl = mapM (touchRelCoord x0 y0) =<< touchListToMap tl

    modifyStrokes :: (PointAction, Map.Map TouchId ScreenCoord)
                  -> (Map.Map TouchId [ScreenCoord], [[ScreenCoord]])
                  -> (Map.Map TouchId [ScreenCoord], [[ScreenCoord]])
    modifyStrokes (PointsStart, new) (cur, old) =
      (Map.union (fmap (:[]) new) cur, old)
    modifyStrokes (PointsEnd, del) (cur, old) =
      let delEntries :: Map.Map TouchId [ScreenCoord] = Map.filterWithKey (\k _ -> Map.member k del) cur
          insEntries :: [[ScreenCoord]] = Map.elems $ fmap reverse delEntries
      in  (Map.difference cur delEntries, old ++ insEntries)
    modifyStrokes (PointsMove, new) (cur,old) =
      let cur' = Map.unionWith (++) (fmap (:[]) new) cur
      in  (cur', old)
    modifyStrokes (PointsClear, _) (cur, _) = (cur, mempty)

touchRelCoord x0 y0 tch = relativizedCoord x0 y0 <$> touchCoord tch

tShow :: Show a => a -> T.Text
tShow = T.pack . show


drawingArea :: (PostBuild t m,
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
                           DomBuilderSpace m ~ GhcjsDomSpace) => Dynamic t Bool -> Event t () -> Event t () -> DrawingAreaConfig t -> m (DrawingArea t)
drawingArea okToDraw storePixelsFromCanvas touchClears cfg = mdo

  pb <- getPostBuild

  let (canvW, canvH) = _drawingAreaConfig_geom cfg
  (cEl,_) <- elAttr' "canvas" ("id" =: "canvas"
                      <> "width"  =: tShow canvW
                      <> "height" =: tShow canvH) $ blank

  let canvEl = (castToHTMLCanvasElement . _element_raw) cEl
  img0 <- liftIO $ newImageData' (fromIntegral canvW :: Word) (fromIntegral canvH :: Word)

  Just ctx :: Maybe CanvasRenderingContext2D <- liftIO $ fromJSVal =<< getContext canvEl ("2d" :: JSString)
  performEvent_ $ liftIO (clearArea canvW canvH ctx canvEl) <$ _drawingAreaConfig_clear cfg
  performEvent_ $ liftIO (clearArea canvW canvH ctx canvEl) <$ pb

  -- TODO: Updating the image pixels 2x per second. Find a better way
  pixels <- performEvent (liftIO (getCanvasBuffer canvW canvH ctx canvEl) <$ leftmost [() <$ pixTicks, _drawingAreaConfig_send cfg])
  pixels' <- holdDyn img0 pixels



  touches <- widgetTouches cEl okToDraw touchClears

  let s = _widgetTouches_finishedStrokes touches

  let strokeDone = updated s

  redrawGuardOver <- delay 0.0001 strokeDone
  redrawOk <- holdDyn True (leftmost [True <$ redrawGuardOver, False <$ strokeDone])

  bkgndDelay <- delay 0 strokeDone
  backgroundUpdates <- performEvent (ffor (tag (current s) (leftmost [bkgndDelay, [] <$ storePixelsFromCanvas])) $ \strks ->
    liftIO (recomputeBackground canvW canvH ctx canvEl strks))
  background <- holdDyn Nothing $ fmap Just $ leftmost [backgroundUpdates]

  tInit <- liftIO getCurrentTime
  ticks <- gate (current redrawOk) <$> tickLossy 0.03 tInit

  pixTicks <- tickLossy 0.5 tInit

  let redrawData = (,) <$> fmap Map.elems
                           (current $ _widgetTouches_currentStrokes touches)
                       <*> current background

  performEvent_ $ ffor (tag redrawData ticks) (liftIO . redraw' ctx canvEl)

  return $ DrawingArea cEl pixels'

getMouseEventCoords' :: EventM e MouseEvent (Int,Int)
getMouseEventCoords' = do
  e <- event
  (x,y) <- bisequence (MouseEvent.getClientX e, MouseEvent.getClientY e)
  return (x,y)

-- getTimedMouseEventCoords' :: EventM e MouseEvent TimedCoord
-- getTimedMouseEventCoords' = do
--   e <- event
--   t <- liftIO getCurrentTime
--   (x,y) <- bisequence (getClientX e, getClientY e)
--   return $ TC t x y

relativeCoords :: (PostBuild t m,
                           MonadIO m,
                           MonadFix m,
                           MonadIO (Performable m),
                           TriggerEvent t m,
                           PerformEvent t m,
                           HasWebView m,
                           MonadHold t m,
                           HasWebView (Performable m),
                           PerformEvent t (Performable m),
                           DomBuilderSpace m ~ GhcjsDomSpace) => El t -> m (Dynamic t (Maybe ScreenCoord))
relativeCoords el = do
  let moveFunc (x,y) = do
        now <- liftIO getCurrentTime
        Just cr <- getBoundingClientRect (_element_raw el)
        t <- fmap floor (getTop cr)
        l <- fmap floor (getLeft cr)
        return $ Just ((fromIntegral $ x - l),(fromIntegral $ y - t))
  p <- performEvent $ leftmost [return Nothing <$ domEvent Mouseleave el
                               , (fmap moveFunc (domEvent Mousemove el))]
  holdDyn Nothing p

relativizedCoord :: Int -> Int -> ScreenCoord -> ScreenCoord
relativizedCoord x0 y0 (x,y) = (x-x0, y-y0)

touchCoord :: Touch -> IO ScreenCoord
touchCoord touch = bisequence (getClientX touch, getClientY touch)


getCanvasBuffer :: Int -> Int -> CanvasRenderingContext2D -> HTMLCanvasElement -> IO ImageData
getCanvasBuffer wid hei ctx el = do
  d <- getImageData ctx 0 0 (realToFrac wid) (realToFrac hei)
  maybe (Prelude.error "No imagedata") return d


clearArea :: Int -> Int -> CanvasRenderingContext2D -> HTMLCanvasElement -> IO ()
clearArea wid hei ctx canv = do
  save ctx
  setFillStyle ctx
    (Just $ CanvasStyle $ jsval ("rgba(255,250,255,1)" :: JSString))
  fillRect ctx 0 0 (realToFrac wid) (realToFrac hei)
  restore ctx



recomputeBackground :: Int -> Int -> CanvasRenderingContext2D
                    -> HTMLCanvasElement
                    -> [[ScreenCoord]]
                    -> IO ImageData
recomputeBackground wid hei ctx canv tc = do
  -- save ctx
  liftIO $ putStrLn "Recompute"
  let c = "hsla(100,50%,50%,1)"
  -- setStrokeStyle ctx (Just . CanvasStyle . jsval $ pack c)
  forM_ (filter (not . null) tc) $ \((hX, hY):ps) -> do
    moveTo ctx (fromIntegral hX) (fromIntegral hY)
    forM_ ps $ \(x1, y1) -> do
      lineTo ctx (fromIntegral x1) (fromIntegral y1)
    stroke ctx
  Just bs <- getImageData ctx 0 0 (realToFrac wid) (realToFrac hei)
    -- Data.ByteString.Char8.pack <$>
    -- toDataURL el el (Nothing :: Maybe String)
  -- restore ctx
  return bs


recomputeBackground' :: Int -> Int -> CanvasRenderingContext2D
                     -> HTMLCanvasElement
                     -> [[ScreenCoord]]
                     -> IO ImageData
recomputeBackground' wid hei ctx canv tc = do
  save ctx
  clearArea wid hei ctx canv
  let c = "hsla(100,50%,50%,1)"
  setStrokeStyle ctx (Just . CanvasStyle . jsval $ pack c)
  forM_ (filter (not . null) tc) $ \((hX, hY):ps) -> do
    moveTo ctx (fromIntegral hX) (fromIntegral hY)
    forM_ ps $ \(x1, y1) -> do
      lineTo ctx (fromIntegral x1) (fromIntegral y1)
    stroke ctx
  Just bs <- getImageData ctx 0 0 (realToFrac wid) (realToFrac hei)
    -- Data.ByteString.Char8.pack <$>
    -- toDataURL el el (Nothing :: Maybe String)
  restore ctx
  return bs



redraw' :: CanvasRenderingContext2D
        -> HTMLCanvasElement
        -> ([[ScreenCoord]], Maybe ImageData)
        -> IO ()
redraw' ctx canv (tc,bkg) = do
  save ctx
  t <- getCurrentTime
  case bkg of
    Just _  -> putImageData ctx bkg 0 0
    Nothing -> return () -- clearArea ctx canv
  -- TODO don't just take one
  forM_ tc $ \cs ->
    forM_ (Prelude.zip cs (Prelude.tail $ cs))
      $ \((hX,hY) , (hX',hY')) -> do
        beginPath ctx
        moveTo ctx (fromIntegral hX) (fromIntegral hY)
        -- let h = floor . (* 255) . (^4) . (+ 0.75) . (/ 4) . sin . (2*pi *) $ realToFrac (diffUTCTime t hT)
        --     c = "hsla(" ++ show h ++ ",50%,45%,1)"
        setStrokeStyle ctx (Just . CanvasStyle . jsval $ pack "rgb(0,0,0)")
        lineTo ctx (fromIntegral hX') (fromIntegral hY')
        closePath ctx
        stroke ctx
  restore ctx


#ifndef ghcjs_HOST_OS
data ImageData
data TouchList
data CanvasRenderingContext2D
data JSString = JSSString String
instance IsString JSString where
  fromString = undefined
data ClientRect
pack :: String -> JSString
pack = undefined
jsval = undefined
fromJSVal = undefined
getBoundingClientRect = undefined
getLeft :: MonadIO m => ClientRect -> m Float
getLeft = undefined
getTop :: MonadIO m => ClientRect -> m Float
getTop = undefined
getChangedTouches = undefined
getLength = undefined
getIdentifier = undefined
getContext = undefined
getClientX = undefined
getClientY = undefined
item :: MonadIO m => TouchList -> Word -> m (Maybe Touch)
item = undefined
getImageData  :: MonadIO m => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> m (Maybe ImageData)
getImageData = undefined
newImageData' = undefined
save = undefined
setFillStyle = undefined
fillRect = undefined
restore = undefined
setStrokeStyle = undefined
moveTo = undefined
lineTo = undefined
stroke = undefined
putImageData = undefined
beginPath = undefined
closePath = undefined
data CanvasStyle = CanvasStyle String
setLineWidth = undefined
#endif
