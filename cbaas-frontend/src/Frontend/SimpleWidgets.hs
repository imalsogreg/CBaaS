{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings   #-}
{-# language GADTs               #-}
{-# language FlexibleContexts    #-}
{-# language TypeFamilies    #-}

module Frontend.SimpleWidgets where

import Data.Bool (bool)
import Data.Default
import Data.Either (isRight)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Reflex.Dom
import Text.Read (readMaybe)
import Frontend.Utils

data TextWidgetConfig t = TextWidgetConfig
  { _textWidgetConfig_initialText :: T.Text
  , _textWidgetConfig_setText :: Event t T.Text
  }

instance Reflex t => Default (TextWidgetConfig t) where
  def = TextWidgetConfig "" never

data TextWidget t = TextWidget
  { _textWidget_text :: Dynamic t T.Text
  }

instance HasValue (TextWidget t) where
  type Value (TextWidget t) = Dynamic t T.Text
  value = _textWidget_text

textWidget
  :: (DomBuilderSpace m ~ GhcjsDomSpace,
     DomBuilder t m,
     PostBuild t m
     )
  => TextWidgetConfig t -> m (TextWidget t)
textWidget (TextWidgetConfig t0 dt) = do
  ti <- textInput def { _textInputConfig_initialValue = t0
                      , _textInputConfig_setValue     = dt }
  return $ TextWidget (value ti)

data DoubleWidgetConfig t = DoubleWidgetConfig
  { _doubleWidgetConfig_initialVal :: Double
  , _doubleWidgetConfig_setVal     :: Event t Double
  }

instance Reflex t => Default (DoubleWidgetConfig t) where
  def = DoubleWidgetConfig 9 never

data DoubleWidget t = DoubleWidget
  { _doubleWidget_double :: Dynamic t Double
  }

instance HasValue (DoubleWidget t) where
  type Value (DoubleWidget t) = Dynamic t Double
  value = _doubleWidget_double

doubleWidget
  :: (DomBuilderSpace m ~ GhcjsDomSpace,
      DomBuilder t m,
      PostBuild t m,
      MonadHold t m
      )
  => DoubleWidgetConfig t -> m (DoubleWidget t)
doubleWidget  (DoubleWidgetConfig d0 dd) = do
  ti <- textInput def { _textInputConfig_initialValue = tshow d0
                      , _textInputConfig_setValue = tshow <$> dd }
  let aux = fromMaybe (Left ("Double parse error" :: T.Text)) . readMaybe . T.unpack
      parsed = aux <$> value ti
  lastDouble <- holdDyn d0 $ fmapMaybe id $ hush <$> updated parsed
  let errMsg = zipDynWith (\p ld -> bool ("(" <> tshow ld <> ") Please enter a number")
                                         ("")
                                         (isRight p)
                          ) parsed lastDouble
  dynText errMsg
  return $ DoubleWidget lastDouble
