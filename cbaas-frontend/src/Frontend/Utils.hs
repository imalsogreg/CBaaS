{-# language RecursiveDo #-}
{-# language RankNTypes  #-}
{-# language OverloadedStrings  #-}
{-# language GADTs  #-}
{-# language FlexibleContexts  #-}
{-# language ScopedTypeVariables  #-}

module Frontend.Utils where

import Control.Monad (liftM2)
import Control.Monad.Fix (MonadFix)
import Data.Bool
import Data.Monoid
import GHC.IORef (IORef)
import Control.Monad.Ref (MonadRef, Ref)
import qualified Data.Text as T
import Reflex
import Reflex.Dom

data LoggedInEditor = IsEditor
                    | NotIsEditor
                    deriving (Eq)

data ActionState = Viewing
                 | Editing
                 | Saving
                 deriving (Eq)

-- editableWidget :: (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, Ref m ~ IORef, Ref (Performable m) ~ IORef, MonadHold t m, MonadFix m, PerformEvent t m, TriggerEvent t m, HasWebView m, HasWebView (Performable m), PostBuild t m, MonadRef m)
--                => LoggedInEditor
--                   -- ^ Build-time: Can the logged in user edit this?
--                -> (Dynamic t ActionState -> m a)
--                   -- ^ Widget construction action with an 'editing?' context
--                -> Event t ()
--                   -- ^ 'Reset after Save' indicator - brings back the 'viewing' state
--                -> m (Dynamic t a, Dynamic t ActionState, Event t ())
--                -- ^ Return the @a@, editing state, and save-request-triggers
editableWidget isEditor w redraw = mdo

  let runEditing =
        case isEditor of
          NotIsEditor -> do
            let e = constDyn Viewing
            a <- w e
            return (e,a)
          IsEditor -> mdo
            e <- holdDyn Viewing $ leftmost [] -- TODO
            let cState = current e
            (edit, cancel, save) <- elClass "div" "buttons" $ do
              edit   <- auxBtn (Right "pencil")          (== Viewing) e
              cancel <- auxBtn (Right "remove-circle")   (== Editing) e
              save   <- auxBtn (Right "ok-circle")       (== Editing) e
              _      <- auxBtn (Left  "img/spinner.gif") (== Saving)  e
              return (edit,cancel,save)
            a <- w e
            return (e,a)

  let auxBtn :: MonadWidget t m
             => Either T.Text T.Text  -- ^ R Glyph name | L spinner
             -> (ActionState -> Bool) -- ^ Display?
             -> Dynamic t ActionState -- ^ Current editing state
             -> m (Event t ())        -- ^ Return Click events
      auxBtn (Right glyph) drawPredicate editing = do
        btnAttrs <- forDyn editing $ \s ->
          "class" =: ("editable-state glyphicon glyphicon-" <> glyph) <>
          "aria-hidden" =: "true" <>
          (bool ("display" =: "none") mempty (drawPredicate s))
        btn <- fst <$> elDynAttr' "span" btnAttrs (return ())
        return (gate (fmap drawPredicate (current editing)) (domEvent Click btn))
      sometimesButton (Left imgsrc) drawPredicate editing = do
        clicks <- domEvent Click <$> elAttr "img" ("src" =: imgsrc)
        return (gate (fmap drawPredicate (current editing)) clicks)

  ea <- elClass "div" "editable-div" (widgetHold runEditing (runEditing <$ redraw))
  e :: Dynamic t ActionState <- joinDyn <$> mapDyn fst ea
  return (snd <$> ea, e, never)
