{-# language RecursiveDo #-}
{-# language RankNTypes  #-}

module Frontend.Utils where

import Control.Monad (liftM2)
import Data.Bool
import Data.Monoid
import Reflex
import Reflex.Dom

data LoggedInEditor = IsEditor
                    | NotIsEditor
                    deriving (Eq)

data ActionState = Viewing
                 | Editing
                 | Saving
                 deriving (Eq)

editableWidget :: MonadWidget t m
               => LoggedInEditor
                  -- ^ Build-time: Can the logged in user edit this?
               -> (Dynamic t ActionState -> m a)
                  -- ^ Widget construction action with an 'editing?' context
               -> Event t ()
                  -- ^ 'Reset after Save' indicator - brings back the 'viewing' state
               -> m (Dynamic t a, Dynamic t ActionState, Event t ())
               -- ^ Return the @a@, editing state, and save-request-triggers
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
             => Either String String  -- ^ R Glyph name | L spinner
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
  e <- joinDyn <$> mapDyn fst ea
  a <- mapDyn snd ea
  return (a, e, never)
