{-# language RecursiveDo #-}

module Frontend.Utils where

import Reflex
import Reflex.Dom

data LoggedInEditor = IsEditor
                    | NotIsEditor

data Action = Viewing
            | Editing
            | Saving

editableWidget :: MonadWidget t m
               => LoggedInEditor
                  -- ^ Build-time: Can the logged in user edit this?
               -> (Behavior t Editing -> m a)
                  -- ^ Widget construction action with an 'editing?' context
               -> Event t ()
                  -- ^ 'Save' indicator - triggers redraw in the 'viewing' state
               -> m (a, Dynamic t Action, Event t ())
                  -- ^ Return the @a@, editing state, and save-request-triggers
editableWidget isEditor redraw w = do

  elClass "div" "editable-div" $ widgetHold (w' <$ redraw)
  where w' = case isEditor of
          NotIsEditor -> w (constant NotEditing)
          IsEditor    -> mdo

            editingState <- holdDyn Viewing $ leftmost [] -- TODO

            let cState = current editingState
            (edit, cancel, save) <- elClass "div" "buttons" $ do
              edit   <- auxBtn (Right "pencil")          (== Viewing) cState
              cancel <- auxBtn (Right "remove-circle")   (== Editing) cState
              save   <- auxBtn (Right "ok-circle")       (== Editing) cState
              _      <- auxBtn (Left  "img/spinner.gif") (== Saving)  cState
              return (edit,cancel,save)

            return ()

        auxBtn :: MonadWidget t m
               => Either String String -- ^ R Glyph name | L spinner
               -> (Editing -> Bool)    -- ^ Display?
               -> Behavior t Action    -- ^ Current editing state
               -> m (Event t ())       -- ^ Return Click events
        auxBtn (Right glyph) drawPredicate editing = do
          btnAttrs <- forDyn editingState $ \s ->
            "class" =: ("editable-state glyphicon glyphicon-" <> glyph) <>
            "aria-hidden" =: "true"
            (bool ("display" =: "none") mempty (drawPredicate s))
          btn <- fst <$> elDynAttr' "span" btnAttrs (return ())
          return (gate (fmap drawPredicate editing) domEvent Click btn)
        sometimesButton (Left imgsrc) drawPredicate editing =
          clicks <- domEvent Click <$> elAttr "img" ("src" =: imgsrc)
          return (gate (fmap drawPredicate editing) clicks)
