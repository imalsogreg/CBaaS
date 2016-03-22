{-# language RecursiveDo #-}
{-# language LambdaCase  #-}
{-# language RankNTypes  #-}

module Frontend.Function where

import Data.List (isInfixOf)
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text as T
import Reflex
import Reflex.Dom
import RemoteFunction

------------------------------------------------------------------------------
-- | FunctionWidget shows profile information for a function,
--   and editing controls when the viewer has editing rights
data FunctionWidget t = FunctionWidget
  { _functionWidget_profile :: Dynamic t Function }

------------------------------------------------------------------------------
-- | TODO: more info, styling. editable (hash)tags for functions
functionWidget :: MonadWidget t m => FunctionWidget t -> m ()
functionWidget f = do
  d <- dyn =<< mapDyn functionDetails (_functionWidget_profile f)
  return ()
  where functionDetails fun =
          elClass "div" "function-widget" $ do
            el "div" (text . T.unpack $ fnName fun)
            el "div" (text $ show $ fnType fun)
            mapM_ (elClass "div" "tag" . text . show) (fnTags fun)


functionListing :: forall t m .MonadWidget t m
                => Dynamic t (Map.Map T.Text Function)
                -- TODO: change Function to FunctionInfo
                --       FunctionInfo might list tags,
                --       usage history, nWorkers implementing, etc
                -> m (Event t String)
functionListing functions = mdo
  searchbox <- value <$> elClass "div" "search-box" (textInput def)
  funPredicate <- mapDyn funListPredicate searchbox
  okFuns <- combineDyn Map.filterWithKey funPredicate functions

  listing <- selectViewListWithKey_ curSelect okFuns $ \k v b -> do
    divAttrs <- forDyn b $ \case
      False -> "class" =: "function-entry"
      True  -> "class" =: "function-entry selected" <> "style" =: "background-color:gray" -- TODO only class & css
    (e,_) <- elDynAttr' "div" divAttrs (functionWidget (FunctionWidget v))
    return (k <$ domEvent Click e)

  curSelect <- holdDyn T.empty listing
  return (T.unpack <$> updated curSelect)

 -- TODO: more customizations
funListPredicate :: String -> T.Text -> v -> Bool
funListPredicate s k v
  | null s    = False
  | otherwise = s `isInfixOf` T.unpack k
