{-# language RecursiveDo #-}
{-# language LambdaCase  #-}
{-# language RankNTypes  #-}
{-# language ScopedTypeVariables  #-}

module Frontend.Function where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Foldable as F
import Data.List (foldl', isInfixOf)
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text as T
import Reflex
import Reflex.Dom
import Reflex.Dom.WebSocket
import Message
import Model
import RemoteFunction
import WorkerProfile


------------------------------------------------------------------------------
-- | FunctionWidget shows profile information for a function,
--   and editing controls when the viewer has editing rights
data FunctionWidget t = FunctionWidget
  { _functionWidget_profile :: Dynamic t Function }

functionWidget :: MonadWidget t m => FunctionWidget t -> m ()
functionWidget f = do
  d <- dyn =<< mapDyn functionDetails (_functionWidget_profile f)
  return ()
  where functionDetails fun =
          elClass "div" "function-widget" $ do
            el "div" (text . T.unpack $ fnName fun)
            el "div" (text $ show $ fnType fun)
            mapM_ (elClass "div" "tag" . text . show) (fnTags fun)


------------------------------------------------------------------------------
-- | A list of functions meant to update with typing in the search box
functionListing :: forall t m .MonadWidget t m
                => Dynamic t (Map.Map T.Text Function)
                -> Dynamic t T.Text
                -- TODO: change Function to FunctionInfo
                --       FunctionInfo might list tags,
                --       usage history, nWorkers implementing, etc
                -> m (Event t String)
functionListing functions sel = mdo
  searchbox <- value <$> elClass "div" "search-box" (textInput def)
  funPredicate :: Dynamic t (T.Text -> Function -> Bool) <- mapDyn funListPredicate searchbox
  okFuns <- combineDyn Map.filterWithKey funPredicate functions

  listing <- selectViewListWithKey_ curSelect okFuns $ \k v b -> do
    divAttrs <- forDyn b $ \case
      False -> "class" =: "function-entry"
      True  -> "class" =: "function-entry selected" <>
               "style" =: "background-color:gray" -- TODO only use class & css file
    (e,_) <- elDynAttr' "div" divAttrs (functionWidget (FunctionWidget v))
    return (k <$ domEvent Click e)

  curSelect <- holdDyn T.empty listing
  return (T.unpack <$> updated curSelect)

 -- TODO: more customizations
funListPredicate :: String -> T.Text -> v -> Bool
funListPredicate s k v
  | null s    = False
  | otherwise = s `isInfixOf` T.unpack k


------------------------------------------------------------------------------
-- | Page-level coordination of function-related widgets
functionPage :: forall t m.MonadWidget t m => m ()
functionPage = mdo
  pb <- getPostBuild
  ws <- webSocket "/api/browser" (WebSocketConfig wsSends)
  let msg = decoded (_webSocket_recv ws)
  let x  = msg :: Event t BrowserMessage

  wsSends <- return never -- TODO Send data sometimes?

  browserId <- holdDyn 0 $ fforMaybe msg $ \case
    SetBrowserID i -> Just i
    _              -> Nothing

  workers0 :: Event t WMap <- fmapMaybe id <$> getAndDecode ("/api/worker" <$ pb)
  let workersInit :: Event t (WMap -> WMap) = ffor workers0 $ \(ws) -> undefined
        -- foldl' (\(k,v) m -> Map.insert k v m) mempty (Map.toList ws)

  workersModify :: Event t (WMap -> WMap) <- fforMaybe msg $ \case
    WorkerJoined wId wProfile -> Just (Map.insert wId wProfile)
    WorkerLeft wId            -> Just (Map.delete wId)
  workers :: Dynamic t WMap <- _ -- foldDyn ($) mempty (leftmost [workersInit, workersModify])
  nWorkers :: Dynamic t Int <- mapDyn F.length workers

  -- elClass "div" "function-page" $ do
  --   functionListing workers (constDyn 0)

  return ()

type WMap = Map.Map WorkerProfileId WorkerProfile

decoded :: (Reflex t, A.FromJSON a)
                   => Event t BS.ByteString
                   -> Event t a
decoded = fmapMaybe (A.decode . BSL.fromStrict)
