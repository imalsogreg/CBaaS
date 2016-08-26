{-# language CPP #-}
{-# language GADTs #-}
{-# language FlexibleContexts #-}
{-# language RecursiveDo #-}
{-# language LambdaCase  #-}
{-# language RankNTypes  #-}
{-# language OverloadedStrings  #-}
{-# language ScopedTypeVariables  #-}

module Frontend.Function where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Foldable as F
import Data.List (foldl', isInfixOf)
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import GHCJS.DOM.Types (Document)
import Reflex
import Reflex.Dom
import Reflex.Dom.WebSocket
import Message
import Model
import RemoteFunction
import WorkerProfile
import BrowserProfile
import EntityID
#ifdef ghcjs_HOST_OS
import GHCJS.DOM.Document (getLocation)
#endif
import GHCJS.DOM.Location
import Frontend.Expression


------------------------------------------------------------------------------
-- | FunctionWidget shows profile information for a function,
--   and editing controls when the viewer has editing rights
data FunctionWidget t = FunctionWidget
  { _functionWidget_profile :: Dynamic t Function }

functionWidget :: (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, PostBuild t m) => FunctionWidget t -> m ()
functionWidget f = do
  d <- dyn =<< mapDyn functionDetails (_functionWidget_profile f)
  return ()
  where functionDetails fun =
          elClass "div" "function-widget" $ do
            el "div" (text $ fnName fun)
            el "div" (text . T.pack . show $ fnType fun)
            mapM_ (elClass "div" "tag" . text . T.pack . show) (fnTags fun)


------------------------------------------------------------------------------
-- | A list of functions meant to update with typing in the search box
functionListing :: forall t m .(DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, MonadFix m,MonadHold t m, PostBuild t m)
                => Dynamic t (Map.Map T.Text Function)
                -> Dynamic t T.Text
                -- TODO: change Function to FunctionInfo
                --       FunctionInfo might list tags,
                --       usage history, nWorkers implementing, etc
                -> m (Event t T.Text)
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
  return $ updated curSelect

 -- TODO: more customizations
funListPredicate :: T.Text -> T.Text -> v -> Bool
funListPredicate s k v
  | T.null s  = False
  | otherwise = s `T.isInfixOf` k


------------------------------------------------------------------------------
-- | Page-level coordination of function-related widgets
functionPage :: forall t m.(DomBuilder t m, HasWebView (Performable m), MonadHold t m, MonadIO m, MonadIO (Performable m), MonadFix m, TriggerEvent t m, PerformEvent t m, PostBuild t m, HasWebView m) => Document -> m ()
functionPage doc = mdo
  pb <- getPostBuild

  t0 <- liftIO getCurrentTime
  tick <- tickLossy 1 t0
  browserURL <- unrelativizeWebSocketUrl doc "/api1/browserupdates"
  ws <- webSocket browserURL (WebSocketConfig wsSends)
  let msg = decoded (_webSocket_recv ws)
  let x  = msg :: Event t BrowserMessage

  wsSends <- return $ leftmost [["ping"] <$ tick]

  browserId <- holdDyn Nothing $ fmapMaybe id $ ffor msg $ \case
    SetBrowserID i -> Just $ Just i
    _              -> Nothing

  workers0 :: Event t WorkerProfileMap <- fmapMaybe id <$> getAndDecode ("/api1/worker" <$ pb)
  let workersInit :: Event t (WorkerProfileMap -> WorkerProfileMap) = ffor workers0 $ \(EntityMap ws) -> const . EntityMap $
        foldl' (\m (k,v) -> Map.insert k v m) mempty (Map.toList ws)

  let workersModify :: Event t (WorkerProfileMap -> WorkerProfileMap) = fforMaybe msg $ \case
        WorkerJoined wId wProfile -> Just (EntityMap . Map.insert wId wProfile . unEntityMap)
        WorkerLeft wId            -> Just (EntityMap . Map.delete wId . unEntityMap)
        _                         -> Nothing

  workers :: Dynamic t WorkerProfileMap <- foldDyn ($) (EntityMap mempty) (leftmost [workersInit, workersModify])
  nWorkers :: Dynamic t Int <- mapDyn (F.length . unEntityMap) workers

  display browserId
  display $ fmap unEntityMap workers

  return ()

type WMap = Map.Map WorkerProfileId WorkerProfile

decoded :: (Reflex t, A.FromJSON a)
                   => Event t BS.ByteString
                   -> Event t a
decoded = fmapMaybe (A.decode . BSL.fromStrict)


------------------------------------------------------------------------------
unrelativizeWebSocketUrl :: (DomBuilder t m, MonadIO m) => Document -> T.Text -> m T.Text
unrelativizeWebSocketUrl doc s = do
  (Just loc) <- liftIO $ getLocation doc
  newProto :: T.Text <- liftIO (getProtocol loc) >>= \case
    ("https:" :: T.Text) -> return "wss:"
    "http:"              -> return "ws:"
  host <- liftIO $ getHost loc
  path <- if "/" `T.isPrefixOf` s
          then return ""
          else liftIO $ getPathname loc
  return $ newProto <> "//" <> host <> path <> s

#ifndef ghcjs_HOST_OS
getHost = undefined
getPathname = undefined
getLocation = undefined
getProtocol = undefined
#endif
