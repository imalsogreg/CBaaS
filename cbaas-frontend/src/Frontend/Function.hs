{-# language CPP #-}
{-# language GADTs #-}
{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language RecursiveDo #-}
{-# language LambdaCase  #-}
{-# language RankNTypes  #-}
{-# language OverloadedStrings  #-}
{-# language ScopedTypeVariables  #-}

module Frontend.Function where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Exception (MonadAsyncException)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Ref (MonadRef, Ref)
import qualified Data.Aeson as A
import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Foldable as F
import Data.List (foldl', isInfixOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Time (getCurrentTime)
import GHC.IORef (IORef)
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
import Frontend.ImageWidget


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
functionPage :: forall t m.(DomBuilder t m,
                            PostBuild t m,
                            MonadIO m,
                            MonadFix m,
                            MonadIO (Performable m),
                            TriggerEvent t m,
                            PerformEvent t m,
                            HasWebView m,
                            MonadHold t m,
                            HasWebView (Performable m),
                            Ref m ~ IORef,
                            Ref (Performable m) ~ IORef,
                            MonadRef m,
                            MonadRef (Performable m),
                            PerformEvent t (Performable m),
                            MonadSample t (Performable m),
                            DomBuilderSpace m ~ GhcjsDomSpace) => Document -> m ()
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

  let env  :: Dynamic t (Map.Map T.Text (Expr Type)) = ffor workers $ \(EntityMap wm) -> Map.fromList . fmap remoteCallToVal $ Map.toList wm
                                                       -- Map.mapWithKey (\k v -> remoteCallToVal (k,v)) wm
      dEnv = ffor (leftmost [updated env, tagDyn env pb]) $ \e -> Map.map Just e

  e <- expression def { _expressionConfig_updateEnvironment =  dEnv }

  go <- button "Evaluate"

  inputWidgets <- listWithKey (fmap (fromMaybe mempty) . fmap hush $ (fmap . fmap) widgetInventory (_expression_expr e)) (inputWidget doc)

  dumbEval env (fmapMaybe id $ fmap hush $ tagPromptlyDyn (_expression_expr e) go)
  -- display $ fmap unEntityMap workers
  display env

  elAttr "div" ("style" =: "border: 1px solid black") $ do
    display $ (fmap . fmap) widgetInventory (_expression_expr e)
    blank

  return ()

tShow :: Show a => a -> T.Text
tShow = T.pack . show

inputWidget :: forall t m.(PostBuild t m,
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
                           DomBuilderSpace m ~ GhcjsDomSpace)
            => Document
            -> T.Text
            -> Dynamic t Type
            -> m (Dynamic t (Expr Type))
inputWidget doc k dynType = do
  inp <- dyn (ffor dynType $ \case
    TModelImage -> do
      text (tShow k)
      imWid <- imageInputWidget doc def
      return $ ELit TModelImage . VImage . ModelImage <$> imageInputWidget_image imWid)
  join <$> holdDyn (defVal <$> dynType) inp

defVal :: Type -> Expr Type
defVal t = ELit t v
  where v = case t of
          TModelImage -> VImage . ModelImage $ defImg
          TDouble -> VDouble 0
          TText -> VText ""
          TList _ -> VList []

holdLastJust :: (Reflex t, PostBuild t m, MonadHold t m) => a -> Dynamic t (Maybe a) -> m (Dynamic t a)
holdLastJust a0 dA = do
  pb <- getPostBuild
  holdDyn a0 $ fmapMaybe id $ leftmost [tagPromptlyDyn dA pb, updated dA]

hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _         = Nothing

type WMap = Map.Map WorkerProfileId WorkerProfile

remoteCallToVal :: (WorkerProfileId, WorkerProfile) -> (T.Text, Expr Type)
remoteCallToVal (wpId, wp@(WorkerProfile wpName (wpFuncName, wpFuncType))) =
        (wpFuncName, ERemote wpFuncType (wpId, wp, wpFuncName))

exprInputWidgets :: (DomBuilder t m)
                 => Dynamic t (Either t (Expr Type))
                 -> m (Dynamic t (Map.Map T.Text Val))
exprInputWidgets = undefined

widgetInventory :: Expr Type -> Map.Map T.Text Type
widgetInventory expr = case expr of
    v@(EVar t varName) -> bool mempty (varName =: t) ("#" `T.isInfixOf` varName)

    lm@(ELambda p n b) -> widgetInventory b

    r@(ERemote _ _) -> mempty

    ap@(EApp p a b) -> widgetInventory a <> widgetInventory b

    ap@(EPrim1 p pr a) -> widgetInventory a

    ap@(EPrim2 p pr a b) -> widgetInventory a <> widgetInventory b

resolveWidgetVars :: Map.Map T.Text (Either T.Text Val)
                  -> Either T.Text (Expr a)
                  -> Either T.Text (Expr a)
resolveWidgetVars env (Left err) = Left err
resolveWidgetVars env (Right expr) = case expr of

    EVar p varName | "#" `T.isPrefixOf` varName -> case Map.lookup varName env of
      Nothing  -> Left $ "No value found for variable: " <> varName
      Just val -> ELit p <$> val

    v@(EVar _ _) -> Right v

    lm@(ELambda p n b) -> ELambda p n <$> resolveWidgetVars env (Right b)

    r@(ERemote _ _) -> Right r

    ap@(EApp p a b) -> liftA2 (EApp p) (resolveWidgetVars env (Right a)) (resolveWidgetVars env (Right b))

    ap@(EPrim1 p pr a) -> EPrim1 p pr <$> resolveWidgetVars env (Right a)

    ap@(EPrim2 p pr a b) -> liftA2 (EPrim2 p pr) (resolveWidgetVars env (Right a)) (resolveWidgetVars env (Right b))

dumbEval :: (DomBuilder t m,
              PostBuild t m,
              MonadIO m,
              MonadFix m,
              MonadIO (Performable m),
              TriggerEvent t m,
              PerformEvent t m,
              HasWebView m,
              MonadHold t m,
              HasWebView (Performable m)
            ) => Dynamic t (Map.Map T.Text (Expr Type)) -> Event t (Expr Type) -> m ()
dumbEval env expr = do
  performEvent_ $ (liftIO . print) <$> expr
  performRequestAsync $ ffor expr $ \case
    EApp _ (ERemote _ ((EntityID wId), wProfile, funcName)) (ELit _ v) ->
      let url = "api1/callfun?worker-id=" <> T.pack (show wId)
      in  XhrRequest url "POST" def { _xhrRequestConfig_sendData = (T.unpack $ E.decodeUtf8 . BSL.toStrict $ A.encode v)}
  return ()



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
