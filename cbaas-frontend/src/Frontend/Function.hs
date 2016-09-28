{-# language CPP #-}
{-# language GADTs #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language RankNTypes #-}
{-# language RecursiveDo #-}
{-# language LambdaCase  #-}
{-# language RankNTypes  #-}
{-# language OverloadedStrings  #-}
{-# language JavaScriptFFI #-}
{-# language TypeSynonymInstances #-}
{-# language ScopedTypeVariables  #-}

module Frontend.Function where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Exception (MonadAsyncException)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Ref (MonadRef, Ref)
import qualified Data.Aeson as A
import           Data.Aeson ((.=))
import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Either (isRight)
import qualified Data.Foldable as F
import Data.List (foldl', isInfixOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Time (getCurrentTime)
import GHC.IORef (IORef)
import GHCJS.DOM.Types (Document)
import Reflex
import Reflex.Dom
import Reflex.Dom.WebSocket
import Text.PrettyPrint.HughesPJClass (prettyShow)
import Message
import Model
import RemoteFunction
import WorkerProfile
import BrowserProfile
import EntityID
#ifdef ghcjs_HOST_OS
import GHCJS.DOM.Document (getLocation)
import GHCJS.Marshal
import GHCJS.Types (JSVal)
#endif
import GHCJS.DOM.Location

import Job
import Pretty
import Frontend.Expression
import Frontend.ImageWidget


------------------------------------------------------------------------------
-- | FunctionWidget shows profile information for a function,
--   and editing controls when the viewer has editing rights
-- data FunctionWidget t = FunctionWidget
--   { _functionWidget_profile :: Dynamic t WorkerProfile }

functionListingItem :: (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, PostBuild t m) => T.Text -> Dynamic t Type -> m (Event t T.Text) -- FunctionWidget t -> m ()
functionListingItem k t = do
  item <- fmap fst $ elAttr' "div" ("class" =: "item" <> "data-value" =: k)$ do
    elAttr "span" ("class" =: "description") $ dynText $ T.pack . prettyShow <$> t
    text k
  return $ (k <> " #1") <$ domEvent Click item


functionListing' :: forall t m .(DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, MonadFix m,MonadHold t m, PostBuild t m)
                => Dynamic t (Map.Map T.Text Type) -- WorkerProfileMap -- (Map.Map T.Text Function)
                -- TODO: change Function to FunctionInfo
                --       FunctionInfo might list tags,
                --       usage history, nWorkers implementing, etc
                -> m (Event t T.Text)
functionListing' funs = do
 elAttr "div" ("style" =: "min-width: 300px;") $ do
  divClass "ui fluid search selection dropdown" $ do
    elAttr "input" ("type" =: "hidden" <> "name" =: "function-search") blank
    elAttr "i" ("class" =: "dropdown icon") blank
    divClass "default text" $ text "Remote function..."
    menu :: Dynamic t (Map.Map T.Text (Event t T.Text)) <- divClass "menu" $
      listWithKey funs (functionListingItem)
    return $ switchPromptlyDyn $ leftmost . Map.elems <$> menu

------------------------------------------------------------------------------
-- | A list of functions meant to update with typing in the search box
functionListing :: forall t m .(DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, MonadFix m,MonadHold t m, PostBuild t m)
                => Dynamic t (Map.Map T.Text Type) -- WorkerProfileMap -- (Map.Map T.Text Function)
                -> Dynamic t T.Text
                -- TODO: change Function to FunctionInfo
                --       FunctionInfo might list tags,
                --       usage history, nWorkers implementing, etc
                -> m (Event t T.Text)
functionListing functions sel = mdo
  searchbox <- value <$> elClass "div" "search-box" (textInput def)
  funPredicate :: Dynamic t (T.Text -> Type -> Bool) <- mapDyn funListPredicate searchbox
  okFuns <- combineDyn (\p m -> Map.filterWithKey p m) funPredicate functions

  listing <- selectViewListWithKey_ curSelect okFuns $ \k v b -> do
    let divAttrs = ffor b $ \case
          False -> "class" =: "function-entry"
          True  -> "class" =: "function-entry selected" <>
                   "style" =: "background-color:gray" -- TODO only use class & css file
    (e,_) <- elDynAttr' "div" divAttrs (functionListingItem k v)
    return (k <$ domEvent Click e)

  curSelect :: Dynamic t T.Text <- holdDyn T.empty listing
  return $ updated curSelect

 -- TODO: more customizations
funListPredicate :: T.Text -> T.Text -> Type -> Bool
funListPredicate s k t
  | T.null s  = True
  | otherwise = s `T.isInfixOf` k

viewResults resultIds = do
  results <- getAndDecode $ ffor resultIds $ \(EntityID rId) ->
    "/api1/jobresult?job-id=" <> tShow rId
  widgetHold blank $ ffor results $ \case
    Just (JobResult (VImage mi) _) -> displayImg' mi
    Just (JobResult (VText t) _ ) -> text t
    Just (JobResult (VLabelProbs ps) _ ) -> displayLabelProbs ps
    Nothing -> text "Nothing"
    _ -> text "Non-image result"
  blank
  -- display =<< foldDyn (:) ([] :: [Maybe JobResult]) results

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

  -- TODO: Is this necessary, is there a cleaner way? (dropdown doesn't seem to work with less)
  performEvent_ =<< delay 0.1 (liftIO initDropdown <$ pb)

  t0 <- liftIO getCurrentTime
  tick <- tickLossy 1 t0
  browserURL <- unrelativizeWebSocketUrl doc "/api1/browserupdates"
  ws <- webSocket browserURL (WebSocketConfig (wsSends :: Event t [BS.ByteString]))
  let msg = decoded (_webSocket_recv ws)
  let x  = msg :: Event t BrowserMessage

  wsSends <- return $ leftmost [["ping"] <$ tick]

  browserId <- holdDyn Nothing $ fmapMaybe id $ ffor msg $ \case
    SetBrowserID i -> Just $ Just i
    _              -> Nothing

  let resultReport = fmapMaybe id $ ffor msg $ \case
        (JobFinished jobId) -> Just jobId
        _                   -> Nothing

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
      envWithWidgets :: Dynamic t (Map.Map T.Text (Expr Type)) = zipDynWith (<>) env inputWidgets
      dEnv = ffor (leftmost [updated env, tagPromptlyDyn env pb]) $ \e -> Map.map Just e

  listingClicks <- divClass "ui menu" $ do
    elClass "a" "item" $ text "cbaas"
    elClass "a" "item" $ text "Help"
    divClass "right menu" $ elClass "a" "ui item" $
      functionListing' ((Map.fromList . map wpFunction . Map.elems . unEntityMap) <$> workers :: Dynamic t (Map.Map T.Text Type))

  jobsState <- foldDyn ($) mempty $ leftmost [Set.insert <$> jobs, Set.delete <$> resultReport]
  let evalButtonState :: Dynamic t EvalStatus = zipDynWith
        (\js exprValid -> if Set.null js
                          then bool EvalInvalid EvalOk (isRight exprValid)
                          else EvalWorking) jobsState (_expression_expr e)
  evalButtonState' <- holdDyn EvalOk $ leftmost [updated evalButtonState, EvalWorking <$ _expression_go e]

  (e, inputWidgets, jobs) <- divClass "non-menu-content" $ do

    e <- divClass "expression" $ do
      divClass "ui labeled input" $ do
        divClass "ui label" $ do
          text "cbaas:"
        expression def { _expressionConfig_updateEnvironment =  dEnv
                       , _expressionConfig_evalStatus = evalButtonState'
                       , _expressionConfig_setText = listingClicks
                       }

    (inputWidgets, jobs) <- divClass "inputs-outputs" $ do

      inputWidgets <- divClass "inputs" $
        joinDynThroughMap <$>
        listWithKey (fmap (fromMaybe mempty) . fmap hush $ (fmap . fmap) widgetInventory (_expression_expr e)) (inputWidget doc)

      let furnishedExpr :: Dynamic t (Either T.Text (Expr Type)) =
            zipDynWith (\env' expr' -> resolveWidgetVars env' =<< expr') envWithWidgets (_expression_expr e)

      jobs <- dumbEval env (fmapMaybe id $ fmap hush $ tagPromptlyDyn furnishedExpr (_expression_go e))

      elAttr "div" ("class" =: "results") $ viewResults resultReport

      return (inputWidgets, jobs)

    return (e, inputWidgets, jobs)

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
      imWid <- imageInputWidget doc def
      text k
      return $ ELit TModelImage . VImage . ModelImage <$> imageInputWidget_image imWid)
  join <$> holdDyn (defVal <$> dynType) inp

defVal :: Type -> Expr Type
defVal t = ELit t v
  where v = case t of
          TModelImage -> VImage . ModelImage $ defImg
          TDouble -> VDouble 0
          TText -> VText ""
          TList -> VList []

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

resolveWidgetVars :: Map.Map T.Text (Expr Type)
                  -> Expr Type
                  -> Either T.Text (Expr Type)
resolveWidgetVars env expr = case expr of

    EVar p varName | "#" `T.isPrefixOf` varName -> case Map.lookup varName env of
      Nothing  -> Left $ "No value found for variable: " <> varName
      Just val -> Right $ val

    v@(EVar _ _) -> Right v

    lm@(ELambda p n b) -> ELambda p n <$> resolveWidgetVars env b

    r@(ERemote _ _) -> Right r

    ap@(EApp p a b) -> liftA2 (EApp p) (resolveWidgetVars env a) (resolveWidgetVars env b)

    ap@(EPrim1 p pr a) -> EPrim1 p pr <$> resolveWidgetVars env a

    ap@(EPrim2 p pr a b) -> liftA2 (EPrim2 p pr) (resolveWidgetVars env a) (resolveWidgetVars env b)

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
            ) => Dynamic t (Map.Map T.Text (Expr Type)) -> Event t (Expr Type) -> m (Event t (EntityID Job))
dumbEval env expr = do
  -- performEvent_ $ (liftIO . print) <$> expr
  jobs <- performRequestAsync $ ffor expr $ \case
    EApp _ (ERemote _ ((EntityID wId), wProfile, funcName)) (ELit _ v) ->
      let url = "api1/callfun?worker-id=" <> T.pack (show wId)
          job = Job funcName v
      in  XhrRequest "POST"  url def { _xhrRequestConfig_sendData = (T.unpack $ E.decodeUtf8 . BSL.toStrict $ A.encode job)
                                     , _xhrRequestConfig_headers = "Content-Type" =: "application/json"}
  return $ fmapMaybe id $ fmap (\x -> A.decode . BSL.fromStrict . E.encodeUtf8 =<< _xhrResponse_responseText x) jobs



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

-- TODO: This crashes if run too early (before Plotly.js is done loading)
displayLabelProbs :: (DomBuilder t m, PostBuild t m, PerformEvent t m, MonadIO m, MonadIO (Performable m)) => [(T.Text, Double)] -> m ()
displayLabelProbs lps = do
  pb <- getPostBuild
  elAttr "div" ("id" =: "plotly-area" <> "style" =: "height:360px;") blank
  performEvent_ $ (liftIO $ runPlotlyLayout obj layout) <$ pb
  where obj      = [dat]
        layout   = A.object [ "margin" .= A.object
                              ["t" .= (0::Int)
                              ,"b" .= (20::Int)
                              ,"l" .= (200::Int)
                              ,"r" .= (120::Int)
                              ]
                            , "height" .= ("360" :: T.Text)]
        dat      = A.object [ "y"           .= labels
                            , "x"           .= probs
                            , "type"        .= ("bar" :: T.Text)
                            , "orientation" .= ("h" :: T.Text)]
        (labels,probs) = unzip $ reverse lps

#ifdef ghcjs_HOST_OS

runPlotly :: A.ToJSON a => a -> IO ()
runPlotly dat = js_runPlotly =<< toJSVal_aeson dat

runPlotlyLayout :: (A.ToJSON a, A.ToJSON b) => a -> b -> IO ()
runPlotlyLayout dat layout = toJSVal_aeson dat >>= \a -> toJSVal_aeson layout >>= \b -> js_runPlotlyLayout a b

runPlotlyLayoutOptions :: (A.ToJSON a, A.ToJSON b, A.ToJSON c) => a -> b -> c -> IO ()
runPlotlyLayoutOptions dat layout options = toJSVal_aeson options >>= \o -> toJSVal_aeson layout >>= \l -> toJSVal_aeson dat >>= \d -> js_runPlotlyLayoutOptions d l o


foreign import javascript unsafe "Plotly.plot('plotly-area', $1)"
  js_runPlotly :: JSVal -> IO ()

foreign import javascript unsafe "Plotly.plot('plotly-area', $1, $2)"
  js_runPlotlyLayout :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "Plotly.plot('plotly-area', $1, $2, $3)"
  js_runPlotlyLayoutOptions :: JSVal -> JSVal -> JSVal -> IO ()



foreign import javascript safe "$('.ui.dropdown').dropdown(); console.log('test')"
  initDropdown :: IO ()
#else
initDropdown = undefined
getHost = undefined
getPathname = undefined
getLocation = undefined
getProtocol = undefined
runPlotly = undefined
toJSVal_aeson = undefined
runPlotlyLayout = undefined
runPlotlyLayoutOptions = undefined
instance IsXhrPayload String where
  xmlHttpRequestSend = undefined
#endif
