{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- module TypeCheck (
--     typeInfer
--   , dumbCheck
--   , dumbCheck'
--   , mgu
--   , merge
--   , (@@)
--   , tcRun -- temporary
--   ) where
module TypeCheck where

import Control.Monad ((>=>),liftM2)
import qualified Data.List as L
import Data.Monoid ((<>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Model
import Kind
import qualified Kind as K
import Type
import Parse -- temporary, for an error message
import Pretty -- temporary, for an error message
import Debug.Trace

newtype Subst = Subst { unSubst :: [(TyVar, Type)] }
  deriving (Eq, Show)

showSubst :: Subst -> T.Text
showSubst (Subst xs) =
  T.unlines $ map (\((n,_),t) -> n <> ": " <> T.pack (show $ prettyType t)) xs

type TyVar = (T.Text,Kind)

class HasKind t where
  kind :: t -> Kind

instance HasKind Kind where
  kind = id

instance HasKind Type where
  kind (TVar n k) = k
  kind (TCon _ k) = k
  kind (TApp f a) = case kind f of
                      TyFun _ b -> b
                      _         -> error "Kind error"
  
instance HasKind TyVar where
  kind (_,k) =  k

class Types t where
  apply :: Subst -> t -> t
  tv    :: t -> [TyVar]

instance Types Type where
  apply s (TVar u k)   = case lookup (u,k) (unSubst s) of
                           Just t  -> t
                           Nothing -> TVar u k
  apply s (TApp u v) = TApp (apply s u) (apply s v)
  apply _ t          = t
  tv (TVar u k)      = [(u,k)]
  tv (TApp u v)      = tv u `L.union` tv v
  tv _               = []

instance Types a => Types [a] where
  apply s as = map (apply s) as
  tv = L.nub . L.concat . map tv


instance Types (Expr Type) where
  apply s e = fmap (apply s) e
  tv (ELit t _) = tv t
  tv (EVar t _) = tv t
  tv (ELambda t _ b) = tv t `L.union` tv b
  tv (ERemote t _) = tv t
  tv (EApp t f a) = tv t `L.union` tv f `L.union` tv a
  tv (EPrim _ _ ) = []

exprType :: Expr Type -> Type
exprType e = case e of
  ELit t _ -> t
  EVar t _ -> t
  ELambda t _ _ -> t
  ERemote t _ -> t
  EApp t _ _ -> t
  EPrim t _ -> t

  

infixl 4 =:
(=:) :: TyVar -> Type -> Subst
n =: t = Subst [(n,t)] 

infixl 4 @@
(@@) :: Subst -> Subst -> Subst
Subst s1 @@ Subst s2 = Subst $ [(u, apply (Subst s1) t) | (u,t) <- s2] ++ s1

instance Monoid Subst where
  mempty         = Subst []
  a `mappend` b  = a @@ b

merge :: Subst -> Subst -> Either T.Text Subst
merge s1 s2 = if agree then return (Subst $ unSubst s1 ++ unSubst s2) else Left "Merge fails"
  where agree = all (\(v,k) -> apply s1 (TVar v k) == apply s2 (TVar v k))
                (map fst (unSubst s1) `L.intersect` map fst (unSubst s2))


mgu :: Type -> Type -> Either T.Text Subst
mgu (TApp l r) (TApp l' r') = liftM2 (@@) (mgu l l') (mgu r r')
mgu (TVar u k) t = varBind (u,k) t
mgu (TCon tc1 k1) (TCon tc2 k2)
  | tc1 == tc2 = return mempty
  | otherwise  =  Left $ "Unification error between type constructors: " <>
                  (T.pack . show) tc1 <> " & " <> (T.pack . show) tc2
mgu a b = Left $ T.unlines
  ["Unification error"
  , "Couldn't unify: "
  , "  " <> (T.pack . show) (prettyType a)
  , "with: "
  , "  " <> (T.pack . show) (prettyType b)
  ]

varBind :: TyVar -> Type -> Either T.Text Subst
varBind (tn,k) t
  | t == TVar tn k     = return mempty
  | (tn,k) `elem` tv t = error "occurs check fails"
  | k /= kind t        = error "kind mismatch"
  | otherwise          = return $ (tn,k) =: t

match :: Type -> Type -> Either T.Text Subst
match (TApp l r) (TApp l' r') = do
  s1 <- match l l'
  s2 <- match r r'
  merge s1 s2
match (TVar n k) t
  | k == kind t = return $ (n,k) =: t
  | otherwise        = Left "Kind mismatch"
match (TCon tc1 k1) (TCon tc2 k2)
  | tc1 == tc2 = return mempty
  | otherwise  = Left "Type mismatch"
match _ _ = Left "Type mismatch"

insertWithFreshTName :: T.Text -> Type -> Expr Type -> Subst -> (T.Text, Subst)
insertWithFreshTName n t ex s@(Subst subs) =
  let n' = findFreshTName ex s
  in (n', Subst (((n', K.Type),t) : subs))

findFreshTName :: Expr Type -> Subst -> T.Text
findFreshTName ex (Subst subs) =
  let allNames = Set.fromList [T.pack (l : show n) | l <- ['a'..'z'], n <- [0..10]]
      oldNames = Set.fromList (map (fst.fst) subs ++ map fst (tv ex))
  in case Set.toList $ allNames `Set.difference` oldNames of
        (x:_) -> x
        _     -> error "Error - no more fresh names!"

  
dbg :: String -> Expr b -> Either T.Text (Expr Type, Subst) -> Either T.Text (Expr Type, Subst)
dbg prefix exp a = trace (prefix <> ": " <> show (prettyExpr exp) <> " :: " <> typeString) a
  where typeString = case a of
                       Left e -> "<<Error: " <> T.unpack e <> ">>"
                       Right (e,_) -> show (prettyType $ exprType e)

-----------------------------------------------------------------------
typeInfer :: Subst -> Expr a -> Either T.Text (Expr Type, Subst)

typeInfer s l@(ELit a lit) = dbg "lit" l $ let t = litType lit
                             in Right (apply s (ELit (litType lit) lit), s)

typeInfer s v@(EVar _ n) = dbg "var" v $ let t = (apply s (TVar n K.Type))
                                         in Right (apply s (EVar t n), s)

typeInfer s l@(ELambda _ n body) = dbg "lam" l $
  case trace "TC Body:" $ typeInfer s body of
    Right (bodyTyped, bodySubs) -> case findVar n bodyTyped of
      Nothing ->
        let t = TVar (findFreshTName bodyTyped (s @@ bodySubs)) K.Type `fn` exprType bodyTyped
        in trace "VAR NOT FOUND" $ return (ELambda t n bodyTyped, bodySubs)
      Just (EVar varType n) ->
        let t = trace "VAR FOUND" $ varType `fn` exprType bodyTyped
            subs' = s @@ bodySubs
        in  Right (apply subs' (ELambda t n bodyTyped), s @@ bodySubs)
    Left err -> Left err

typeInfer s e@(EApp _ f a) = dbg "app1" e $ do
  (fTyped, fSubs) <- typeInfer s f
  (aTyped, aSubs) <- typeInfer (s @@ fSubs) a
  let aTy = exprType aTyped
      fTy = exprType fTyped
  let allSubs = s @@ fSubs @@ aSubs
  case traceShow (prettyType fTy) $ traceShow (prettyType aTy) $ fTy of
    TApp (TApp (TCon TCFun _) fParam) fBody -> do
      sUnify <- mgu aTy fParam
      let subs' = sUnify @@ fSubs @@ aSubs
      return (apply subs' (EApp fBody fTyped aTyped), sUnify @@ fSubs @@ aSubs)
    TVar varName _ -> let resTypeName = findFreshTName (EApp fTy fTyped aTyped) allSubs :: T.Text
                          resType = TVar resTypeName K.Type :: Type
                          funType = aTy `fn` resType :: Type
                          subs'   = allSubs @@ ((varName, K.Type) =: funType)
                      in  Right (apply subs' (EApp resType fTyped aTyped),
                                 subs')
    _ -> Left "Tried to apply non-function to an argument"

typeInfer s pr@(EPrim _ p) = dbg "prim" pr $ return (EPrim (primType p) p, s)

-----------------------------------------------------------------------
-- Temporary helper function
tcRun :: T.Text -> IO ()
tcRun a = case parseExpr a of
  Left err   -> error . T.unpack $ "Parse error: " <> err
  Right expr -> case typeInfer mempty expr of
    Left err -> putStrLn $ T.unpack err
    Right (tyed,subs) ->
      putStrLn . T.unpack . T.unlines $
      ["" ,"Type:" ,T.pack (show $ prettyType $ exprType tyed)
      ,"" ,"Expr:" , T.pack (show $ prettyExpr tyed)
      ,"" ,"Subs:" , showSubst subs
      ]



litType :: Val -> Type
litType (VDouble _) = TCon TCDouble K.Type
litType (VImage _) = TCon TCImage K.Type
litType (VText _) = TCon TCText K.Type
litType _ = error "unimplemented" -- TODO: finish these

primType :: Prim -> Type
primType p = case p of
  P1Negate -> tDouble `fn` tDouble
  P1Not    -> tBool   `fn` tBool
  P1Succ   -> tDouble `fn` tDouble
  P2Sum    -> tDouble `fn` (tDouble `fn` tDouble)



-- | Assume the expression is the application of one variable name to another variable name
--   Lookup the argument's type from the (assumed-known) type of the function
dumbCheck :: Map.Map T.Text (Expr Type) -> Expr a -> Either T.Text (Map.Map T.Text (Expr Type), Type, Type)
dumbCheck env (EApp _ (EVar _ funcName) (EVar _ argName)) =
  case Map.lookup funcName env of
    Just (ERemote (TApp (TApp (TCon TCFun _) typeA ) typeB) _) -> Right (Map.insert argName (EVar typeA argName) env, typeA, typeB)
    Just t                                   -> Left $ "Expected function "
                                                <> funcName <> " to be a function, actual type: "
                                                <> T.pack (show t)
    _                                        -> Left $ "No function " <> funcName <> " found."
dumbCheck _ _ = Left "dumbCheck only works on (EApp x y)"

-- | Assume the expression is the application of one variable name to another variable name
--   Lookup the argument's type from the (assumed-known) type of the function
dumbCheck' :: Map.Map T.Text (Expr Type) -> Expr a -> Either T.Text (Map.Map T.Text (Expr Type), Expr Type)
dumbCheck' env (EApp _ (EVar _ funcName) (EVar _ argName)) =
  case Map.lookup funcName env of
    Just (ERemote (TApp (TApp (TCon TCFun sas) typeA) typeB) r) -> Right (Map.insert argName (EVar typeA argName) env,
                                                      (EApp typeB (ERemote (TApp (TApp (TCon TCFun sas) typeA) typeB) r) (EVar typeA argName)))
    Just t                                   -> Left $ "Expected function "
                                                <> funcName <> " to be a function, actual type: "
                                                <> T.pack (show t)
    _                                        -> Left $ "No function " <> funcName <> " found."
dumbCheck' _ _ = Left "dumbCheck only works on (EApp _ (ELambda _ _ _) y)"
