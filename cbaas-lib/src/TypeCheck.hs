{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypeCheck (
    typeCheck
  , dumbCheck
  , dumbCheck'
  ) where

import Data.Monoid ((<>))
import qualified Data.Map as Map
import qualified Data.Text as T
import Model
import Type

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


-- Need to do some more reading!
typeCheck :: Map.Map T.Text Type -> Expr a -> Either T.Text (Expr Type)
typeCheck env expr = undefined

stepChecker :: Map.Map T.Text Type -> Expr (Maybe Type) -> (Map.Map T.Text Type, Expr (Maybe Type))
stepChecker = undefined
