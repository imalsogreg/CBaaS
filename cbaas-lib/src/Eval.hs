{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text as T
import Model
import Job

type JobThunk = Either WorkerProfile JobResult

countRemotes :: Map.Map T.Text (Expr a) -> Expr a -> Int
countRemotes env e = case e of
  ELit _ _ -> 0
  EVar _ varName -> maybe 0 (countRemotes env) (Map.lookup varName env)
  ELambda _ _ b  -> countRemotes env b
  ERemote _ _    -> 1
  EApp _ f a -> countRemotes env f + countRemotes env a
  EPrim1 _ _ e -> countRemotes env e
  EPrim2 _ _ a b -> countRemotes env a + countRemotes env b

-- evalStepSimple :: MonadIO m
--                => ((WorkerProfileId, WorkerProfile, T.Text) -> m (Either T.Text (Expr a)))
--                -> Map.Map T.Text (Expr a)
--                -> Expr a
--                -> m (Either T.Text (Map.Map T.Text (Expr a), Expr a))
-- evalStepSimple runRemote env e = case e of
--   l@(ELit _ _) -> return (Right (env, l))
--   EVar _ vName ->
--     maybe (return $ Left $ "No variable found named " <> vName)
--           (\e -> return $ Right (env,e))
--           (Map.lookup vName env)
--   lam@(ELambda _ _ _) -> return (Right (env, lam))
--   ERemote _ r -> do
--     v <- runRemote r
--     case v of
--       Left e     -> return $ Left $ "Error running remote worker: " <> e
--       Right exp' -> return $ Right (env, exp')
--   EApp (ELambda _ pName bod) a = substitute pName a bod
--   EPrim1 _ prim a = case a of
--     ELit v -> return $ Right (env, evalPrim1 prim v)


-- (map "label" "getListOfPictures")

-- evalStep :: Map.Map T.Text (Expr JobThunk)
--          -> Expr JobThunk
--          -> (Map.Map T.Text (Expr JobThunk), Either [WorkerProfile] (Expr JobThunk))
-- evalStep env expr = case expr of
--   EVar _ varName  = note ("Undefined value: " <> varName) (Map.lookup varName env)
--   ELit _ l        = Right $ l
--   ELambda a n bod = Right $ ELambda a n bod -- No progress to be made
--   ERemote t       = case t of
--     Left j 
