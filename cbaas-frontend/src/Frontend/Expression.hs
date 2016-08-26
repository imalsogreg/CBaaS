{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Expression where

import Data.Default
import qualified Data.Map as Map
import qualified Data.Text as T
import Reflex
import Reflex.Dom

import Model
import EntityID
import WorkerProfile

data ExpressionConfig t = ExpressionConfig
 { _expressionConfig_attributes :: Dynamic t (Map.Map T.Text T.Text)
 , _expressionConfig_environment :: Dynamic t (Map.Map T.Text (Type, Val))
 , _expressionConfig_workers :: Dynamic t WorkerProfileMap
 }

instance Reflex t => Default (ExpressionConfig t) where
  def = ExpressionConfig (constDyn mempty) (constDyn mempty) (constDyn $ EntityMap mempty)

data Expression t = Expression
  { _expression_text :: Dynamic t T.Text
  , _expression_type :: Dynamic t (Maybe Type)
  }
