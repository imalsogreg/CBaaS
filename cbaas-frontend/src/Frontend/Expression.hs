{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Expression where

import qualified Data.Map as Map
import qualified Data.Text as T
import Reflex
import Reflex.Dom

import Model

data ExpressionConfig t = ExpressionConfig
 { _expressionConfig_attributes :: Dynamic t (Map.Map T.Text T.Text)
 , _expressionConfig_environment :: Dynamic t (Map T.Text (Type, Val))
 }

instance Reflex t => Default (ExpressionConfig t) where
  def = constDyn mempty

data Expression t = Expression
  { _expression_text :: Dynamic t T.Text
  , _expression_type :: Dynamic t (Maybe Type)
  }
