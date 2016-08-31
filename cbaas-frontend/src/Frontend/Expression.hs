{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE GADTs               #-}

module Frontend.Expression where

import Control.Applicative
import Control.Monad.Fix (MonadFix)
import Data.Bool
import Data.Default
import Data.Either
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text as T
import Reflex
import Reflex.Dom

import Model
import Parse
import TypeCheck
import EntityID
import WorkerProfile

data ExpressionConfig t = ExpressionConfig
 { _expressionConfig_textboxAttributes :: Dynamic t (Map.Map T.Text T.Text)
 , _expressionConfig_initialEnvironment :: Map.Map T.Text (Expr Type)
 , _expressionConfig_updateEnvironment :: Event t (Map.Map T.Text (Maybe (Expr Type)))
 , _expressionConfig_workers :: Dynamic t WorkerProfileMap
 }

instance Reflex t => Default (ExpressionConfig t) where
  def = ExpressionConfig (constDyn mempty) mempty never (constDyn $ EntityMap mempty)

data Expression t = Expression
  { _expression_text :: Dynamic t T.Text
  , _expression_expr :: Dynamic t (Either T.Text (Expr Type))
  }

mapUnionWithSpace :: (Ord k) => Map.Map k T.Text -> Map.Map k T.Text -> Map.Map k T.Text
mapUnionWithSpace = Map.unionWith (\a b -> a <> " " <> b)

expression :: forall t m.(DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace) => ExpressionConfig t -> m (Expression t)
expression (ExpressionConfig textAttrs env0 dEnv workers) =
  divClass "expression-widget" $ do
    rec exprText <- value <$> textInput def { _textInputConfig_attributes =
                                          zipDynWith mapUnionWithSpace textAttrs internalAttrs }

        env <- foldDyn applyMap env0 dEnv

        let expr  = parseExpr <$> exprText
            typedExpr = fmap snd <$> zipDynWith (\env e -> e >>= (dumbCheck' env) ) env expr
            goodExpr = isRight <$> typedExpr -- liftA2 (&&) (isRight <$> expr) (isRight <$> etype)
            internalAttrs = ffor goodExpr $ bool
              ("style" =: "box-shadow: 0px 0px 5px rgba(255,0,0,0.5);")
              mempty
    display expr
    display typedExpr

    return $ Expression exprText typedExpr
