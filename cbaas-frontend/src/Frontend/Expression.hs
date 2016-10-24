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
import TypeCheck hiding ((=:))
import EntityID
import WorkerProfile

data ExpressionConfig t = ExpressionConfig
 { _expressionConfig_textboxAttributes :: Dynamic t (Map.Map T.Text T.Text)
 , _expressionConfig_initialEnvironment :: Map.Map T.Text (Expr Type)
 , _expressionConfig_updateEnvironment :: Event t (Map.Map T.Text (Maybe (Expr Type)))
 , _expressionConfig_workers :: Dynamic t WorkerProfileMap
 , _expressionConfig_setText :: Event t T.Text
 , _expressionConfig_evalStatus :: Dynamic t EvalStatus
 }

instance Reflex t => Default (ExpressionConfig t) where
  def = ExpressionConfig (constDyn mempty) mempty never (constDyn $ EntityMap mempty) never (constDyn EvalOk)

data Expression t = Expression
  { _expression_text :: Dynamic t T.Text
  , _expression_expr :: Dynamic t (Either T.Text (Expr Type))
  , _expression_go :: Event t ()
  }

data EvalStatus = EvalOk | EvalInvalid | EvalWorking
  deriving (Eq)

mapUnionWithSpace :: (Ord k) => Map.Map k T.Text -> Map.Map k T.Text -> Map.Map k T.Text
mapUnionWithSpace = Map.unionWith (\a b -> a <> " " <> b)

expression :: forall t m.(DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace) => ExpressionConfig t -> m (Expression t)
expression (ExpressionConfig textAttrs env0 dEnv workers setText valid) =
  divClass "expression-widget" $ divClass "ui icon input" $ do
    rec exprText <- value <$> textInput
          (TextInputConfig { _textInputConfig_inputType = "text"
                           , _textInputConfig_initialValue = ""
                           , _textInputConfig_setValue = setText
                           , _textInputConfig_attributes = zipDynWith mapUnionWithSpace textAttrs internalAttrs
                           })

        let btnAttrs =
              ffor valid $ \v -> let c = case v of
                                       EvalOk -> "circular inverted send link icon"
                                       EvalInvalid -> " disabled send icon"
                                       EvalWorking -> "circular inverted loading spinner link icon"
                                 in ("class" =: c)
        (btn,_) <- elDynAttr' "i" btnAttrs blank
        env <- foldDyn applyMap env0 dEnv

        let expr  = parseExpr <$> exprText
            typedExpr = fmap snd <$> zipDynWith (\env e -> e >>= (dumbCheck' env) ) env expr
            goodExpr = isRight <$> typedExpr -- liftA2 (&&) (isRight <$> expr) (isRight <$> etype)
            internalAttrs = ffor goodExpr $ bool
              ("style" =: "box-shadow: 0px 0px 5px rgba(255,0,0,0.5);" <> "placeholder" =: "label #1")
              mempty

    return $ Expression exprText typedExpr (domEvent Click btn)
