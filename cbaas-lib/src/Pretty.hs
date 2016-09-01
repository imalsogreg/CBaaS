module Pretty where

import Data.Monoid ((<>))
import qualified Data.Text as T
import Text.PrettyPrint ((<+>))
import qualified Text.PrettyPrint as P
import qualified Text.PrettyPrint.HughesPJClass as P

import Model

pText = P.text . T.unpack

prettyExpr :: Expr a -> P.Doc
prettyExpr e = case e of
  ELit _ v -> prettyVal v
  EVar _ n -> pText n
  ELambda _ n b -> P.hsep [P.char '\\' <> pText n, P.text "->", prettyExpr b]
  ERemote _ (_,_,n) -> pText n
  EApp _ f a -> P.hsep [prettyExpr f, prettyExpr a]
  EPrim1 _ p a -> P.text (show p) <> prettyExpr a
  EPrim2 _ p a b -> prettyExpr a <+> P.text (show p) <+> prettyExpr b

instance P.Pretty (Expr a) where
  pPrint = prettyExpr

prettyVal :: Val -> P.Doc
prettyVal v = case v of
  VImage _ -> P.text "<<Image>>"
  _        -> P.text $ show v -- TODO: Better pretty printing

instance P.Pretty Val where
  pPrint = prettyVal

prettyType :: Type -> P.Doc
prettyType t = case t of
  TyApp a b -> prettyType a <+> prettyType b
  TVar t    -> pText t
  TFunction  a b -> prettyType a <+> P.text "→" <+> prettyType b
  TTuple a b -> P.char '(' <> prettyType a <> P.char ',' <> prettyType b <>  P.char ')'
  _ -> P.text (drop 1 $ show t)

instance P.Pretty Type where
  pPrint = prettyType
