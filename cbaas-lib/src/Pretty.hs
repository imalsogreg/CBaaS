module Pretty where

import Data.Monoid ((<>))
import qualified Data.Text as T
import Text.PrettyPrint ((<+>))
import qualified Text.PrettyPrint as P
import qualified Text.PrettyPrint.HughesPJClass as P

import Model hiding (pText)

pText = P.text . T.unpack

prettyExpr :: Expr a -> P.Doc
prettyExpr e = case e of
  ELit _ v -> prettyVal v
  EVar _ n -> pText n
  ELambda _ n b -> P.hsep [P.char '\\' <> pText n, P.text "->", prettyExpr b]
  ERemote _ (_,_,n) -> pText n
  EApp _ f a -> P.hsep [prettyExpr f, prettyExpr a]
  EPrim _ p -> P.text (show p)

instance P.Pretty (Expr a) where
  pPrint = prettyExpr

prettyVal :: Val -> P.Doc
prettyVal v = case v of
  VImage _ -> P.text "<<Image>>"
  _        -> P.text $ show v -- TODO: Better pretty printing

instance P.Pretty Val where
  pPrint = prettyVal
