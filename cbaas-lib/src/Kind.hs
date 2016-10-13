{-# language ScopedTypeVariables #-}
{-# language DeriveGeneric       #-}
{-# language DeriveAnyClass      #-}

module Kind (
  Kind(..),
  prettyKind,
  parseKind
)where

import           Control.Applicative
import           Control.DeepSeq (NFData)
import           Control.Monad (when)
import qualified Data.Aeson as A
import           Data.Aeson ((.=), (.:))
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid ((<>))
import           GHC.Generics
import qualified Text.Parsec.Text as P
import qualified Text.Parsec as P
import           Text.PrettyPrint ((<+>))
import qualified Text.PrettyPrint as PP
import qualified Text.PrettyPrint.HughesPJClass as PP

data Kind = Type | TyFun Kind Kind
  deriving (Eq, Ord, Show, Read, Generic, A.ToJSON, A.FromJSON, NFData)

prettyKind :: Kind -> PP.Doc
prettyKind Type = PP.text "*"
prettyKind (TyFun f a) = argDoc <+> PP.text "->" <+> prettyKind a
  where argDoc = case f of
          Type -> prettyKind f 
          _    -> PP.parens (prettyKind f) 

parseKind :: P.Parser Kind
parseKind = P.try parseTyFun <|> parseKindAtom

parseKindAtom :: P.Parser Kind
parseKindAtom = P.between (P.char '(') (P.char ')') parseKind
            <|> (P.char '*' >> return Type)

parseTyFun :: P.Parser Kind
parseTyFun = do
  atoms <- parseKindAtom `P.sepBy` (P.spaces >> P.string "->" >> P.spaces)
  when (length atoms < 1) (fail "Expected at least one kind atom")
  return $  foldr1 TyFun atoms
