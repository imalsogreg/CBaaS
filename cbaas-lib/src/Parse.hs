{-# LANGUAGE OverloadedStrings #-}

module Parse (
  parseExpr
  ) where

import           Control.Applicative ((<|>))
import           Data.Bifunctor (first)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Expr as P
import qualified Text.Parsec.String as P
import qualified Text.Parsec.Token as P
import Model

parseExpr :: T.Text -> Either T.Text (Expr ())
parseExpr t = first (T.pack . show) $ P.parse expr "expression" (T.unpack t)


expr :: P.Parser (Expr ())
expr = lamb <|> P.try app <|> P.buildExpressionParser opTable term

app :: P.Parser (Expr ())
app = do
  t1 <- term
  P.spaces
  tN <- P.sepBy1 term P.spaces
  return $ L.foldl' (EApp ()) t1 tN

tokP = P.makeTokenParser P.emptyDef
opTable = [ [prefix "-" (EPrim1 () P1Negate)
--            ,
             -- prefix "log10" (UPrim1 PLog10),
             -- prefix "log" (UPrim1 PLogE),
             -- prefix "exp10" (UPrim1 PExp10),
             -- prefix "exp" (UPrim1 PExpE),
             -- postfix "dB" (UPrim1 PToDb)]
--          , [binary "^" (UPrim2 PPow) AssocLeft
            ]
          , [binary "*" (EPrim2 () P2Prod) P.AssocLeft
--            , binary "/" (UPrim2 PDiv) AssocLeft
            ]
          , [binary "+" (EPrim2 () P2Sum) P.AssocLeft
--            , binary "-" (UPrim2 PDiff) AssocLeft
--            , binary "->" (UPrim2 PRange) AssocLeft
            ]
          ]

binary name fun assoc = P.Infix (do{ P.reservedOp tokP name; return fun}) assoc
prefix name fun = P.Prefix (do{P.reservedOp tokP name; return fun})
postfix name fun = P.Postfix (do{P.reservedOp tokP name; return fun})

term = P.try (P.between (P.char '(') (P.char ')') expr
       <|> lamb
       <|> lit
       <|> (EVar () . T.pack <$> varName)) <* P.spaces

varName :: P.Parser String
varName = P.identifier tokP -- alphanumeric name
     <|> ((('#':) . show)   -- #n widget index style name
          <$> (P.char '#' >> P.natural tokP))

lamb :: P.Parser (Expr ())
lamb = do
  P.char '\\'
  n <- P.identifier tokP
  P.spaces
  P.string "->"
  P.spaces
  body <- expr
  return $ ELambda () (T.pack n) body

lit :: P.Parser (Expr ())
lit = do
  n <- P.optionMaybe (P.char '-')
  v <- either fromIntegral id <$>
       P.naturalOrFloat (P.makeTokenParser P.emptyDef)
  case n of
    Nothing -> return $ ELit () $ VDouble v
    Just _  -> return (ELit () $ VDouble $ negate v)

