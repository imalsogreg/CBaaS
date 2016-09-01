{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Parse (
  parseExpr
  , parseType
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


-- TODO: The type parsing is really broken. Fix & test
parseType :: T.Text -> Either T.Text Type
parseType t = first (T.pack . show) $ P.parse ty "type" (T.unpack t)

parens = P.between (P.char '(') (P.char ')')

ty :: P.Parser Type
ty = P.try ty1 <|> tyNonApp

tyNonApp :: P.Parser Type
tyNonApp = P.try tyArrow -- a -> b
       <|> P.try tyTuple -- (a,b)
       <|> parens ty     -- (a)
       <|> tyVar         -- a
       <|> tyLit         -- Int

tyVar :: P.Parser Type
tyVar = do
  n <- P.lower
  return $ TVar (T.singleton n)

tyArrow :: P.Parser Type
tyArrow = do
  ta <- tyTuple <|> tyLit
  P.spaces
  P.string "->"
  P.spaces
  tb <- ty
  return $ TFunction ta tb

tyLit :: P.Parser Type
tyLit = let aux t = P.try (P.string (drop 1 $ show t) >> return t)
        in  L.foldl1' (<|>) $ map aux [TDouble, TComplex, TText, TModelImage]

ty1 :: P.Parser Type
ty1 = TyApp <$> tyCon1 <*> ty


tyCon1 :: P.Parser (Type)
tyCon1 = P.try (P.string "TVec" >> return TVec)

tyTuple :: P.Parser Type
tyTuple = P.between (P.char '(') (P.char ')')
  (do
      P.spaces
      t1 <- ty
      P.spaces
      P.char ','
      P.spaces
      t2 <- ty
      P.spaces
      return $ TTuple t1 t2
  )
