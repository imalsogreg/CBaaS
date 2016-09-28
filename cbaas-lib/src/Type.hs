{-# language CPP               #-}
{-# language DeriveGeneric     #-}
{-# language FlexibleContexts  #-}
{-# language TemplateHaskell   #-}
{-# language QuasiQuotes       #-}
{-# language OverloadedStrings #-}

module Type (
 Type(..),
 prettyType,
 parseType
 ) where

import           Control.Applicative
import           Data.Bifunctor (first)
import           Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Text as T

#ifndef ghcjs_HOST_OS
import           Database.Groundhog
import           Database.Groundhog.TH
#endif

import           GHC.Generics
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import           Text.PrettyPrint ((<+>))
import qualified Text.PrettyPrint as PP
import qualified Text.PrettyPrint.HughesPJClass as PP
import           Utils
import           Web.HttpApiData

data Type = TDouble
          | TComplex
          | TText
          | TLabelProbs
          | TModelImage
          | TList
          | TVec
          | TTuple Type Type
          | TFunction Type Type
          | TVar T.Text
          | TyApp Type Type
  deriving (Eq, Ord, Show, Read, Generic)


#ifndef __GHCJS__ 
mkPersist ghCodeGen [groundhog| 
  - primitive: Type 
    converter: showReadConverter 
|] 
#endif 


pText :: T.Text -> PP.Doc
pText = PP.text . T.unpack

prettyType :: Type -> PP.Doc
prettyType t = case t of
  TyApp a b -> prettyType a <+> prettyType b
  TVar t    -> pText t
  TFunction  a b -> prettyType a <+> PP.text "->" <+> prettyType b
  TTuple a b -> PP.char '(' <> prettyType a <> PP.char ',' <> prettyType b <>  PP.char ')'
  TModelImage -> PP.text "Image"
  _ -> PP.text (drop 1 $ show t)

instance PP.Pretty Type where
  pPrint = prettyType

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
        in  L.foldl1' (<|>) $ map aux [TDouble, TComplex, TText, TLabelProbs] 
 
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


instance ToHttpApiData Type where
  toUrlPiece = T.pack . PP.prettyShow
  toHeader = BS.pack . PP.prettyShow

-- TODO: Parse tuple types
instance FromHttpApiData Type where
  parseUrlPiece x = case T.words x of
    (x: "->" : ys) -> do
      tx   <- parseUrlPiece x
      tRet <- parseUrlPiece (T.unwords ys)
      return $ TFunction tx tRet
    [x] -> note "No read in type" (readMaybe $ T.unpack x)
    _   -> Left "No parse for type"



