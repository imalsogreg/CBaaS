{-# language CPP                 #-}
{-# language DeriveAnyClass      #-}
{-# language DeriveGeneric       #-}
{-# language FlexibleContexts    #-}
{-# language TemplateHaskell     #-}
{-# language QuasiQuotes         #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}

module Type where

import           Control.Applicative
import           Data.Bifunctor (first)
import           Data.Char (isUpper)
import           Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Text as T

#ifndef ghcjs_HOST_OS
import           Database.Groundhog
import           Database.Groundhog.TH
#endif

import           Control.DeepSeq (NFData)
import qualified Data.Aeson as A
import           GHC.Generics
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import           Text.PrettyPrint ((<+>))
import qualified Text.PrettyPrint as PP
import qualified Text.PrettyPrint.HughesPJClass as PP
import           Utils
import           Web.HttpApiData
import qualified Kind as K


data Type = TCon TyCon K.Kind
          | TVar T.Text K.Kind
          | TApp Type Type
            deriving (Eq, Ord, Show, Read, Generic, A.FromJSON, A.ToJSON)

data TyCon = TCBool
           | TCComplex
           | TCDouble
           | TCText
           | TCLabelProbs
           | TCImage
           | TCUnit
           | TCList
           | TCVec
           | TCFun
           | TCPair
           | TCOther T.Text
  deriving (Eq, Ord, Show, Read, Generic, A.ToJSON, A.FromJSON, NFData)


conKind :: TyCon -> K.Kind
conKind TCFun  = K.TyFun K.Type (K.TyFun K.Type K.Type)
conKind TCPair = K.TyFun K.Type (K.TyFun K.Type K.Type)
conKind TCList = K.TyFun K.Type K.Type
conKind TCVec  = K.TyFun K.Type K.Type
conKind _      = K.Type

newtype KindedType = KType { unKind :: K.Kind }
  deriving (Eq, Show, Read)

#ifndef __GHCJS__ 
mkPersist ghCodeGen [groundhog| 
  - primitive: Type
    converter: showReadConverter 
|] 
#endif 

pText :: T.Text -> PP.Doc
pText = PP.text . T.unpack

tBool   = TCon TCBool K.Type
tUnit   = TCon TCUnit K.Type
tText   = TCon TCText K.Type
tImage  = TCon TCImage K.Type
tDouble = TCon TCDouble K.Type
tList   = TCon TCList (K.TyFun K.Type K.Type)
tArrow  = TCon TCFun (K.TyFun K.Type (K.TyFun K.Type K.Type))
tPair   = TCon TCPair (K.TyFun K.Type (K.TyFun K.Type K.Type))

infix 4 `fn`
fn :: Type -> Type -> Type
a `fn` b = TApp (TApp tArrow a) b

list :: Type -> Type
list a = TApp tList a

pair :: Type -> Type -> Type
pair a b = TApp (TApp tPair a) b

prettyType :: Type -> PP.Doc
prettyType (TApp (TApp (TCon TCFun _) arg) res) =
  modifyArg (prettyType arg) <+> "->" <+> prettyType res
  where modifyArg = case arg of
          (TApp (TApp (TCon TCFun _) _) _) -> PP.parens
          _                                -> id
prettyType (TApp (TApp (TCon TCPair _) arg) res) =
  PP.parens $ prettyType arg <> pText ", " <> prettyType res
prettyType (TCon TCFun _) = pText "(->)"
prettyType (TCon TCPair _) = pText "(,)"
prettyType (TCon tycon _) = pText . T.drop 2 . T.pack . show $ tycon
prettyType (TApp f arg) = prettyType f <+> modifyArg (prettyType arg)
  where modifyArg = case arg of
          (TApp _ _) -> PP.parens
          _          -> id
prettyType (TVar n _) = pText n

instance PP.Pretty Type where
  pPrint = prettyType

-- TODO: The type parsing is really broken. Fix & test 
parseType :: T.Text -> Either T.Text (Type) 
parseType t = first (T.pack . show) $ P.parse ty "type" (T.unpack t) 
 

ty :: P.Parser Type
ty = P.try funTy
 <|> P.try appTy
 <|> atomicTy

appTy :: P.Parser Type
appTy = P.chainl1 (combinePs [atomicTy] <* P.spaces) (return TApp)

funTy :: P.Parser Type
funTy = do
  tF <- combinePs [appTy, atomicTy]
  P.spaces >> P.string "->" >> P.spaces
  tArg <- ty
  return $ tF `fn` tArg


atomicTy :: P.Parser Type
atomicTy = P.try pairTy
       <|> parensTy ty
       <|> conTy

combinePs :: [P.Parser a] -> P.Parser a
combinePs = foldl1 ((<|>))

parensTy :: P.Parser Type -> P.Parser Type
parensTy = P.between (P.char '(' >> P.spaces) (P.spaces >> P.char ')')

pairTy :: P.Parser Type
pairTy = do
  P.char '('
  ty1 <- ty
  P.spaces >> P.char ',' >> P.spaces
  ty2 <- ty
  P.char ')'
  return $ pair ty1 ty2

conTy :: P.Parser Type
conTy = do
  tok <- P.many1 P.alphaNum
  return $
    case readMaybe ("TC" <> tok) of
      Just n  -> TCon n (conKind n)
      Nothing ->
        if   isUpper (head tok)
        then TCon (TCOther $ T.pack tok) K.Type -- Assumption: All unknown type constructors
        else (TVar $ T.pack tok)         K.Type --             and type variables are (:: *)



instance ToHttpApiData (Type) where
  toUrlPiece = T.pack . PP.prettyShow
  toHeader = BS.pack . PP.prettyShow

-- TODO: Parse tuple types
instance FromHttpApiData (Type) where
  parseUrlPiece x = case T.words x of
    (x: "->" : ys) -> do
      tx   <- parseUrlPiece x
      tRet <- parseUrlPiece (T.unwords ys)
      return $ tx `fn` tRet
    [x] -> note "No read in type" (readMaybe $ T.unpack x)
    _   -> Left "No parse for type"
