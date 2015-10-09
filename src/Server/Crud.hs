{-# LANGUAGE OverloadedStrings #-}

module Server.Crud where

import Prelude hiding (unlines)
import Data.ByteString.Char8 (ByteString(..), intercalate, pack, unlines, unpack)
import Data.Monoid ((<>))
import Data.Proxy
import Database.PostgreSQL.Simple (ToRow, FromRow)
import Database.PostgreSQL.Simple.Types (Query(..))


-------------------------------------------------------------------------------
class (ToRow a, FromRow a) => Crud a where
  tableName   :: Proxy a -> ByteString
  tableRows   :: Proxy a -> [RowDescription]


-------------------------------------------------------------------------------
data RowDescription = RowDescription
  { _rdName  :: ByteString
  , _rdType  :: ByteString
  , _rdExtra :: ByteString
  }


-------------------------------------------------------------------------------
getOneQuery :: Crud a => Proxy a -> Query
getOneQuery p =
  Query . unlines $ ["SELECT " <> selectPart p
                    ,"FROM "   <> tableName p
                    ,"WHERE id=?"
                    ,"AND"
                    ,canRead]


-------------------------------------------------------------------------------
getAllQuery :: Crud a => Proxy a -> Query
getAllQuery p =
    Query . unlines $ ["SELECT "<> selectPart p
                      ,"FROM "  <> tableName p
                      ,"WHERE"
                      ,canRead
                      ]

postQuery :: Crud a => Proxy a -> Query
postQuery p =
  let allRows = rowNames permissionParts <> rowNames (tableRows p)
      allQs   = map (const "?") allRows
      cSep    = intercalate ", "
  in  Query . unlines $ ["INSERT INTO (" <> cSep allRows <> ")"
                        ,"VALUES ("      <> cSep allQs   <> ")"]


rowNames :: [RowDescription] -> [ByteString]
rowNames = map _rdName

-------------------------------------------------------------------------------
permissionParts :: [RowDescription]
permissionParts =
  [RowDescription "owner" "uuid" "REFERENCES owner(id)"
  ,RowDescription "group" "uuid" "REFERENCES group(id)"
  ,RowDescription "ownerread" "bool" "NOT NULL"
  ,RowDescription "ownerwrite" "bool" "NOT NULL"
  ,RowDescription "groupread" "bool" "NOT NULL"
  ,RowDescription "groupwrite" "bool" "NOT NULL"
  ,RowDescription "publicread" "bool" "NOT NULL"
  ,RowDescription "ownerwrite" "bool" "NOT NULL"
  ]

selectPart :: Crud a => Proxy a -> ByteString
selectPart p = intercalate "," (map _rdName (tableRows p))


canRead :: ByteString
canRead = "((ownerread=true AND owner=?) OR"
       <> " (groupread=true AND group=?) OR"
       <> " publicread=true)"

canWrite :: ByteString
canWrite = "((ownerwrite=true AND owner=?) OR"
        <> " (groupwrite=true AND group=?) OR"
        <> " publicwrite=true)"

