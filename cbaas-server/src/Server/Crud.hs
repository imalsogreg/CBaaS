{-# LANGUAGE OverloadedStrings #-}

module Server.Crud where

import Prelude hiding (unlines, unwords)
import Data.ByteString.Char8 (ByteString(..), intercalate, pack, unlines, unwords, unpack)
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


-------------------------------------------------------------------------------
postQuery :: Crud a => Proxy a -> Query
postQuery p =
  let allRows = rowNames permissionParts <> rowNames (tableRows p)
      allQs   = map (const "?") allRows
      cSep    = intercalate ", "
  in  Query . unlines $ ["INSERT INTO (id, " <> cSep allRows <> ")"
                        ,"VALUES (uuid_generate_v4(), " <> cSep allQs   <> ")"]


-------------------------------------------------------------------------------
putQuery :: Crud a => Proxy a -> Query
putQuery p =
  let allRows = rowNames permissionParts <> rowNames (tableRows p)
      allEqs  = map (<> "=?") allRows
      cSep    = intercalate ", "
  in  Query . unlines $ ["UPDATE " <> tableName p
                        ,"SET "    <> cSep allEqs
                        ,"WHERE id=?"
                        ]


-------------------------------------------------------------------------------
deleteQuery :: Crud a => Proxy a -> Query
deleteQuery p = Query ("DELETE FROM " <> tableName p <> " WHERE id=?")



createTableQuery :: Crud a => Proxy a -> Query
createTableQuery p = Query . unlines $
  ["CREATE TABLE " <> tableName p <> " ("]
  <> map (\(RowDescription n t d) -> unwords [n,t,d]) (tableRows p)
  <> [")"]


-------------------------------------------------------------------------------
rowNames :: [RowDescription] -> [ByteString]
rowNames = map _rdName


-------------------------------------------------------------------------------
permissionParts :: [RowDescription]
permissionParts =
  [RowDescription "owner"      "uuid" "NOT NULL REFERENCES owner(id)"
  ,RowDescription "group"      "uuid" "REFERENCES group(id)"
  ,RowDescription "ownerread"  "bool" "NOT NULL DEFALUT TRUE"
  ,RowDescription "ownerwrite" "bool" "NOT NULL DEFAULT TRUE"
  ,RowDescription "groupread"  "bool" "NOT NULL DEFAULT TRUE"
  ,RowDescription "groupwrite" "bool" "NOT NULL DEFAULT TRUE"
  ,RowDescription "publicread" "bool" "NOT NULL DEFAULT TRUE"
  ,RowDescription "ownerwrite" "bool" "NOT NULL DEFAULT TRUE"
  ]

selectPart :: Crud a => Proxy a -> ByteString
selectPart p = intercalate ", " (map _rdName (tableRows p))


canRead :: ByteString
canRead = "((ownerread=true AND owner=?) OR"
       <> " (groupread=true AND group=?) OR"
       <> " publicread=true)"

canWrite :: ByteString
canWrite = "((ownerwrite=true AND owner=?) OR"
        <> " (groupwrite=true AND group=?) OR"
        <> " publicwrite=true)"

