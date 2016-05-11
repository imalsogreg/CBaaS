{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Utils
  ( keyToInt
  , keyToIntegral
  , intToKey
  , integralToKey
  , ghCodeGen
  , err300
-- #ifndef __GHCJS__
--   , ghConfig
-- #endif
  ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import           Data.Pool
import           Database.Groundhog
import           Database.Groundhog.Core
import           Database.Groundhog.Generic
import           Database.Groundhog.TH
#ifndef __GHCJS__
import           Database.Groundhog.Postgresql
import           Snap.Core (MonadSnap(..), finishWith, setResponseCode)
import           Snap
#else
import           GHC.Int
#endif
------------------------------------------------------------------------------
import qualified Data.UUID as U


-- #ifdef __GHCJS__

-- type family DefaultKey a :: *

-- #else

#ifndef __GHCJS__
pg :: proxy Postgresql
pg = undefined

err300 :: MonadSnap m => String -> m b
err300 e = do
  modifyResponse $ setResponseStatus 300 "ServerError"
  modifyResponse $ setHeader "Content-Type" "text/plain"
  writeBS (BS.pack e)
  getResponse >>= finishWith


#else

err300 :: String -> IO ()
err300 = error

data NilBackend

instance DbDescriptor NilBackend where
  type AutoKeyType NilBackend = Int64
  type QueryRaw    NilBackend = []
  backendName _    = "NilBackend"

pg :: proxy NilBackend
pg = undefined
#endif


-------------------------------------------------------------------------------
keyToInt
    :: (PrimitivePersistField (Key a b))
    => Key a b
    -> Int
keyToInt = keyToIntegral


-------------------------------------------------------------------------------
-- | Convert 'Key' to any integral type.
keyToIntegral
    :: (PrimitivePersistField i, PrimitivePersistField (Key a b))
    => Key a b
    -> i
keyToIntegral =
    fromPrimitivePersistValue pg . toPrimitivePersistValue pg


-------------------------------------------------------------------------------
-- | Type specialized input for type inference convenience.
intToKey
    :: (PrimitivePersistField (Key a b))
    => Int
    -> Key a b
intToKey = integralToKey


-------------------------------------------------------------------------------
-- | Convert any integral type to 'Key'
integralToKey
    :: (PrimitivePersistField i, PrimitivePersistField (Key a b))
    => i
    -> Key a b
integralToKey =
    fromPrimitivePersistValue pg . toPrimitivePersistValue pg


instance PersistField U.UUID where
  persistName _ = "UUID"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField U.UUID where
  toPrimitivePersistValue p u = toPrimitivePersistValue p (show u)
  fromPrimitivePersistValue p x = read (fromPrimitivePersistValue p x)

ghCodeGen :: CodegenConfig
ghCodeGen = defaultCodegenConfig { namingStyle = lowerCaseSuffixNamingStyle }
