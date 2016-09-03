{-# LANGUAGE CPP #-}

module Utils (

#ifndef ghcjs_HOST_OS
  ghCodeGen,
#endif

  hush,
  note,
  readMaybe

  ) where

import Database.Groundhog.TH
import Text.Read (readMaybe)

#ifndef __GHCJS__
ghCodeGen :: CodegenConfig
ghCodeGen = defaultCodegenConfig
    { namingStyle = lowerCaseSuffixNamingStyle }
#endif

note :: e -> Maybe a -> Either e a
note e Nothing  = Left e
note e (Just a) = Right a

hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _         = Nothing
