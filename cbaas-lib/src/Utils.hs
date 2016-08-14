{-# LANGUAGE CPP #-}

module Utils where

import Database.Groundhog.TH

#ifndef __GHCJS__
ghCodeGen :: CodegenConfig
ghCodeGen = defaultCodegenConfig
    { namingStyle = lowerCaseSuffixNamingStyle }
#endif
