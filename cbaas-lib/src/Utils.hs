module Utils where

import Database.Groundhog.TH

ghCodeGen :: CodegenConfig 
ghCodeGen = defaultCodegenConfig 
    { namingStyle = lowerCaseSuffixNamingStyle } 
