name:                CBaaS
version:             0.1.0.0
synopsis:            Center for Brains Minds and Machines as a Service
-- description:         
homepage:            http://github.com/CBMM/CBaaS
license:             BSD3
license-file:        LICENSE
author:              Greg Hale
maintainer:          imalsogreg@gmail.com
-- copyright:           
category:            Web
build-type:          Simple
extra-source-files:  README.md
cabal-version:       >= 1.10

library
  build-depends:       base >=4.8 && <4.9
                     , aeson >= 0.9 && < 0.11
                     , async
                     , base64-bytestring
                     , basic-sop >= 0.2 && < 0.3
                     , bytestring >=0.10 && <0.11
                     , configurator
                     , containers >=0.5 && <0.6
                     , deepseq >=1.4 && <1.5
                     , directory >=1.2 && <1.3
                     , filepath >=1.4 && <1.5
                     , free >= 4.12 && < 4.13
                     , generics-sop >= 0.2 && < 0.3
                     , groundhog >= 0.7.0.3 && < 0.8
                     , groundhog-th
                     , heist >= 1.0 && < 1.1
                     , http-api-data >= 0.2 && < 0.3
                     , JuicyPixels >= 3.2.7 && < 3.8
                     , lens >= 4.8 && < 4.14
                     , map-syntax
                     , monad-control
                     , optparse-applicative
                     , postgresql-simple
                     , snaplet-postgresql-simple >= 1.0 && < 1.1
                     , process >=1.2 && <1.3
                     , servant >= 0.5
                     , servant-docs >= 0.5
                     , servant-js >= 0.5 && < 0.6
                     , servant-matlab >= 0.5 && < 0.6
                     , snap >= 1.0 && < 1.1
                     , snap-core >= 1.0 && < 1.1
                     , mtl >= 2.2.1 && < 2.3
                     , resource-pool
                     , stm
                     , transformers >=0.4 && <0.5
                     , transformers-base
                     , text >=1.2 && < 1.3
                     , time >=1.5 && <1.6
                     , unagi-chan
                     , unix >=2.7 && <2.8
                     , uri-bytestring
                     -- , urlencoded
                     , uuid >= 1.3.11 && < 1.4
                     , vector
                     , websockets >= 0.9 && < 0.10
                     , websockets-snap >= 1.0 && < 1.1
  hs-source-dirs:      src
  exposed-modules:     API
                       APIDocs
                       Browser
                       ClientGen.JS
                       ClientGen.Matlab
                       Combo
                       EntityID
                       Job
                       Message
                       Model
                       Permissions
                       RemoteFunction
                       Semantics
                       Tag
                       User

                       Worker
                       WorkerClient
  build-tools:         hsc2hs
  default-language:    Haskell2010

executable server
 build-depends:       base >= 4.8 && < 4.9
                    , aeson
                    , basic-sop
                    , base64-bytestring
                    , bytestring
                    , CBaaS
                    , configurator
                    , containers
                    , deepseq
                    , directory
                    , filepath
                    , generics-sop
                    , groundhog
                    , groundhog-th
                    , heist
                    , http-api-data
                    , JuicyPixels
                    , lens
                    , map-syntax
                    , monad-control
                    , mtl
                    , postgresql-simple
                    , process
                    , resource-pool
                    , servant >= 0.5
                    , servant-snap >= 0.5 && < 0.6
                    , servant-matlab
                    , snap >= 1.0
                    , snap-core
                    , snap-loader-dynamic >= 1.0 && < 1.1
                    , snap-loader-static  >= 1.0 && < 1.1
                    , snap-loader-static
                    , snap-server >= 1.0
                    , snaplet-postgresql-simple
                    , stm
                    , transformers
                    , transformers-base
                    , text
                    , time
                    , unix
                    , uri-bytestring
                    , uuid
                    , vector
                    , websockets
                    , websockets-snap >= 1.0 && < 1.1
 hs-source-dirs:    exec
 main-is:           ServerMain.hs
 other-modules:     Server.Application
                    Server.Site
                    Server.APIServer
                    Server.Crud
                    Server.Utils
                    Server.WebSocketServer
 ghc-options: -threaded -rtsopts "-with-rtsopts=-N"
 default-language:  Haskell2010

executable pixavg
  hs-source-dirs: exec
  main-is:        PixAvg.hs
  build-depends:  base
                , wreq
                , lens
                , CBaaS
                , JuicyPixels
                , lens
                , optparse-applicative
                , servant
                , websockets
  default-language: Haskell2010
  ghc-options: -threaded -rtsopts "-with-rtsopts=-N"

test-suite spec
  type: exitcode-stdio-1.0
  ghc-options: -Wall -threaded
  hs-source-dirs: test
  main-is: Spec.hs
  build-depends: base == 4.*, 
                 aeson,
                 basic-sop >= 0.2 && < 0.3,
                 CBaaS,
                 generics-sop,
                 hspec >= 2.2 && < 2.3, 
                 hspec-snap >= 1.0 && < 1.1,
                 JuicyPixels,
                 QuickCheck >= 2.8 && < 2.9,
                 text,
                 unordered-containers,
                 vector
