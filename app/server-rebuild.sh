#!/usr/bin/env bash
cd ../cbaas-server
../deps/reflex-platform/work-on ./tr.nix ./. --command "cabal configure && cabal build && cp dist/build/cbaas-server/cbaas-server ../app"
cd ../app
