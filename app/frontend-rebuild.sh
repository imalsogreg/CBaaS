#!/usr/bin/env bash
cd ../cbaas-frontend
../deps/reflex-platform/work-on ./tr.nix ./. --command "cabal configure --ghcjs && cabal build && cp -r dist/build/*/*.jsexe ../app/static/media/js/"
cd ../app
