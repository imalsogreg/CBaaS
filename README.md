CBaaS
=====

* Strictly a work-in-progress. There is no code here yet, only ideas *

[Center for Brains Minds and Machines](http://cbmm.mit.edu) as a Service. Test your stimulus against community algorithms. Test your algorithm against community stimuli. See what brains think.

Use
---

 - Browse stimuli, classifiers, and data on the website
 - Use the [API](TODO docs) to fetch and classify stimuli
 - Get feature vectors for stimuli from standard visual models

API
---

Access CBaaS services from your favorite language: MATLAB/Octave, Python, Julia, Javascript, Haskell

Building
--------

Server dependencies: PostgreSQL, Haskell, Snap, Servant, icu

## Ubuntu 14.04

```bash
sudo apt-get install postgresql ghc libicu-dev
export PATH=$HOME/.cabal/bin:$PATH
git clone http://github.com/CBMM/CBaaS
git submodule update --init --recursive
./init-sandbox.sh
cabal install --only-dep
cabal build
```

## Mac with Homebrew
```bash
brew install postgresql
brew install icu4c
git clone http://github.com/CBMM/CBaaS
git submodule update --init --recursive
./init-sandbox.sh
cabal install text-icu --extra-lib-dirs=/usr/local/opt/icu4c/lib --extra-include-dirs=/usr/local/opt/icu4c/include
cabal install --only-dep
cabal build
```

Forking
-------

Fork, modify, and rebrand as you like (although there's nothing here yet). CBaaS is [BSD-3](http://opensource.org/licenses/BSD-3-Clause) licensed.

