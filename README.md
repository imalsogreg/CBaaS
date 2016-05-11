cbaas
=====

[Center for Brains Minds and Machines](http://cbmm.mit.edu) as a Service. Test your stimulus against community algorithms. Test your algorithm against community stimuli. See what brains think.

What is it?
-----------

An open-world network for composing research results. cbaas will allow you to expose your research artifact to the internet, so that others can test it with new data, and you can have live access to it during presentations. Composable reseaarch means that if your algorithm produces data that another consumes, you can compose them end-to-end.

Architecture
------------

 - *Worker*: A unit of research expressed as a function. By calling your function from one of the cbaas client libraries, you make it discoverable to the cbaas website as a service. Users can send your code arguments (e.g. pictures, spike trains), and your experimental code can send back responses (e.g. classifications, spike train decodings).
 - *User*: A visitor to the cbaas website, or a user of cbaas browsing code in a slide deck. Users can send queries to individual *worker* functions. Additionally, users can write cbaas expressions that compose *worker* functions together in ways that the *workers'* authors may not have anticipated
 - *Server*: A cbaas server manages the flow of information between *users* and *workers*. It caches *workers'* responses to speed up subsequent calls with matching arguments, and handles permissions and rate-limiting.

Is it ready?
------------

Nope! But we're working hard right now on the core features and documentation. We hope to have a working demo ready soon.


API
---

Access CBaaS services from your favorite language: MATLAB/Octave, Python, Julia, Javascript, Haskell

Building
--------

Server dependencies: PostgreSQL, Haskell, Snap, Servant, icu, zlib

## Ubuntu 14.04

```bash
sudo apt-get install build-essential postgresql ghc libicu-dev zlib1g-dev
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

Deploying/Running
-----------------

## Ubuntu 14.04

```Ubuntu
# This script copies the server executable and static assests by scp
./deploy.sh your_key_pair.pem user@example.com

# Set up the sql server
ssh -i your_key_pair.pem user@example.com
sudo apt-get install postgresql-client postgresql postgresql-contrib
```

sudo apt-get install 
Forking
-------

Fork, modify, and rebrand as you like (although there's nothing here yet). CBaaS is [BSD-3](http://opensource.org/licenses/BSD-3-Clause) licensed.

