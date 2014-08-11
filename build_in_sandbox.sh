#!/bin/bash

PS4='($LINENO)+ '
set -x
set -e

rm -fr .cabal-sandbox cabal.sandbox.config dist

cabal sandbox init

cabal sandbox add-source ../hs-jose

cabal install --haddock-hyperlink-source --dependencies-only
cabal install --haddock-hyperlink-source
