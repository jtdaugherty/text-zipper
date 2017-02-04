#!/usr/bin/env bash

set -e

(cd $HOME && cabal install hspec-discover)
cabal new-build --enable-tests
./dist-newstyle/build/text-zipper-*/build/text-zipper-tests/text-zipper-tests
