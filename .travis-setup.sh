#!/bin/sh

# Taken from: https://github.com/commercialhaskell/stack/blob/master/.travis-setup.sh
# The difference is that we only do a stack build

set -eux

travis_retry() {
  cmd=$*
  $cmd || (sleep 2 && $cmd) || (sleep 10 && $cmd)
}

fetch_stack_osx() {
  curl -skL https://www.stackage.org/stack/osx-x86_64 | tar xz --strip-components=1 --include '*/stack' -C ~/.local/bin;
}

fetch_stack_linux() {
  curl -sL https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack';
}

# We need stack to generate cabal files with precise bounds, even for cabal
# builds.
mkdir -p ~/.local/bin;
if [ `uname` = "Darwin" ]; then
  travis_retry fetch_stack_osx
else
  travis_retry fetch_stack_linux
fi

# We need stack to download GHC for stack builds.
travis_retry stack --no-terminal setup;
