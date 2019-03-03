#!/usr/bin/env bash

#set -e

git rm wiki-* trac-ini.md trac-*.md
git commit -m "Drop Trac wiki help resources"

sed -i -e 's%https\?://hackage.haskell.org/trac/ghc/wiki/%%g' *.md
sed -i -e 's%https\?://ghc.haskell.org/trac/ghc/wiki/%%g' *.md
sed -i -e 's%/trac/ghc/wiki/%%g' *.md
git add -u
git commit -m "Fix up Trac wiki links"

# Fix a few import errors
rename gh-c- ghc- * status/*
git add .
git commit -m 'Fix GHC page names'

rename msy-s msys building/preparation/windows/*
git add .
git commit -m 'Fix msys page names'
