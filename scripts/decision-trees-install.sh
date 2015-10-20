#!/bin/bash

GIT_Weka_Data="https://github.com/fehu/min-dat--weka-data.git"
GIT_Decision_Trees="https://github.com/fehu/min-dat--decision-trees.git"

ROOT_DIR=`pwd`

build () {
    cabal configure
    cabal install --enable-tests --dependencies-only
    cabal build
    }


git clone $GIT_Weka_Data WekaData
git clone $GIT_Decision_Trees DecisionTrees

cd "$ROOT_DIR/WekaData"
build

cd "$ROOT_DIR/DecisionTrees"

cabal sandbox init
cabal install "$ROOT_DIR/WekaData"
build


