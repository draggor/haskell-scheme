#!/bin/bash

cabal configure -g
cabal build
./st
