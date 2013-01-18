#!/bin/sh

cd src
ghc --make -outputdir=build dlc.hs -o ../dlc
