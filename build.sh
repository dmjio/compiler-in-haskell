#!/bin/sh

scan_in="src/Scanner/DLCScanner.x"
scan_out="src/Scanner/DLCScanner.hs"
if ( ! [ -e $scan_out ] ) || [ $scan_in -nt $scan_out ]; then
    alex $scan_in -o $scan_out
fi

parser_in='src/Parser/DLCParser.y'
parser_out='src/Parser/DLCParser.hs'
if ( ! [ -e $parser_out ] || [  $parser_in -nt $parser_out ] ); then
    happy $parser_in -o $parser_out
fi

cd src
# ghc -O3 --make -outputdir=build dlc.hs -o ../dlc
ghc --make -outputdir=build dlc.hs -o ../dlc
