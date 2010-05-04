#!/bin/bash -e
OUTFILE=a.out
INFILE=Main.hs
echo Compressing $INFILE '->' $OUTFILE
(cat loader.dat;gzip -9 -c $INFILE) >$OUTFILE
chmod a+x $OUTFILE
echo Compression: $(stat -f %z $INFILE) bytes '->' $(stat -f %z $OUTFILE) bytes.
