#!/bin/bash

# usage: ./mk-templates.sh SET
#
# For SET equals 1, 2, 3, etc.

################################################################
if [[ $# -ne 1 ]]; then
  echo "usage: $0 SET" >> /dev/stderr
  exit 2
fi
set=$1
# There are 8 challenges per set, and first challenge is 1.
challenges=($(seq $(($set * 8 - 7)) $(($set * 8))))
mkdir -p Set$set
for c in ${challenges[*]}; do
  out=Set$set/C$c.hs
  echo '{-' >> $out
  pandoc http://cryptopals.com/sets/$set/challenges/$c --to markdown >> $out
  echo '-}' >> $out
  echo >> $out
  echo "module Set$set.C$c () where" >> $out
done
