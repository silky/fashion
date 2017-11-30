#!/bin/zsh
for i in {1..200}
do
    echo $i
    stack exec simple-geometry -- -w 100 -o output/$i.svg
done
