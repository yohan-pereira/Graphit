#!/bin/bash

while [ 1 ]; do
  for i in 1 2 3; do
    for j in 1 2 3; do
      echo -e "graph$i\t$j\t$RANDOM"
    done
  done
  sleep 1
done | nc localhost 6666
