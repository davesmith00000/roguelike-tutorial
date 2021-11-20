#!/usr/bin/env bash

set -e

for i in {1..13}
do
	cd part$i
  echo ">>> Part $i"
  echo $(pwd)
  sbt clean update compile test
  cd ..
done
