#!/bin/bash
cd ./build
rm test2
make
cd ..
test2=./build/test2
$test2 ./data.inp ./data.out