#!/bin/bash
cd ./build
rm test3
make
cd ..
prog=./build/test3
$prog ./data.inp ./data.out