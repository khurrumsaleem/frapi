#!/bin/bash

fraptran=../../build/debug/main_fraptran
frapi=../../build/debug/test_frapi

#gdb --command script.gdb --args $fraptran nru-mt1.inp
gdb --command script.gdb --args $frapi fraptran nru-mt1.inp '' nru-mt1-out.txt

