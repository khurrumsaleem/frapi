#!/bin/bash

dirs='envrl frap lib matpro mech'
modules='zrmodels zirconium void uraniumdioxide timestep mox plenumspring functions deformation'

for name in $dirs; do
    for module in $modules; do
        './mod_rename.py' -d '../fraptran/'$name -o $module -n $module'_fraptran'
    done
done

#mod_rename.py -d ../fraptran/envrl

#ZrModels
#Zirconium
#Void
#UraniumDioxide
#TimeStep
#Mox
#plenumspring
#functions
#deformation