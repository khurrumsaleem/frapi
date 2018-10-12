#!/usr/bin/python 
# -*- coding:utf-8 -*-

from subprocess import call
from h5py import File
import matplotlib.pyplot as plt
from numpy import array

task = 'rep-na1'

#call(['../../build/debug/main_frapcon', 'rep-na1-frapcon.inp'])
#call(['../../build/debug/main_fraptran', 'rep-na1-fraptran.inp'])
#call(['../../build/debug/test_frapi', "fraptran", '%s-fraptran.inp'%task, './restart-na1.txt', './%s-out.txt'%task])
#call(["../../utils/frapi2h5.py", "%s-out.txt"%task, "%s-frapi.h5"%task])

names = [
'delth',
'dcldh',
#'bup',
'frpo2',
'totalvoidvol',
#'chstrs',
#'frbal',
#'pmxbal',
#'r8bal',
#'tcebal',
#'pdrato',
#'rnbnt',
#'totnb'
]

def draw(task):
    f0 = File('%s-frapi.h5'%task)

    for i, name in enumerate(names):
        fig, ax = plt.subplots()

        time0 , data0 = [], []
        for group in f0.keys():
            time0.append(f0[group]['frapi time step'][0])
            data0.append(f0[group][name][0])

        ax.plot(time0, data0, '-o', ms=1, lw=2, alpha=0.7, mfc='orange')
        ax.set_xlabel('time, s')
        ax.set_ylabel(name)
        ax.grid()

        plt.show()
        #plt.savefig('%s/%s.png'%(dirname, name.split(',')[0].replace(' ','_')))

    f0.close()

draw(task)