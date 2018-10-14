#!/usr/bin/python 
# -*- coding:utf-8 -*-
import os
from subprocess import call
from h5py import File
import matplotlib.pyplot as plt
from numpy import array

task = 'rep-na1'


#call(['../../build/debug/main_frapcon', 'rep-na1-frapcon.inp'])
call(['../../build/debug/main_fraptran', 'rep-na1-fraptran.inp'])
call(['../../build/debug/test_frapi', "fraptran", '%s-fraptran.inp'%task, './restart-na1.txt', './%s-out.txt'%task])
call(["../../utils/fraptran2h5.py", "%s-fraptran.plot"%task, "%s-fraptran.h5"%task])
call(["../../utils/frapi2h5.py", "%s-out.txt"%task, "%s-frapi.h5"%task])

names = [
#'delth',
#'gapthick',
'rinterfacgap',
#'dcldh',
#'bup',
#'frpo2',
#'totalvoidvol',
#'chstrs',
#'frbal',
#'pmxbal',
#'r8bal',
#'tcebal',
#'pdrato',
#'rnbnt',
#'totnb'
]


def draw(filename):
    f0 = File('%s-frapi.h5'%filename)
    f1 = File('%s-fraptran.h5'%filename)

    dirname = '../../doc/graphics'
    if not os.path.isdir(dirname): 
        os.mkdir(dirname)

    dirname = '../../doc/graphics/%s'%filename
    if not os.path.isdir(dirname): 
        os.mkdir(dirname)

    print "%50s %10s %10s %10s %10s"%('Parameter', 'Units', 'RMS, %', 'MAX, %', 'Result')

    for i, name in enumerate(names):
        fig, ax = plt.subplots()

        time0 , data0 = [], []
        for group in f0.keys():
            time0.append(f0[group]['frapi time, s'][0])
            data0.append(f0[group][name][0])

        time1 , data1 = [], []    
        for group in f1.keys():        
            time1.append(f1[group]['time, s'])
            data1.append(f1[group][name][0]) 

        ax.plot(time0, data0, '-o', ms=1, lw=2, alpha=0.7, mfc='orange')
        ax.plot(time1, data1, '-o', ms=1, lw=2, alpha=0.7, mfc='blue')
        ax.set_xlabel('time, s')
        ax.set_ylabel(name)
        ax.legend(['FRAPI', 'FRAPTRAN'])
        ax.grid()

        plt.show() #savefig('%s/%s.png'%(dirname, name.split(',')[0].replace(' ','_')))
        exit()

        data0 = array(data0)[:]
        data1 = array(data1)[:]
        eps = 1.e-6
        a = (data0 + eps)/(data1 + eps) - 1
        errmax = 100 * max(abs(a))
        errrms = 100 * pow(pow(a, 2).mean(), 0.5)
        if errmax < 5 and errrms < 1: 
            res = 'OK'
        else:
            res = 'ERROR'
        name, units = name.split(',')
        print "%50s %10s %10.1f %10.1f %10s"%(name, units, errrms, errmax, res)

    f0.close()
    f1.close()


draw(task)