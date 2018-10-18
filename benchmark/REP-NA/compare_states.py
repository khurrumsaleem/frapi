#!/usr/bin/python
import os, sys
from subprocess import call
import shutil

def str2float(a):
    try:
        a = float(a.strip())
        b = True
    except ValueError:
        b = False
    return b


skiping = """dtpo
mheat
bheat
t2
printballoon
tem
t22
ncool2
nhbh 
printrodburst
ncall
dtp
nhinta 
tend
dtplta
ntimes
dtmaxa
defsize
tmax
tplot
nhupta 
ndtmax
pbh2
nptha 
tpo
ntprs
dtpoa
ngbh
ntimesteps
time
first_pass
time0
dktoutcounter""".split()


task = 'rep-na1'

call(['../../build/debug/main_fraptran', '%s-fraptran.inp'%task])
shutil.move('./memory.out', './memory-1.out')
call(['../../build/debug/test_frapi', "fraptran", '%s-fraptran.inp'%task, './restart-na1.txt', './%s-out.txt'%task])
shutil.move('./memory.out', './memory-2.out')

with open('memory-1.out','r') as f:
    d = [a.split('=') for a in f.read().split('\n')[:-1]]
    d1 = {x[0].strip() : x[1].strip() for x in d}

with open('memory-2.out','r') as f:
    d = [a.split('=') for a in f.read().split('\n')[:-1]]
    d2 = {x[0].strip() : x[1].strip() for x in d}

for key in d1:
    a, b = d1[key], d2[key]
    #if key == 'time': print key, a, b
    if not a == b and not key in skiping:
        a_ = str2float(a)
        b_ = str2float(b)
        if not (a_ and b_):
            print '%20s %10s %10s' % (key, a, b)
        else:
            a = float(a)
            b = float(b)
            if ( abs(a - b) > 1E-6 * abs(a) ):
                print '%20s %16.6E %16.6E' % (key, a, b)
