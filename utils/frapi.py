#!/usr/bin/python

import re
import os, sys

base = {}
with open('frapi.txt', 'r') as f:
    lines = f.read().split('\n')
    for line in lines:
        d = line.split()[-1].split('%')
        base[d[2].lower()] = d[1]

if False:
    checklist = 'case include fraptran_rod dfcon subroutine call'.split()
    txt = ''
    with open('frapi.f90', 'r') as f:
        lines = f.read().split('\n')
        for line in lines:
            for key in base.keys():
                if 'this %' in line and all([not s in line for s in checklist]):
                    line = line.lower()
                    if key in line:
                        line = line.replace(key, 'fraptran_rod(1) %% %s %% %s'%(base[key],key))
            txt += "%s\n"%line

    with open('../frapi/frapi.f90', 'w') as f:
        f.write(txt)


if False:
    txt = ''
    with open('ft_pointers_h.f90', 'r') as f:
        lines = f.read().split('\n')
        for line in lines:
            key = line.split('::')[1].split()[0].lower()
            if not key in base.keys():
                txt += "%s\n"%line

    with open('ft_pointers_updated.f90', 'w') as f:
        f.write(txt)