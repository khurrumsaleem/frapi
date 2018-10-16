#!/usr/bin/python
import os, sys

with open('1.txt','r') as f:
    d = [a.split('=') for a in f.read().split('\n')]
    d1 = {x[0].strip() : x[1].strip() for x in d}

with open('2.txt','r') as f:
    d = [a.split('=') for a in f.read().split('\n')]
    d2 = {x[0].strip() : x[1].strip() for x in d}

for key in d1:
    a, b = d1[key], d2[key]
    if not a == b:
        print key, a, b