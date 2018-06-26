#!/usr/bin/python 
# -*- coding:utf-8 -*-

from subprocess import call

for x in "IFA-432-r1".split():
    call(['../../utils/test.py', x])