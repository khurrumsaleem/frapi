#!/usr/bin/python 
# -*- coding:utf-8 -*-

from subprocess import call

for x in "IFA-515-10rA1".split():
    call(['../../utils/test.py', x])