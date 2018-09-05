#!/usr/bin/python 
# -*- coding:utf-8 -*-

from subprocess import call

filenames = """
IFA-515-10rA1
IFA-515-10rA2
IFA-515-10rB1
IFA-515-10rB2
""".split()

for x in filenames:
    call(['../../utils/test.py', x])