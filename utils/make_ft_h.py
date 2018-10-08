#!/usr/bin/python

import re
import os, sys

exceptions = """
btuhkw        
wmktobsftf    
mpatopsi      
btulbtojkg    
psift2        
psitolbft2    
psitopa       
patopsi       
jkgtobtulb    
btulbftojkgk  
avogadro      
gravity_si    
psitompa      
powcnv        
r_in3psirlbmol
r_jmolk       
ftin          
psitokpa      
ftom          
psitoatm      
wmktobhftf    
ftmetr        
psitokgcm2    
pi            
jkgktobtulbf  
psinm2        
kgcm2topsi    
speedlight    
hgpcnv        
boltzmann     
gtolb         
atmtopsi      
bhftftowmk    
idx
btuhkw
sechr
bhftftowmk
table1
table2
table3
is_export
""".split()[1:]

vars = {}
with open('ft_all_targets.f90', 'r') as f:
    lines = f.read().split('\n')
    for line in lines:
        if '::' in line:
            a,b = line.split('::')
            b = b.split()[0].lower()
            if not b in exceptions:
                vars[b] = a.lower()

ch_0, ch_1, bn_0, bn_1, r8_0, r8_1, r8_2, r8_3, i4_0, i4_1, i4_2, i4_3 = {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}

for var in vars:
    tp = vars[var]
    if 'logical' in tp and not 'dimension' in tp:
        bn_0[var] = "{:<70}".format('logical')
    elif 'logical' in tp and 'dimension(:)' in tp:
        bn_1[var] = "{:<26}, {:<20}, {:<20}".format('logical', 'dimension(:)', 'allocatable')
    elif 'character' in tp and not 'dimension' in tp:
        length = tp.split('=')[1].split(')')[0]
        ch_0[var] = "{:<70}".format('character(len=%s)'%length)
    elif 'character' in tp and 'dimension' in tp and '(:)' in tp:
        length = tp.split('=')[1].split(')')[0]
        ch_1[var] = "{:<26}, {:<20}, {:<20}".format('character(len=%s)'%length, 'dimension(:)', 'allocatable')
    elif 'real' in tp and not 'dimension' in tp:
        r8_0[var] = "{:<70}".format('real(r8k)')
    elif 'real' in tp and 'dimension'in tp and '(:)' in tp:
        r8_1[var] = "{:<26}, {:<20}, {:<20}".format('real(r8k)', 'dimension(:)', 'allocatable')
    elif 'real' in tp and 'dimension'in tp and '(:,:)' in tp:
        r8_2[var] = "{:<26}, {:<20}, {:<20}".format('real(r8k)', 'dimension(:,:)', 'allocatable')
    elif 'real' in tp and 'dimension'in tp and '(:,:,:)' in tp:
        r8_3[var] = "{:<26}, {:<20}, {:<20}".format('real(r8k)', 'dimension(:,:,:)', 'allocatable')
    elif 'integer' in tp and not 'dimension' in tp:
        i4_0[var] = "{:<70}".format('integer(ipk)')
    elif 'integer' in tp and 'dimension'in tp and '(:)' in tp:
        i4_1[var] = "{:<26}, {:<20}, {:<20}".format('integer(ipk)', 'dimension(:)', 'allocatable')
    elif 'integer' in tp and 'dimension'in tp and '(:,:)' in tp:
        i4_2[var] = "{:<26}, {:<20}, {:<20}".format('integer(ipk)', 'dimension(:,:)', 'allocatable')
    elif 'integer' in tp and 'dimension'in tp and '(:,:,:)' in tp:
        i4_3[var] = "{:<26}, {:<20}, {:<20}".format('integer(ipk)', 'dimension(:,:,:)', 'allocatable')
    else:
        print tp, var

tplist = [bn_0, ch_0, i4_0, r8_0, bn_1, ch_1, i4_1, r8_1, i4_2, r8_2, i4_3, r8_3]

if False:
    txt = ''
    with open('../fraptran/include/ft_replicants_h.f90', 'w') as f:
        for tp in tplist:
            for var in tp:
                txt += '%s :: r__%s \n' % (tp[var], var)
        f.write(txt)

    txt = ''
    with open('../fraptran/include/ft_load_h.f90', 'w') as f:
        for tp in tplist:
            for var in tp:
                a = "this %% %s" % var
                b = "this %% r__%s" % var
                txt += '{:<30} = {:<30}\n'.format(a,b)
        f.write(txt)

    txt = ''
    with open('../fraptran/include/ft_dump_h.f90', 'w') as f:
        for tp in tplist:
            for var in tp:
                a = "this %% %s" % var
                b = "this %% r__%s" % var
                txt += '{:<30} = {:<30}\n'.format(b,a)
        f.write(txt)

    txt = ''
    with open('../fraptran/include/ft_pointers_h.f90', 'w') as f:
        for tp in tplist:
            for var in tp:
                if 'allocatable' in tp[var]: 
                    tp_ = tp[var].replace(', allocatable','             ')
                else:
                    tp_ = tp[var]
                txt += '%s, pointer :: %s \n' % (tp_, var)
        f.write(txt)

    txt = ''
    with open('../fraptran/include/ft_associate_h.f90', 'w') as f:
        for tp in tplist:
            for var in tp:
                a = "%s" % var
                b = "this %% %s" % var
                txt += '{:<30} => {:<30}\n'.format(b,a)
        f.write(txt)