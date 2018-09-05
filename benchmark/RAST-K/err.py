from numpy import *

ref = loadtxt('ref.out').T
table = loadtxt('data.out').T
data, mesh = table[:-1], table[-1]

names = [
'axial fuel temperature, C',
'bulk coolant temperature, C',
'total gap conductance, W/(m^2*K)',
'oxide thickness, um',
'mechanical gap thickness, um',
'gap pressure, MPa',
'cladding hoop strain, %',
'cladding axial stress, MPa'
]   

eps = 1.E-10
print("%40s %10s %10s"%('name', 'rms', 'max'))
for i, name in enumerate(names):
    dx = ( abs(data[i]-ref[i]) + eps ) / (abs(ref[i]).mean() + eps)
    emax = 100 * dx.max()
    erms = 100 * pow(pow(dx,2).mean(), 0.5)
    print("%40s: %10.1f %10.1f"%(name, erms, emax))