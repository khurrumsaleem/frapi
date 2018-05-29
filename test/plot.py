from numpy import *
import matplotlib.pyplot as plt

ref = loadtxt('../../frapcon_standalone/working/1.out').T
table = loadtxt('1.out').T
data, mesh = table[:-1], table[-1]

names = [
'axial fuel temperature, C',
'bulk coolant temperature, C',
'gap conductance, W/(m^2*K)',
'oxide thickness, um',
'mechanical gap thickness, um',
'gap pressure, MPa',
'cladding hoop strain, %',
'cladding axial stress, MPa'
]   

for i, name in enumerate(names):
        fig, ax = plt.subplots()
        ax.plot(mesh, data[i], '-o', ms=10, lw=2, alpha=0.7, mfc='orange')
        ax.plot(mesh, ref[i], '-o', ms=10, lw=2, alpha=0.7, mfc='red')
        ax.set_xlabel('Axial elevation, cm')
        ax.set_ylabel(name)
        ax.legend(['NEW FRAPCON', 'OLD FRAPCON'])
        ax.grid()
    #plt.savefig('../doc/figs/%s.png'%name.split(',')[0].replace(' ','_'))

plt.show()
