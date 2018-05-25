from sympy import *

z = symbols('z')
r = symbols('r')

h = symbols('h')

ya = symbols('ya')
yb = symbols('yb')

ra = symbols('ra')
rb = symbols('rb')

f = ya + (yb - ya) * (rb - r) / (rb - ra)

a = Integral(r*f, (r, ra, rb))

a = a.doit()
a = simplify(a)
a = collect(a, ra*rb)
print a