# The Bessel functions

import numpy as n
import scipy.special as s
import matplotlib.pyplot as p

#x = range(-50,51)
#p.imshow(n.ufunc.outer(s.jn,x,x),origin='lower')
#p.show()

x = n.linspace(-10, 10, 512)
y = n.linspace(-10, 10, 512)
x, y = n.meshgrid(x,y)
r = n.sqrt(x*x+y*y)
c = (x+1j*y)/r

b = {}
for k in xrange(-8, 8):
    b[k] = s.jn(k, r)*c**k

t = 3
tr = n.sqrt((x+t)*(x+t)+y*y)
tc = ((x+t)+1j*y)/tr
tb = {}
for k in xrange(-8, 8):
    tb[k] = s.jn(k, tr)*tc**k

print b[1]
#print sum([

terms = [s.jn(-k, t)*b[2+k] for k in xrange(-4, 5)]

#p.subplot(331)
#p.imshow(n.real(b[2]), vmin=-0.5, vmax=0.5)

#p.subplot(332)
p.imshow(n.real(tb[2]), vmin=-0.5, vmax=0.5, cmap='seismic')
p.contour(n.real(tb[2]), colors='gray')
p.axis('off')
p.savefig("sum.png")
p.cla()

for k, term in enumerate(terms):
    p.imshow(n.real(term), vmin=-0.5, vmax=0.5, cmap='seismic')
    p.contour(n.real(term), colors='gray')
    p.axis('off')
    p.savefig("term-%d.png" % k)
    p.cla()

#p.subplot(339)
#p.imshow(n.real(sum(terms)), vmin=-0.5, vmax=0.5)
#p.show()
