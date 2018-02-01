# The Bessel functions

import numpy as n
import scipy.special as s
import matplotlib.pyplot as p

x = range(-50,51)
p.imshow(n.ufunc.outer(s.jn,x,x),origin='lower')
p.show()
