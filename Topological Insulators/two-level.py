import numpy
import math

# Solve
# i dpsi/dt = H psi

def v2c(u, v):
    return numpy.array([u, v], dtype = numpy.complex)

def h((x, y, z)):
    return numpy.array([
        [z, x-1j*y],
        [x+1j*y, -z]])

psi = v2c(1, 0)
phase = 1
berry = 1

# total time
T = 1000

if 0:
    p0 = (0, 0, 1)
    p1 = (0, 1, 0)
    p2 = (0, 0, -1)
    p3 = (0, -1, 0)
else:
    p0 = (0, 0, 1)
    p1 = (0, 0.25, 0.75)
    p2 = (0, 0, 0.5)
    p3 = (0, -0.25, 0.75)

def xyz(t0):
    t = t0/T
    x = numpy.interp(t, [0, 0.25, 0.5, 0.75, 1.0], [p0[0], p1[0], p2[0], p3[0], p0[0]])
    y = numpy.interp(t, [0, 0.25, 0.5, 0.75, 1.0], [p0[1], p1[1], p2[1], p3[1], p0[1]])
    z = numpy.interp(t, [0, 0.25, 0.5, 0.75, 1.0], [p0[2], p1[2], p2[2], p3[2], p0[2]])
    return (x, y, z)

# steps/sec
N = 100
dt = 1.0/N
t = 0

print t, xyz(t), psi
for i in xrange(0, T*N):
    if abs(psi[0])>=abs(psi[1]):
        j = 0
    else:
        j = 1
    E = h(xyz(t)).dot(psi)[j]/psi[j]
    k1 = -1j*h(xyz(t)).dot(psi)
    k2 = -1j*h(xyz(t+0.5*dt)).dot(psi+0.5*dt*k1)
    k3 = -1j*h(xyz(t+0.5*dt)).dot(psi+0.5*dt*k2)
    k4 = -1j*h(xyz(t+dt)).dot(psi+dt*k3)
    psi += dt/6*(k1+2*k2+2*k3+k4)
    phase *= numpy.exp(-1j*E*dt)
    t += dt

print t, xyz(t), psi
print abs(psi[0])**2+abs(psi[1])**2
print "phase =", phase
