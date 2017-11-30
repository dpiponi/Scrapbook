#import math
#import numpy

@printf "Hello\n"

function f(x, y)
    return x+2*y
end

n = 100000

# Estimate variance
sz = 0
szz = 0
for t = 1:n
    x = (randn(), randn())
    z = f(x[1], x[2])
    sz += z
    szz += z*z
end

variance = (szz-sz*sz/n)/(n-1)

n = 10000
m = 10000
s = 0
# Estimate RHS
for t = 1:n
    x = (randn(), randn())
    s1 = 0
    s2 = 0
    for u = 1:m
        e1 = f(x[1], randn())
        s1 += e1
        e2 = f(randn(), x[2])
        s2 += e2
    end
    e1 = e1/m
    e2 = e2/m
    z = f(x[1], x[2])
    s += (z-e1)^2+(z-e2)^2
end

s = s/n

@printf "%f %f\n" variance (0.5*s)
