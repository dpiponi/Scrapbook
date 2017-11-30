#import math
#import numpy

@printf "Hello\n"

n = 100

m = 100

function f()
    s = 0
    for t = 1:m
        s += randn() < 0.5
    end
    return s/n
end

l = 100000

# Estimate variance
sz = 0
szz = 0
for t = 1:l
    z = f()
    sz += z
    szz += z*z
end

variance = (szz-sz*sz/l)/(l-1)

@printf "%f %f\n" variance (1/(2*n))
