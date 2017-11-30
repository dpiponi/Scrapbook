#import math
#import numpy

# Counterexample to p.9

@printf "Hello\n"

n = 10000000

s1 = 0.0
s2 = 0.0
s3 = 0.0
for t = 1:n
    x1 = 0.4*rand()-1
    x2 = 0.4*rand()-1

    if x1+x2 > 0
        a = 1
    else
        a = -1
    end

    if x2 > 0
        a1 = 1
    else
        a1 = -1
    end

    if x1 > 0
        a2 = 1
    else
        a2 = -1
    end

    z = a*(x1+x2)
    z1 = a1*x1
    z2 = a2*x2

    s1 += (z-z1)^2-(a1*z1)^2
    s2 += 2*(z-z1+a1*z1)^2
    s3 += z-z1

    if z1-z < 0
        @printf "%f\n" z-z1
    end
#    lhs = (z-z1)^2-(a1*z1)^2
#    rhs = 2*(z-z1+a1*z1)
#    if lhs > rhs
#        @printf "%f %f: %f %f\n" x1 x2 lhs rhs
#    end
end

@printf "%f %f %f\n" (s1/n) (s2/n) (s3/n)
