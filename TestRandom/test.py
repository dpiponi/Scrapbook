from interval import interval
from heapq import *

def coercing(f):
    from functools import wraps

    @wraps(f)
    def wrapper(self, other):
        try:
            return f(self, self.cast(other))
        except self.ScalarError:
            return NotImplemented
    return wrapper

@coercing
def le(a, b):
    print "le()"
    if a[-1].sup <= b[0].inf:
        return True
    elif a[0].inf > b[-1].sup:
        return False
    else:
        raise ValueError("Hello!")

@coercing
def ge(a, b):
    print "ge()"
    if b[-1].sup <= a[0].inf:
        return True
    elif b[0].inf > a[-1].sup:
        return False
    else:
        raise ValueError("Hello!")

interval.__le__ = le
interval.__ge__ = ge

a = interval([1,2])
b = interval([2,3])

def integrate(f, tol):
    queue = [(-1.0, interval([0, 1]))]

    total = 0.0
    bound = 1.0
    while bound > tol:
        print "queue =", queue
        print "total =", total
        print "bound =", bound
        (width, x) = heappop(queue)
        width = -width
        try:
            result = f(x)
            print "Accumulating", result, "from", x
            total += result*width
            bound -= width
        except ValueError as e:
            a = x[0].inf
            b = x[0].sup
            heappush(queue, (-0.5*width, interval([a, 0.5*(a+b)])))
            heappush(queue, (-0.5*width, interval([0.5*(a+b), b])))

    return total

def coin(x):
    return x <= 0.33333333

print integrate(coin, 1e-8)
