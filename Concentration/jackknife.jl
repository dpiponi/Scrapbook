a = rand(10)

# print(a,'\n')

function mean(j, a)
    n = length(a)
    t = 0
    c = 0
    for i = 1:n
        if j != i
            t += a[i]
            c += 1
        end
    end
    return t/c
end

function gmean(j, a)
    n = length(a)
    t = 0
    c = 0
    for i = 1:n
        if j != i
            t += log(a[i])
            c += 1
        end
    end
    return exp(t/c)
end

function sum(j, a)
    n = length(a)
    t = 0
    for i = 1:n
        if j != i
            t += a[i]
        end
    end
    return t
end

function prod(j, a)
    n = length(a)
    t = 1
    for i = 1:n
        if j != i
            t *= a[i]
        end
    end
    return t
end

function sup(j, a)
    n = length(a)
    t = -1e8
    for i = 1:n
        if j != i
            t = max(t, a[i])
        end
    end
    return t
end

print(gmean(0, a), '\n')

n = length(a)
b = zeros(n)
for i in 1:n
    ps = n*gmean(0, a)-(n-1)*gmean(i, a)
#    print(ps, '\n')
    b[i] = ps
end

print(mean(0, b), '\n')
