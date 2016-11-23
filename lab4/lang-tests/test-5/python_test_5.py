def recsum(x):
    if x == 1:
        return x
    else:
        print "print stack"
        return x + recsum(x - 1)

print recsum(5)
