def tailrecsum(x, running_total=0):
    if x == 0:
        return running_total
    else:
        print "print stack"        
        return tailrecsum(x - 1, running_total + x)

print tailrecsum (5, 0)
