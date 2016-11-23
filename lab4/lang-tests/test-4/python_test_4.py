x = "estatico"

def g():
    print x

def f():
    x = "dinamico"
    g()

f()
