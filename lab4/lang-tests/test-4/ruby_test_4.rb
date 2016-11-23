$x = "estatico"
def g()
    return print $x , "\n"
end
def f()
    $x = "dinamico"
    return g()
end
f()
