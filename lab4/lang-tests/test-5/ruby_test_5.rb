def fact(n)
    return 1 if n <= 1
    puts "print stack"
    n * fact(n-1)
end

puts fact(5)
