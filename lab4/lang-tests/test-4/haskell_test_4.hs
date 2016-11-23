import System.IO

x = 1

main =
    do
        result <- f 3
        print(result)


g :: Integer -> IO Integer
g z = 
    do
        let result = z + x
        return result

f :: Integer -> IO Integer
f y = 
    do
        let x = y + 1
        g (x * y)
