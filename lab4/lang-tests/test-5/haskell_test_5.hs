module Main

where

import Debug.Trace

factorial :: Int -> Int
factorial 1 = traceStack ("fin stack") 1
factorial x = traceStack ("stack") x * (factorial $ x - 1)

main = do
    putStrLn $ "factorial 5: " ++ show (factorial 5)
