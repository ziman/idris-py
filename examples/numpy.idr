module Main

import Python
import Python.Lib.Numpy.Dependent

main : PIO ()
main = do
  xs <- array [[1,2,3],[4,5,6]]
  putStrLn "Hello world!"
