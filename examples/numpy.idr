module Main

import Data.Vect

import Python
import Python.Lib.Numpy.Dependent

main : PIO ()
main = do
    printLn xs
  where
    xs : Array 2 3 NFloat
    xs = array NFloat [[1,2,3],[4,5,6]]
