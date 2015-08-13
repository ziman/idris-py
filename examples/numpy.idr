module Main

import Data.Vect

import Python
import Python.Lib.Numpy.Dependent

f : Nat -> Maybe (List Bool)
f Z = Just [True, False]
f (S n) = f n

main : PIO ()
main = do
    printLn $ f 4
    printLn xs
  where
    xs : Array 2 3 NFloat
    xs = array NFloat [[1,2,3],[4,5,6]]
