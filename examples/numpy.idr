module Main

import Data.Vect

import Python
import Python.IO
import Python.Lib.Numpy

%default total

f : Nat -> Maybe (List Bool)
f Z = Just [True, False]
f (S n) = f n

xs : Array 3 4 DFloat
xs = array DFloat
  [[1.0,-2.1, 3.3, -0.1]
  ,[3.5, 7.0, 0.0, -5.2]
  ,[0.5, 7.2,-1.1,  0.0]
  ]

ys : Array 6 2 DFloat
ys = reshape xs

main : PIO ()
main = do
  printLn $ f 4
  printLn xs
  printLn ys
