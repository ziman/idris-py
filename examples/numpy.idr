module Main

import Data.Vect

import Python
import Python.Lib.Numpy

%default total

f : Nat -> Maybe (List Bool)
f Z = Just [True, False]
f (S n) = f n

xs : Array 3 4 NFloat
xs = array NFloat
  [[1.0,-2.1, 3.3, -0.1]
  ,[3.5, 7.0, 0.0, -5.2]
  ,[0.5, 7.2,-1.1,  0.0]
  ]

ys : Array 6 2 NFloat
ys = reshape xs

main : PIO ()
main = do
    printLn $ f 4
    printLn xs
    printLn ys
