import Data.Vect
import Python
import Python.Lib.Numpy.Matrix

%default total

f : Nat -> Maybe (List Bool)
f Z = Just [True, False]
f (S n) = f n

xs : Matrix 3 4 DDouble
xs = array _
  [[1.0,-2.1, 3.3, -0.1]
  ,[3.5, 7.0, 0.0, -5.2]
  ,[0.5, 7.2,-1.1,  0.0]
  ]

ys : Matrix 6 2 DDouble
ys = reshape xs

zs : Matrix 4 3 DDouble
zs = reshape ys

main : PIO ()
main = do
  printLn' $ f 4
  printLn' xs
  printLn' ys
  printLn' zs
  -- printLn (ys `dot` zs)  -- won't work!
  printLn' (xs `dot` zs)
  printLn' (xs `dot` transpose xs)
  printLn' (transpose xs `dot` xs)
  printLn' $ 2 * transpose (zs `dot` xs) + fromDouble 0.2
