module Main

import Python
import Python.Lib.Numpy

main : PIO ()
main = do
  numpy <- Numpy.import_
  putStrLn "Hello world!"
