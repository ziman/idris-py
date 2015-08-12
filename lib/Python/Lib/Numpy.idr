module Python.Lib.Numpy

import Python

%default total
%access public

Numpy : Signature
Numpy = signature "numpy"
  [
  ]

import_ : PIO $ Obj Numpy
import_ = importModule "numpy"
