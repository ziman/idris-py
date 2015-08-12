module Python.Lib.Numpy

import Python
import Data.Erased

%default total
%access public

NDArray : Signature
NDArray = signature "NDArray"
  [ "__str__" ::. [] ~> String
  ]

Numpy : Signature
Numpy = signature "numpy"
  [ "array" ::.
      Function (
        Dep (Forall Type) $ \a =>
           Dep (Pi $ List (List $ unerase a)) $ \xs =>
              Dep (Pi $ String) $ \dtype =>
                 Return (Obj NDArray)
      )
  ]

import_ : PIO $ Obj Numpy
import_ = importModule "numpy"
