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
          Dep (Forall Type) rest {- \a =>
             Dep (Pi $ List (List $ unerase a)) $ \xs =>
                Dep (Pi $ String) $ \dtype =>
                   Return (Obj NDArray)
          -}
        )
    ]
  where
    -- workaround for strange elaboration
    rest : (ea : Erased Type) -> Telescope
      (Sigma (List $ List (unerase ea)) $ \xs =>
         Sigma String $ \dtype => ())
    rest (Erase a) =
      Dep (Pi $ List (List a)) $ \xs =>
        Dep (Pi String) $ \dtype =>
          Return (Obj NDArray)

import_ : PIO $ Obj Numpy
import_ = importModule "numpy"
