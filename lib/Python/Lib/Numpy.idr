module Python.Lib.Numpy

import Python
import Python.Prim
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
    PyL : Type -> Type
    PyL a = Obj $ PyList a

    -- workaround for strange elaboration
    rest : (ea : Erased Type) -> Telescope
      (Sigma (PyL $ PyL (unerase ea)) $ \xs =>
         Sigma String $ \dtype => ())
    rest (Erase a) =
      Dep (Pi $ PyL (PyL a)) $ \xs =>
        Dep (Pi String) $ \dtype =>
          Return (Obj NDArray)

import_ : PIO $ Obj Numpy
import_ = importModule "numpy"
