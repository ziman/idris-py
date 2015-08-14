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

NDArrayClass : Signature
NDArrayClass = signature "NDArrayClass"
  [ "reshape" ::. [Obj NDArray, Integer, Integer] ~> Obj NDArray
  , "__add__" ::. [Obj NDArray, Obj NDArray] ~> Obj NDArray
  , "__mul__" ::. [Obj NDArray, Obj NDArray] ~> Obj NDArray
  , "__div__" ::. [Obj NDArray, Obj NDArray] ~> Obj NDArray
  , "__sub__" ::. [Obj NDArray, Obj NDArray] ~> Obj NDArray
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
    , "ndarray" ::. NDArrayClass
    , "abs" ::. [Obj NDArray] ~> Obj NDArray
    , "tile" ::. [Obj NDArray, List Integer] ~> Obj NDArray
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
