module Python.Prim

import Python

PyString : Signature
PyString = MkSignature "PyString"
  [ "join" ::: [Iterator String] ~> String
  ]

data PythonPrim : Type -> Signature -> Type where
  PPString : PythonPrim String PyString

obj : (x : a) -> {auto pf : PythonPrim a sig} -> Object sig
obj x = believe_me x
