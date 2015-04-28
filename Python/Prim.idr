module Python.Prim

import Python

data PyString : PySig where
  PyString_join : PyString "join" ([Iterator String] ~> String)

data PythonPrim : Type -> PySig -> Type where
  PPString : PythonPrim String PyString

obj : (x : a) -> {auto pf : PythonPrim a sig} -> Object sig
obj x = believe_me x
