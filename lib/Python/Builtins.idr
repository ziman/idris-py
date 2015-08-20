module Python.Builtins

import Python
import Python.IO
import Python.RTS

%default total
%access abstract

data PyType : Signature -> Signature where
  fName : PyType sig "__name__" String
  fCall : PyType sig "__call__" $ CallField [Dyn] (Ref sig)

data PyInt : Signature where
  piClass : PyInt "__class__" $ Ref (PyType PyInt)

data PyFloat : Signature where
  pfClass : PyFloat "__class__" $ Ref (PyType PyFloat)

data PyBool : Signature where
  pbClass : PyBool "__class__" $ Ref (PyType PyBool)

data PyStr : Signature where
  psClass : PyStr "__class__" $ Ref (PyType PyStr)

data PyBytes : Signature where
  pbyClass : PyBytes "__class__" $ Ref (PyType PyBytes)

data PyList : Signature where
  plClass : PyList "__class__" $ Ref (PyType PyList)

data PyDict : Signature where
  pdClass : PyDict "__class__" $ Ref (PyType PyDict)

data PyTuple : Signature where
  ptClass : PyTuple "__class__" $ Ref (PyType PyTuple)

data PySet : Signature where
  psetClass : PySet "__class__" $ Ref (PyType PySet)

data Builtins : Signature where
  bInt   : Builtins "int"   $ Ref (PyType PyInt)
  bFloat : Builtins "float" $ Ref (PyType PyFloat)
  bBool  : Builtins "bool"  $ Ref (PyType PyBool)
  bStr   : Builtins "str"   $ Ref (PyType PyStr)
  bList  : Builtins "list"  $ Ref (PyType PyList)
  bDict  : Builtins "dict"  $ Ref (PyType PyDict)
  bSet   : Builtins "set"   $ Ref (PyType PySet)
  bTuple : Builtins "tuple" $ Ref (PyType PyTuple)

builtins : Ref Builtins
builtins = MkRef (unsafePerformIO $ getGlobal "__builtins__")

list : Ref $ PyType PyList
list = builtins /. "list"

int : Ref $ PyType PyInt
int = builtins /. "int"

float : Ref $ PyType PyFloat
float = builtins /. "float"

dict : Ref $ PyType PyDict
dict = builtins /. "dict"

set : Ref $ PyType PySet
set = builtins /. "set"

tuple : Ref $ PyType PyTuple
tuple = builtins /. "tuple"

instance Cast (Ref PyInt) Int where
  cast = unsafeFromDyn . ptr

instance Cast Int (Ref PyInt) where
  cast = MkRef . toDyn

instance Cast (Ref PyFloat) Float where
  cast = unsafeFromDyn . ptr

instance Cast Float (Ref PyFloat) where
  cast = MkRef . toDyn

instance Cast (Ref PyBool) Bool where
  cast = unsafeFromDyn . ptr

instance Cast Bool (Ref PyBool) where
  cast = MkRef . toDyn

instance Cast (Ref PyStr) String where
  cast = unsafeFromDyn . ptr

instance Cast String (Ref PyStr) where
  cast = MkRef . toDyn

instance Cast (List a) (Ref PyList) where
  -- runtime representation is iterable
  -- just call the list constructor
  cast xs = unsafePerformIO (list $. [toDyn xs])
