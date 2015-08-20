module Python.Builtins

import Python
import Python.IO
import Python.RTS

%default total
%access abstract

PyType : Signature -> Signature
PyType sig "__name__" = Attr String
PyType sig "__call__" = Call [Dyn] (Ref sig)

PyInt : Signature
PyInt _ = NotField

PyFloat : Signature
PyFloat _ = NotField

PyBool : Signature
PyBool _ = NotField

PyStr : Signature
PyStr _ = NotField

PyBytes : Signature
PyBytes _ = NotField

PyList : Signature
PyList _ = NotField

PyDict : Signature
PyDict _ = NotField

PyTuple : Signature
PyTuple _ = NotField

PySet : Signature
PySet _ = NotField

Builtins : Signature
Builtins f = case f of
  "int"   => attr $ PyType PyInt
  "float" => attr $ PyType PyFloat
  "bool"  => attr $ PyType PyBool
  "str"   => attr $ PyType PyStr
  "list"  => attr $ PyType PyList
  "dict"  => attr $ PyType PyDict
  "set"   => attr $ PyType PySet
  "tuple" => attr $ PyType PyTuple
  _ => NotField

abstract
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

data Native : Type -> Signature -> Type where
  nInt : Native Int PyInt

abstract
fromNative : a -> {auto pf : Native a sig} -> Ref sig
fromNative x = MkRef $ believe_me x

abstract
toNative : Ref sig -> {auto pf : Native a sig} -> a
toNative (MkRef ptr) = believe_me ptr

abstract
pyList : List a -> Ref PyList
pyList xs = unsafePerformIO $ list $. [toDyn xs]
