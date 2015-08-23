module Python.Builtins

import Python
import Python.IO
import Python.RTS
import Python.Telescope

%default total
%access abstract

Num_sig : Type -> Signature
Num_sig a f = case f of
  "__str__" => [] ~> String
  "__add__" => [a] ~> a
  _ => Object f

Int_sig : Signature
Int_sig = Num_sig Int

{-
PyFloat : Signature
PyFloat = PyNum PyFloat

PyBool : Signature
PyBool = Object

PyStr : Signature
PyStr = Object

PyBytes : Signature
PyBytes = Object

PyList : Type -> Signature
PyList a f = case f of
  "append" => [a] ~> ()
  "cons"   => [a] ~> Ref (PyList a)  -- hypothetical
  _ => Object f

PyDict : Type -> Type -> Signature
PyDict k v f = case f of
  "get" => [k] ~> Maybe v
  _ => Object f

PyTuple : Signature
PyTuple = Object

PySet : Type -> Signature
PySet a f = case f of
  _ => Object f

ptype : Signature -> Field
ptype = Attr . Ref . PyType

%default total
Builtins : Signature
Builtins f = case f of
  "int"   => ptype PyInt
  "float" => ptype PyFloat
  "bool"  => ptype PyBool
  "str"   => ptype PyStr
  "tuple" => ptype PyTuple
  "list"  => ParAttr Type $ Ref . PyType . PyList
  "set"   => ParAttr Type $ Ref . PyType . PySet
  "dict"  => ParAttr (Type,Type) $ Ref . PyType . uncurry PyDict
  _ => Module f

abstract
builtins : Ref Builtins
builtins = MkRef (unsafePerformIO $ getGlobal "__builtins__")

list : (a : Type) -> Ref $ PyType (PyList a)
list a = builtins //. ("list", a)

int : Ref $ PyType PyInt
int = builtins /. "int"

float : Ref $ PyType PyFloat
float = builtins /. "float"

dict : (k, v : Type) -> Ref $ PyType (PyDict k v)
dict k v = builtins //. ("dict", (k, v))

set : (a : Type) -> Ref $ PyType (PySet a)
set a = builtins //. ("set", a)

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
toPyList : List a -> Ref (PyList a)
toPyList xs = unsafePerformIO $ list _ $. [toDyn xs]
-}
