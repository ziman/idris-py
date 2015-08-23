module Python.Builtins

import Python
import Python.IO
import Python.RTS

%default total
%access abstract

uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

PyNum : Signature -> Signature
PyNum a f = case f of
  "__str__" => [] ~> String
  "__add__" => [Ref a] ~> Ref a
  _ => Object f

PyType : Signature -> Signature
PyType sig "__name__" = Attr String
PyType sig "__call__" = Call [Dyn] (Ref sig)

PyInt : Signature
PyInt = PyNum PyInt

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

PyDict : Type -> Type -> Signature
PyDict k v f = case f of
  "get" => [k] ~> Maybe v
  _ => Object f

PyTuple : Signature
PyTuple = Object

PySet : Type -> Signature
PySet a f = case f of
  _ => Object f

Builtins : Signature
Builtins f = case f of
  "int"   => attr $ PyType PyInt
  "float" => attr $ PyType PyFloat
  "bool"  => attr $ PyType PyBool
  "str"   => attr $ PyType PyStr
  "list"  => ParAttr Type $ PyType . PyList
  "set"   => ParAttr Type $ PyType . PySet
  "dict"  => ParAttr (Type,Type) $ PyType . uncurry PyDict
  "tuple" => attr $ PyType PyTuple
  _ => NotField

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
dict = builtins /. "dict"

set : (a : Type) -> Ref $ PyType (PySet a)
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
toPyList : List a -> Ref PyList
toPyList xs = unsafePerformIO $ list $. [toDyn xs]
