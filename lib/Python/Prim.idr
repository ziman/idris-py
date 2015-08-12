module Python.Prim

import Python
import Python.Telescope
import Python.Fields
import Python.Functions

%access public
%default total

||| The actual state of iteration.
Iterator : Type -> Signature
Iterator a = signature "Iterator"
  [ "next" ::. [] ~> a
  ]

||| Something that can produce an iterator.
Iterable : Type -> Signature
Iterable a = signature "Iterable"
  [ "__iter__" ::. [] ~> Obj (Iterator a)
  ]

||| Python string as object.
PyString : Signature
PyString = signature "PyString"
  [ "join" ::. [Obj $ Iterable String] ~> String
  ]
  <: Iterable Char

||| Python list as object.
PyList : Type -> Signature
PyList a = signature "PyList"
  [ -- nothing yet
  ]
  <: Iterable a

||| Primitives promotable to objects.
data PythonPrim : Type -> Signature -> Type where
  PPString : PythonPrim String PyString

||| Promote a primitive to an object. Note that this is a no-oop,
||| all primitives already are objects in Python.
obj : (x : a) -> {auto pf : PythonPrim a sig} -> Obj sig
obj x = believe_me x

||| Get the next value from an iterator.
next : Obj (Iterator a) -> PIO (Maybe a)
next {a = a} it = do
  OK x <- try (it /. "next" $: [])
    | Except StopIteration e => return Nothing
    | Except _ e => raise e
  return $ Just x

||| A left-fold over an iterable, implemented in Idris.
||| This is not very efficient (TCO does not kick in here)
||| but it's a demonstration we can do it easily.
|||
||| See also `foreach`, which is a native Python for-loop.
partial
iterate : (iterable : Obj $ Iterable a) -> (st : b) -> (f : b -> a -> PIO b) -> PIO b
iterate iterable st f = do
    iterator <- iterable /. "__iter__" $: []
    iter iterator st f
  where
    partial
    iter : Obj (Iterator a) -> (st : b) -> (f : b -> a -> PIO b) -> PIO b
    iter it st f = do
      Just x <- next it | Nothing => return st
      st' <- f st x
      iter it st' f

||| A left-fold over an iterable, implemented as a for-loop in Python.
|||
||| @ iterable The iterable.
||| @ st Initial state.
||| @ f  PIO action called for every element, transforms the state.
partial abstract
foreach :
  (iterable : Obj $ Iterable a)
  -> (st : b)
  -> (f : b -> a -> PIO b)
  -> PIO b
foreach {a = a} {b = b} iterable st f = do
  iterator <- iterable /. "__iter__" $: []
  unRaw <$>
    foreign FFI_Py "_idris_foreach"
      (Obj (Iterable a) -> Raw b -> Raw (b -> a -> PIO b) -> PIO (Raw b))
      iterable
      (MkRaw st)
      (MkRaw f)

||| Collect all elements of an iterator into a list.
partial
collect : (it : Obj $ Iterable a) -> PIO (List a)
collect it = reverse <$> foreach it List.Nil (\xs, x => return (x :: xs))
