module Python.Prim

import Python

%access public
%default total

Iterator : Type -> Signature
Iterator a = signature "Iterator"
  [ "next" ::: [] ~> a
  ]

Iterable : Type -> Signature
Iterable a = signature "Iterable"
  [ "__iter__" ::: [] ~> Obj (Iterator a)
  ]

PyString : Signature
PyString = signature "PyString"
  [ "join" ::: [Obj $ Iterable String] ~> String
  ]

PyList : Type -> Signature
PyList a = signature "PyList"
  [ -- nothing yet
  ]
  <: Iterable a

data PythonPrim : Type -> Signature -> Type where
  PPString : PythonPrim  String       PyString

obj : (x : a) -> {auto pf : PythonPrim a sig} -> Obj sig
obj x = believe_me x

next : Obj (Iterator a) -> PIO (Maybe a)
next {a = a} it = do
    Right x <- try (it /. "next" $: [])
      | Left e => do
          putStrLn $ show e
          return Nothing
    return $ Just x

partial
iter : Obj (Iterator a) -> (st : b) -> (f : b -> a -> PIO b) -> PIO b
iter it st f = do
  Just x <- next it | Nothing => return st
  st' <- f st x
  iter it st' f

partial
iterate : (iterable : Obj $ Iterable a) -> (st : b) -> (f : b -> a -> PIO b) -> PIO b
iterate iterable st f = do
  iterator <- iterable /. "__iter__" $: []
  iter iterator st f

||| A left-fold over an iterable.
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
    foreign FFI_Py "idris_foreach"
      (Obj (Iterable a) -> Raw b -> Raw (b -> a -> PIO b) -> PIO (Raw b))
      iterable
      (MkRaw st)
      (MkRaw f)

||| Collect all elements of an iterator into a list.
partial
collect : (it : Obj $ Iterable a) -> PIO (List a)
collect it = reverse <$> foreach it List.Nil (\xs, x => return (x :: xs))
