module Python

import Python.IO

%default total
%access public

||| A dynamically typed Python reference
Dyn : Type
Dyn = Ptr

data Field : Type where
  Attr : (ty : Type) -> Field
  ParAttr : (p : Type) -> (tf : p -> Type) -> Field
  Call : (args : List Type) -> (ret : Type) -> Field
  NotField : Field

Signature : Type
Signature = (f : String) -> Field

instance Semigroup Signature where
  (<+>) s t =
    \field => case s field of
        NotField => t field
        result   => result

instance Monoid Signature where
  neutral = const NotField

||| Type-tagged Python reference
record Ref (sig : Signature) where
  constructor MkRef
  ptr : Dyn

attr : Signature -> Field
attr = Attr . Ref

mutual
  infixr 3 ~>
  (~>) : (args : List Type) -> (ret : Type) -> Field
  (~>) args ret = attr $ Function args ret

  -- the root of the inheritance hierarchy
  Object : Signature
  Object "__repr__" = [] ~> String
  Object _          = NotField

  Function : (args : List Type) -> (ret : Type) -> Signature
  Function args ret "__call__" = Call args ret
  Function args ret f = Object f

Module : Signature
Module "__name__" = Attr String
Module f = Object f

PyType : Type -> Signature
PyType a "__name__" = Attr String
PyType a "__call__" = Call [Dyn] a
PyType a f = Object f

data HList : List Type -> Type where
  Nil : HList []
  (::) : a -> HList as -> HList (a :: as)

abstract
toDyn : a -> Dyn
toDyn = believe_me

abstract partial
unsafeFromDyn : Dyn -> a
unsafeFromDyn = believe_me

abstract
toString : Dyn -> String
toString x =
  unsafePerformIO $
    foreign FFI_Py "str" (Dyn -> PIO String) x

infixl 4 /.
abstract
(/.) : (r : Ref sig) -> (f : String) -> {auto pf : sig f = Attr ty} -> ty
(/.) {ty=ty} (MkRef ptr) f =
  unRaw . unsafePerformIO $
    foreign FFI_Py "getattr" (Dyn -> String -> PIO (Raw ty)) ptr f

infixl 4 //.
abstract
(//.) : (r : Ref sig) -> (fps : (String, ps)) -> {auto pf : sig (fst fps) = ParAttr ps tf} -> tf (snd fps)
(//.) {ps=ps} {tf=tf} x (f, params) =
  unRaw . unsafePerformIO $
    foreign FFI_Py "getattr" (Dyn -> String -> PIO (Raw $ tf params)) (toDyn x) f

infixl 4 $.
abstract
($.) : (f : Ref sig) -> {auto pf : sig "__call__" = Call args ret} -> HList args -> PIO ret
($.) {ret=ret} (MkRef ptr) args =
    unRaw <$>
      foreign FFI_Py "_idris_call" (Dyn -> List Dyn -> PIO (Raw ret)) ptr (fromHList args)
  where
    fromHList : HList as -> List Dyn
    fromHList [] = []
    fromHList (x :: xs) = toDyn x :: fromHList xs
