module Python

import Python.IO
import Python.Telescope

%default total
%access public

||| A dynamically typed Python reference
Dyn : Type
Dyn = Ptr

data Field : Type where
  Attr : (ty : Type) -> Field
  ParAttr : (p : Type) -> (tf : p -> Type) -> Field
  Call : (t : Telescope a) -> Field
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
  (~>) args ret = attr . Function $ simple args ret

  -- the root of the inheritance hierarchy
  Object : Signature
  Object "__repr__" = [] ~> String
  Object _          = NotField

  Function : (t : Telescope a) -> Signature
  Function t "__call__" = Call t
  Function t f = Object f

Module : Signature
Module "__name__" = Attr String
Module f = Object f

PyType : Signature -> Signature
PyType sig "__name__" = Attr String
PyType sig "__call__" = Call (Dep (Pi Dyn) $ \_=> Return (Ref sig))
PyType sig f = Object f

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
($.) : (f : Ref sig) -> {auto pf : sig "__call__" = Call {a=a} t} -> (args : a) -> PIO $ retTy t args
($.) {t=t} (MkRef ptr) args =
    unRaw <$>
      foreign FFI_Py "_idris_call"
        (Dyn -> Raw (TList t args) -> PIO (Raw $ retTy t args))
        ptr
        (MkRaw $ strip t args)
