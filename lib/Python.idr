module Python

import Python.Telescope
import Python.IO

%default total
%access public

||| A dynamically typed Python reference
Dyn : Type
Dyn = Ptr

data Field : Type where
  Attr : (ty : Type) -> Field
  ParAttr : (ps : Type) -> (tyf : ps -> Type) -> Field
  Call : {ta : Type} -> (t : Telescope ta) -> Field
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

data Proxy : Type -> Type where
  MkProxy : (a : Type) -> Proxy a

class Object a (sig : Signature) | a where
  -- no methods

abstract
record Function (t : Telescope a) where
  ptr : Dyn

infixr 3 ~>
(~>) : (args : List Type) -> (ret : Type) -> Field
(~>) args ret = Attr $ Function (simple args ret)

-- the root of the inheritance hierarchy
Object_sig : Signature
Object_sig "__repr__" = [] ~> String
Object_sig _          = NotField

abstract
record Module where
  ptr : Dyn

Module_sig : Signature
Module_sig "__name__" = Attr String
Module_sig f = Object_sig f

instance Object Module Module_sig where {}

abstract
record PyType (a : Type) where
  ptr : Dyn

PyType_sig : Type -> Signature
PyType_sig a "__name__" = Attr String
PyType_sig a "__call__" = Call $ simple [Dyn] a
PyType_sig a f = Object_sig f

instance Object (PyType a) (PyType_sig a) where {}

Function_sig : (t : Telescope a) -> Signature
Function_sig t "__call__" = Call t
Function_sig t f = Object_sig f

instance Object (Function t) (Function_sig t) where {}

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
(/.) : Object a sig => a -> (f : String) -> {auto pf : sig f = Attr ty} -> ty
(/.) {ty=ty} x f =
  unRaw . unsafePerformIO $
    foreign FFI_Py "getattr" (Dyn -> String -> PIO (Raw ty)) (toDyn x) f

infixl 4 //.
abstract
(//.) : Object a sig => a -> (fps : (String, params)) -> {auto pf : sig (fst fps) = ParAttr params tyf} -> tyf (snd fps)
(//.) {params=params} {tyf=tyf} x (f, ps) =
  unRaw . unsafePerformIO $
    foreign FFI_Py "getattr" (Dyn -> String -> PIO (Raw $ tyf ps)) (toDyn x) f

infixl 4 $.
abstract
($.) : Object a sig => a -> (args : ta) -> {auto pf : sig "__call__" = Call {ta=ta} t} -> PIO ret
($.) {t=t} {ta=ta} {ret=ret} f args =
    unRaw <$>
      foreign
        FFI_Py "_idris_call"
        (Dyn -> TList t args -> PIO (Raw ret))
        (toDyn f)
        (strip t args)
