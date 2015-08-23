module Python

import Python.IO

%default total
%access public

||| A dynamically typed Python reference
Dyn : Type
Dyn = Ptr

data Field : Type where
  Attr : (ty : Type) -> Field
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

data Proxy : Type -> Type where
  MkProxy : (a : Type) -> Proxy a

class Object a (sig : Signature) | a where
  -- no methods

data Function : (args : List Type) -> (ret : Type) -> Type where {}

infixr 3 ~>
(~>) : (args : List Type) -> (ret : Type) -> Field
(~>) args ret = Attr $ Function args ret

-- the root of the inheritance hierarchy
Object_sig : Signature
Object_sig "__repr__" = [] ~> String
Object_sig _          = NotField

data Module : Type where {}

Module_sig : Signature
Module_sig "__name__" = Attr String
Module_sig f = Object_sig f

instance Object Module Module_sig where {}

data PyType : Type -> Type where {}

PyType_sig : Type -> Signature
PyType_sig a "__name__" = Attr String
PyType_sig a "__call__" = Call [Dyn] a
PyType_sig a f = Object_sig f

instance Object (PyType a) (PyType_sig a) where {}

Function_sig : (args : List Type) -> (ret : Type) -> Signature
Function_sig args ret "__call__" = Call args ret
Function_sig args ret f = Object_sig f

instance Object (Function args ret) (Function_sig args ret) where {}

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
(/.) : Object a sig => (r : a) -> (f : String) -> {auto pf : sig f = Attr ty} -> ty
(/.) {ty=ty} x f =
  unRaw . unsafePerformIO $
    foreign FFI_Py "getattr" (Dyn -> String -> PIO (Raw ty)) (toDyn x) f

infixl 4 $.
abstract
($.) : Object a sig => (f : a) -> {auto pf : sig "__call__" = Call args ret} -> HList args -> PIO ret
($.) {ret=ret} x args =
    unRaw <$>
      foreign FFI_Py "_idris_call" (Dyn -> List Dyn -> PIO (Raw ret)) (toDyn x) (fromHList args)
  where
    fromHList : HList as -> List Dyn
    fromHList [] = []
    fromHList (x :: xs) = toDyn x :: fromHList xs

{-
abstract
(/:) : Object a => (r : PIO (Ref a)) -> (f : String) -> {auto pf : getField r f = Just ty} -> PIO ty
(/:) pio f = map (/. f) pio

infixl 4 $:
abstract
($:) : PIO (Ref $ Function args ret) -> HList args -> PIO ret
($:) pio args = map ($. args) pio
-}
