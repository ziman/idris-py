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

||| Type-tagged Python reference
record Ref (sig : Signature) where
  constructor MkRef
  ptr : Dyn

attr : Signature -> Field
attr = Attr . Ref

Function : (args : List Type) -> (ret : Type) -> Signature
Function args ret "__call__" = Call args ret
Function args ret _ = NotField

infixr 3 ~>
(~>) : (args : List Type) -> (ret : Type) -> Field
(~>) args ret = attr $ Function args ret

data HList : List Type -> Type where
  Nil : HList []
  (::) : a -> HList as -> HList (a :: as)

abstract
toDyn : a -> Dyn
toDyn = believe_me

abstract
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

{-
abstract
(/:) : Object a => (r : PIO (Ref a)) -> (f : String) -> {auto pf : getField r f = Just ty} -> PIO ty
(/:) pio f = map (/. f) pio

infixl 4 $:
abstract
($:) : PIO (Ref $ Function args ret) -> HList args -> PIO ret
($:) pio args = map ($. args) pio
-}
