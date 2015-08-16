module Python

import Python.IO

%default total
%access public

||| A dynamically typed Python reference
Dyn : Type
Dyn = Ptr

||| Type-tagged Python reference
record Ref (a : Type) where
  constructor MkRef
  ptr : Dyn

record Function (args : List Type) (ret : Type) where
  constructor MkFunction
  -- no fields

data HList : List Type -> Type where
  Nil : HList []
  (::) : a -> HList as -> HList (a :: as)

abstract
toDyn : a -> Dyn
toDyn = believe_me

abstract
toString : Dyn -> String
toString x =
  unsafePerformIO $
    foreign FFI_Py "str" (Dyn -> PIO String) x

-- objects

class Object a where
  getField : Ref a -> String -> Maybe Type

infixl 4 /.
abstract
(/.) : Object a => (r : Ref a) -> (f : String) -> {auto pf : getField r f = Just ty} -> ty
(/.) {ty=ty} (MkRef ptr) f =
  unRaw . unsafePerformIO $
    foreign FFI_Py "getattr" (Dyn -> String -> PIO (Raw ty)) ptr f

infixl 4 $.
abstract
($.) : (f : Ref $ Function args ret) -> HList args -> PIO ret
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
