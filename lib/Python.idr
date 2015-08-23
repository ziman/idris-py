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
  ParAttr : (params : Type) -> (tyf : params -> Type) -> Field
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

data Function : (t : Telescope a) -> Type where {}

infixr 3 ~>
(~>) : (args : List Type) -> (ret : Type) -> Field
(~>) args ret = Attr $ Function (simple args ret)

-- the root of the inheritance hierarchy
Object_sig : Signature
Object_sig "__repr__" = [] ~> String
Object_sig _          = NotField

data Module : Type where {}

Module_sig : Signature
Module_sig "__name__" = Attr String
Module_sig f = Object_sig f

data PyType : Type -> Type where {}

PyType_sig : Type -> Signature
PyType_sig a "__name__" = Attr String
PyType_sig a "__call__" = Call $ simple [Dyn] a
PyType_sig a f = Object_sig f

Function_sig : (t : Telescope a) -> Signature
Function_sig t "__call__" = Call t
Function_sig t f = Object_sig f

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

infix 4 .:
record FieldSpec where
  constructor (.:)
  fieldSig : Signature
  fieldName : String


infixl 4 /.
abstract
(/.) : (x : a) -> (fs : FieldSpec) -> {auto pf : fieldSig fs (fieldName fs) = Attr ty} -> ty
(/.) {ty=ty} x (sig .: f) =
  unRaw . unsafePerformIO $
    foreign FFI_Py "getattr" (Dyn -> String -> PIO (Raw ty)) (toDyn x) f

infixl 4 //.
abstract
(//.) : (x : a) -> (fsps : (FieldSpec, params))
  -> {auto pf : fieldSig (fst fsps) (fieldName $ fst fsps) = ParAttr params tyf}
  -> tyf (snd fsps)
(//.) {params=params} {tyf=tyf} x (sig .: f, ps) =
  unRaw . unsafePerformIO $
    foreign FFI_Py "getattr" (Dyn -> String -> PIO (Raw $ tyf ps)) (toDyn x) f

infixl 4 $.
abstract
($.) : (x : a) -> (sigas : (Signature, args)) -> {auto pf : fst sigas "__call__" = Call t} -> PIO $ retTy t (snd sigas)
($.) {t=t} f (sig, args) =
    unRaw <$>
      foreign FFI_Py "_idris_call"
        (Dyn -> Raw (TList t args) -> PIO (Raw $ retTy t args))
        (toDyn f)
        (MkRaw $ strip t args)

{-
abstract
(/:) : Object a => (r : PIO (Ref a)) -> (f : String) -> {auto pf : getField r f = Just ty} -> PIO ty
(/:) pio f = map (/. f) pio

infixl 4 $:
abstract
($:) : PIO (Ref $ Function args ret) -> HList args -> PIO ret
($:) pio args = map ($. args) pio
-}
