module Python.Fields

import Python.RTS
import public Python.IO

%default total
%access public

||| A dynamically typed Python reference
Dyn : Type
Dyn = Ptr

data Field : Type where
  Attr : (ty : Type) -> Field
  ParAttr : (ps : Type) -> (tyf : ps -> Type) -> Field
  Call : (as : List Type) -> (ret : Type) -> Field
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
record Function (as : List Type) (ret : Type) where
  constructor MkFunction
  ptr : Dyn

infix 3 ~~>
(~~>) : (args : List Type) -> (ret : Type) -> Type
(~~>) args ret = Function args ret

infix 3 ~>
(~>) : (args : List Type) -> (ret : Type) -> Field
(~>) args ret = Attr $ args ~~> ret

abstract
marshalPIO : PIO a -> [] ~~> a
marshalPIO = MkFunction . ptrFromPIO

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
PyType_sig a "__call__" = Call [Dyn] a
PyType_sig a f = Object_sig f

instance Object (PyType a) (PyType_sig a) where {}

Function_sig : (as : List Type) -> (ret : Type) -> Signature
Function_sig as ret "__call__" = Call as ret
Function_sig as ret f = Object_sig f

instance Object (Function as ret) (Function_sig as ret) where {}

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
(/.) : Object a sig
  => a -> (f : String)
  -> {auto pf : sig f = Attr ty}
  -> ty
(/.) {ty=ty} x f =
  unRaw . unsafePerformIO $
    foreign FFI_Py "getattr" (Dyn -> String -> PIO (Raw ty)) (toDyn x) f

infixl 4 //.
abstract
(//.) : Object a sig
  => a -> (fps : (String, params))
  -> {auto pf : sig (fst fps) = ParAttr params tyf}
  -> tyf (snd fps)
(//.) {params=params} {tyf=tyf} x (f, ps) =
  unRaw . unsafePerformIO $
    foreign FFI_Py "getattr" (Dyn -> String -> PIO (Raw $ tyf ps)) (toDyn x) f

abstract
call : Object a sig
  => a -> (args : HList as)
  -> {auto pf : sig "__call__" = Call as ret}
  -> PIO ret
call {as=as} {ret=ret} f args =
    unRaw <$>
      foreign
        FFI_Py "_idris_call"
        (Dyn -> HList as -> PIO (Raw ret))
        (toDyn f)
        args                                            --  <--- TODO

infixl 4 $.
($.) :
  Function as ret
  -> (args : HList as)
  -> PIO ret
{-
($.) : Object a sig
  => a -> (args : ta)
  -> {auto pf : sig "__call__" = Call {ta=ta} t}
  -> PIO $ retTy t args
-}
($.) f args = call f args

