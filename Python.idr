module Python

%default total

data PyTypes : Type -> Type where
  PyStr     : PyTypes String
  PyFloat   : PyTypes Float
  PyInt     : PyTypes Int
  PyInteger : PyTypes Integer
  PyChar    : PyTypes Char
  PyPtr     : PyTypes Ptr
  PyUnit    : PyTypes ()
  PyFun     : PyTypes a -> PyTypes b -> PyTypes (a -> b)

FFI_Py : FFI
FFI_Py = MkFFI PyTypes String String

-- read "pie-oh"
PIO : Type -> Type
PIO = IO' FFI_Py

data Args : Type where
  Fixed : (as : List Type) -> Args

PySig : Type
PySig = String -> Type -> Type

record Object : (sig : PySig) -> Type where
  MkObject : (obj : Ptr) -> Object fs

record Method : (args : Args) -> (ret : Type) -> Type where
  MkMethod : (meth : Ptr) -> Method args ret

record Iterator : Type -> Type where
  MkIterator : (iter : Ptr) -> Iterator a

record Exception : Type where
  MkException : (ex : Ptr) -> Exception

data HList : List Type -> Type where
  Nil : HList []
  (::) : (x : a) -> (xs : HList as) -> HList (a :: as)

%used Python.(::) x
%used Python.(::) xs

isNone : Ptr -> PIO Int
isNone p = foreign FFI_Py "idris_is_none" (Ptr -> PIO Int) p

infixl 3 /.
(/.) : Object sig -> (f : String) -> {auto pf : sig f a} -> PIO a
(/.) {a = a} (MkObject obj) f =
  believe_me <$>
    foreign FFI_Py "idris_getfield" (Ptr -> String -> PIO Ptr) obj f

infixl 3 /:
(/:) : PIO (Object sig) -> (f : String) -> {auto pf : sig f a} -> PIO a
(/:) obj f {pf = pf} = obj >>= \o => (/.) o f {pf}

methTy : Args -> Type -> Type
methTy (Fixed as) ret = HList as -> PIO ret

infixl 3 $.
($.) : Method margs ret -> methTy margs ret
($.) {margs = Fixed as} (MkMethod meth) =
  \args => believe_me <$>
    foreign FFI_Py "idris_call" (Ptr -> Ptr -> PIO Ptr) meth (believe_me args)

infixl 3 $:
($:) : PIO (Method margs ret) -> methTy margs ret
($:) {margs = Fixed as} meth = \args => meth >>= \m => m $. args

class Importable (sig : PySig) where
  moduleName : PySig -> String

import_ : (sig : PySig) -> {default %instance imp : Importable sig} -> PIO (Object sig)
import_ sig {imp = imp} =
  believe_me <$>
    foreign FFI_Py "idris_pymodule" (String -> PIO Ptr) (moduleName @{imp} sig)

FMethod : List Type -> Type -> Type
FMethod args ret = Method (Fixed args) ret

Constructor : Type -> Type
Constructor ret = FMethod [] ret

foreach : (it : Iterator a) -> (st : b) -> (b -> a -> PIO b) -> PIO b
foreach (MkIterator it) st f = do
  believe_me <$>
    foreign FFI_Py "idris_foreach"
      (Ptr -> Ptr -> (Ptr -> Ptr -> Ptr) -> PIO Ptr)
      it
      (believe_me st)
      (believe_me f)

collect : (it : Iterator a) -> PIO (List a)
collect it = reverse <$> foreach it List.Nil (\xs, x => return (x :: xs))

class PythonPrim (a : Type) (sig : PySig) where

obj : PythonPrim a sig => (x : a) -> Object sig
obj x = believe_me x
