module Python

%default total
%access public
%language ErrorReflection

--
-- ###  Python objects  ###
--

infix 2 :::
||| The has-type declaration for fields.
record Field : Type where
  (:::) : (n : String) -> (ty : Type) -> Field

||| Python object signature is a list of its fields.
record Signature : Type where
  MkSignature :
    (name : String)  -- for error reporting
    -> (fields : List Field)
    -> Signature

||| Defines how many, what type, etc. arguments a method takes.
data Args : Type where
  Fixed : (as : List Type) -> Args

||| Python object, typed by its signature.
abstract
record Object : (sig : Signature) -> Type where
  MkObject : (obj : Ptr) -> Object fs

||| Python method, typed by its input and output.
abstract
record Method : (args : Args) -> (ret : Type) -> Type where
  MkMethod : (meth : Ptr) -> Method args ret

||| Python iterator yielding items of the given type.
abstract
record Iterator : Type -> Type where
  MkIterator : (iter : Ptr) -> Iterator a


--
-- ###  Python FFI definition  ###
--

namespace FFI

  ||| Supported Python foreign types.
  data PyTypes : Type -> Type where
    PyStr     : PyTypes String
    PyFloat   : PyTypes Float
    PyInt     : PyTypes Int
    PyInteger : PyTypes Integer
    PyChar    : PyTypes Char
    PyUnit    : PyTypes ()
    PyFun     : PyTypes a -> PyTypes b -> PyTypes (a -> b)

    ||| Arbitrary Python objects, opaque to Idris.
    PyPtr     : PyTypes Ptr

    ||| Python objects with a signature known to Idris.
    PyObject  : PyTypes (Object sig)

  FFI_Py : FFI
  FFI_Py = MkFFI PyTypes String String

||| Python IO. Read "pie-oh".
PIO : Type -> Type
PIO = IO' FFI_Py


--
-- ###  Field accessors  ###
--

-- This is a very low-level function for hacking around things.
-- Field accessors should perhaps return Maybe?
private
isNone : Ptr -> PIO Int
isNone p = foreign FFI_Py "idris_is_none" (Ptr -> PIO Int) p

||| Proof that a signature contains a field of the given type.
||| (We don't use List.Elem to keep the signature name in the error message.)
data Contains : Field -> Signature -> Type where
  Here  : {n : String} -> Contains f (MkSignature n $ f :: fs)
  There : {n : String} -> Contains f (MkSignature n fs) -> Contains f (MkSignature n $ f' :: fs)

||| Given a list of types, this is the type
||| of tuples of values with these types.
data HList : List Type -> Type where
  Nil : HList []
  (::) : (x : a) -> (xs : HList as) -> HList (a :: as)

-- These fields enter the FFI and may not be recognised as used.
%used Python.(::) x
%used Python.(::) xs

infixl 3 /.
||| Attribute accessor.
|||
||| @ obj Object with the given signature.
||| @ f   Name of the requested field.
abstract
(/.) : (obj : Object sig) -> (f : String) -> {auto pf : Contains (f ::: a) sig} -> PIO a
(/.) {a = a} (MkObject obj) f =
  believe_me <$>
    foreign FFI_Py "idris_getfield" (Ptr -> String -> PIO Ptr) obj f

infixl 3 /:
||| Attribute accessor, useful for chaining.
|||
||| @ obj PIO action returning an object.
||| @ f   Name of the requested field.
(/:) : (obj : PIO (Object sig)) -> (f : String) -> {auto pf : Contains (f ::: a) sig} -> PIO a
(/:) obj f {pf = pf} = obj >>= \o => (/.) o f {pf}

-- Error reflection for better error messages.
private
fieldErr : Err -> Maybe (List ErrorReportPart)
fieldErr (CantSolveGoal `(Contains (~fname ::: ~fty) ~sig) ntms)
    = Just
        [ TextPart "Field"
        , TermPart fname
        , TextPart "does not exist in object signature"
        , TermPart $ simplify sig
        ]
  where
    simplify : TT -> TT
    simplify `(MkSignature ~name ~fields) = name
    simplify sig = sig

fieldErr _ = Nothing

%error_handlers Python.(/.) pf fieldErr
%error_handlers Python.(/:) pf fieldErr

methTy : Args -> Type -> Type
methTy (Fixed as) ret = HList as -> PIO ret

infixl 3 $.
||| Method call.
|||
||| @ meth The method to call.
abstract
($.) : (meth : Method margs ret) -> methTy margs ret
($.) {margs = Fixed as} (MkMethod meth) =
  \args => believe_me <$>
    foreign FFI_Py "idris_call" (Ptr -> Ptr -> PIO Ptr) meth (believe_me args)

infixl 3 $:
||| Method call, useful for chaining.
|||
||| @ meth PIO action returning a method.
($:) : (meth : PIO (Method margs ret)) -> methTy margs ret
($:) {margs = Fixed as} meth = \args => meth >>= \m => m $. args


--
-- ###  Miscellaneous  ###
--

||| Import a Python module. This is a low-level function
||| since the correctness of signatures cannot be checked.
|||
||| Libraries are encouraged to provide an `import_` function in their namespaces.
|||
||| @ sig     Signature of the returned object. Not checked.
||| @ modName Name of the module, as given to `__import__`.
abstract
importModule : (modName : String) -> PIO (Object sig)
importModule modName =
  believe_me <$>
    foreign FFI_Py "idris_pymodule" (String -> PIO Ptr) modName

infix 3 ~>
||| Infix alias for methods with fixed arguments.
(~>) : List Type -> Type -> Type
(~>) args ret = Method (Fixed args) ret

||| A left-fold over an iterator.
|||
||| @ it The iterator.
||| @ st Initial state.
||| @ f  PIO action called for every element, transforms the state.
abstract
foreach : (it : Iterator a) -> (st : b) -> (f : b -> a -> PIO b) -> PIO b
foreach (MkIterator it) st f = do
  believe_me <$>
    foreign FFI_Py "idris_foreach"
      (Ptr -> Ptr -> (Ptr -> Ptr -> Ptr) -> PIO Ptr)
      it
      (believe_me st)
      (believe_me f)

||| Collect all elements of an iterator into a list.
collect : (it : Iterator a) -> PIO (List a)
collect it = reverse <$> foreach it List.Nil (\xs, x => return (x :: xs))
