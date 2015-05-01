module Python

%default total
%access public
%language ErrorReflection

--
-- ###  Python objects  ###
--

infix 4 :::
||| The has-type declaration for fields.
record Field : Type where
  (:::) : (n : String) -> (ty : Type) -> Field

||| Python object signature is a list of its fields, plus mixins.
record Signature : Type where
  MkSignature :
    (name : String)  -- for error reporting
    -> (fields : List Field)
    -> (mixins : List Signature)
    -> Signature

infixr 4 <:
||| Add a mixin to the signature.
(<:) : Signature -> Signature -> Signature
(<:) (MkSignature n fs mixins) sig = MkSignature n fs (sig :: mixins)

||| Make a signature without mixins.
signature : String -> List Field -> Signature
signature n fs = MkSignature n fs []

||| Defines how many, what type, etc. arguments a method takes.
data Args : Type where
  Fixed : (as : List Type) -> Args

||| Python object, typed by its signature.
abstract
record Obj : (sig : Signature) -> Type where
  MkObj : (obj : Ptr) -> Obj fs

||| Python method, typed by its input and output.
abstract
record Method : (args : Args) -> (ret : Type) -> Type where
  MkMethod : (meth : Ptr) -> Method args ret

||| Python exception.
abstract
record Exception : Type where
  MkException : (e : Ptr) -> Exception


--
-- ###  Python FFI definition  ###
--

namespace FFI

  unRaw : FFI_C.Raw a -> a
  unRaw (MkRaw x) = x

  ||| Supported Python foreign types.
  data PyTypes : Type -> Type where
    PyStr     : PyTypes String
    PyFloat   : PyTypes Float
    PyInt     : PyTypes Int
    PyInteger : PyTypes Integer
    PyChar    : PyTypes Char
    PyUnit    : PyTypes ()
    PyFun     : PyTypes a -> PyTypes b -> PyTypes (a -> b)

    ||| Python objects, opaque to Idris.
    PyPtr       : PyTypes Ptr
    PyException : PyTypes Exception

    ||| Arbitrary Idris objects, opaque to Python.
    PyAny : PyTypes (FFI_C.Raw a)

    ||| Python objects with a signature known to Idris.
    PyObj : PyTypes (Obj sig)

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

using (n : String)
  ||| Proof that a signature contains a field of the given type.
  ||| (We don't use List.Elem to keep the signature name in the error message.)
  data HasField : Signature -> Field -> Type where
    FieldHere  :
         MkSignature n (f :: fs) ms `HasField` f
    FieldThere : MkSignature n fs ms `HasField` f
      -> MkSignature n (f' :: fs) ms `HasField` f
    InMixinThere : MkSignature n [] ms `HasField` f
      -> MkSignature n [] (sig :: ms) `HasField` f
    InMixinHere : sig `HasField` f
      -> MkSignature n [] (sig :: ms) `HasField` f

  ||| Proof that an object can be cast to the signature of one of its mixins.
  data HasMixin : (sig : Signature) -> (mixin : Signature) -> Type where
    MixinHere : MkSignature n fs (m :: ms) `HasMixin` m
    MixinThere : MkSignature n fs ms `HasMixin` m -> MkSignature n fs (m' :: ms) `HasMixin` m

||| Cast an object to the signature of one of its mixins.
mixout : (mixin : Signature) -> {auto pf : sig `HasMixin` mixin} -> Obj sig -> Obj mixin
mixout mixin (MkObj obj) = MkObj obj

infixl 2 >.
||| Cast an object to the signature of one of its mixins.
(>.) : (obj : Obj sig) -> (mixin : Signature) -> {auto pf : sig `HasMixin` mixin} -> Obj mixin
(>.) obj mixin = mixout mixin obj

infixl 2 >:
||| Cast an object to the signature of one of its mixins, chained version.
(>:) : (obj : PIO (Obj sig)) -> (mixin : Signature) -> {auto pf : sig `HasMixin` mixin} -> PIO (Obj mixin)
(>:) obj mixin {pf = pf} = mixout mixin {pf = pf} <$> obj

infixl 4 /.
||| Attribute accessor.
|||
||| @ obj Obj with the given signature.
||| @ f   Name of the requested field.
abstract
(/.) : (obj : Obj sig) -> (f : String) -> {auto pf : sig `HasField` (f ::: a)} -> PIO a
(/.) {a = a} (MkObj obj) f =
  unRaw <$> foreign FFI_Py "idris_getfield" (Ptr -> String -> PIO (Raw a)) obj f

infixl 4 /:
||| Attribute accessor, useful for chaining.
|||
||| @ obj PIO action returning an object.
||| @ f   Name of the requested field.
(/:) : (obj : PIO (Obj sig)) -> (f : String) -> {auto pf : sig `HasField` (f ::: a)} -> PIO a
(/:) obj f {pf = pf} = obj >>= \o => (/.) o f {pf}

-- Error reflection for better error messages.
abstract
fieldErr : Err -> Maybe (List ErrorReportPart)
fieldErr (CantSolveGoal `(HasField ~sig (~fname ::: ~fty)) ntms)
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

||| Given a list of types, this is the type
||| of tuples of values with these types.
data HList : List Type -> Type where
  Nil : HList []
  (::) : (x : a) -> (xs : HList as) -> HList (a :: as)

-- These fields enter the FFI and may not be recognised as used.
%used Python.(::) x
%used Python.(::) xs

methTy : Args -> Type -> Type
methTy (Fixed as) ret = HList as -> PIO ret

infixl 4 $.
||| Method call.
|||
||| @ meth The method to call.
abstract
($.) : (meth : Method margs ret) -> methTy margs ret
($.) {margs = Fixed as} {ret = ret} (MkMethod meth) =
  \args => unRaw <$>
    foreign FFI_Py "idris_call" (Ptr -> (Raw $ HList as) -> PIO (Raw ret)) meth (MkRaw args)

infixl 4 $:
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
importModule : (modName : String) -> PIO (Obj sig)
importModule {sig = sig} modName =
  foreign FFI_Py "idris_pymodule" (String -> PIO (Obj sig)) modName

infixr 5 ~>
||| Infix alias for methods with fixed arguments.
(~>) : List Type -> Type -> Type
(~>) args ret = Method (Fixed args) ret
