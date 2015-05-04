module Python

import public Python.Telescope

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

||| Defines how many, what type, etc. arguments a function takes.
data Args : Type where
  Fixed : (as : List Type) -> Args

||| Python object, typed by its signature.
abstract
record Obj : (sig : Signature) -> Type where
  MkObj : (obj : Ptr) -> Obj fs

||| Python function, typed by its input and output.
abstract
-- This is "data" because a record triggered a bug in auto projections.
data Function : (t : Telescope a) -> Type where
  MkFunction : (meth : Ptr) -> Function t

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
    PyPair    : PyTypes a -> PyTypes b -> PyTypes (a, b)
    PyList    : PyTypes a -> PyTypes (List a)
    PyFun     : PyTypes a -> PyTypes b -> PyTypes (a -> b)
    PyTList   : PyTypes (TList t args)
    PyMaybe   : PyTypes a -> PyTypes (Maybe a)

    ||| Python objects, opaque to Idris.
    PyPtr       : PyTypes Ptr
    PyException : PyTypes Exception
    PyFunction  : PyTypes (Function t)

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

-- This is a separate function because signatures don't always
-- come in this form. Therefore we need to check.
private
simplifySig : TT -> TT
simplifySig `(MkSignature ~name ~fields ~mixins) = name
simplifySig sig = sig

-- Error reflection for better error messages.
mixinErr : Err -> Maybe (List ErrorReportPart)
mixinErr (CantSolveGoal `(HasMixin ~sig ~mixin) ntms)
  = Just
      [ TermPart mixin
      , TextPart "is not mixed into signature"
      , TermPart $ simplifySig sig
      ]
mixinErr _ = Nothing

%error_handlers Python.(>.) pf mixinErr
%error_handlers Python.(>:) pf mixinErr

infixl 4 /.
||| Attribute accessor.
|||
||| @ obj Obj with the given signature.
||| @ f   Name of the requested field.
abstract
(/.) : (obj : Obj sig) -> (f : String) -> {auto pf : sig `HasField` (f ::: a)} -> PIO a
(/.) {a = a} (MkObj obj) f =
  unRaw <$> foreign FFI_Py "getattr" (Ptr -> String -> PIO (Raw a)) obj f

infixl 4 /:
||| Attribute accessor, useful for chaining.
|||
||| @ obj PIO action returning an object.
||| @ f   Name of the requested field.
(/:) : (obj : PIO (Obj sig)) -> (f : String) -> {auto pf : sig `HasField` (f ::: a)} -> PIO a
(/:) obj f {pf = pf} = obj >>= \o => (/.) o f {pf}

-- Error reflection for better error messages.
fieldErr : Err -> Maybe (List ErrorReportPart)
fieldErr (CantSolveGoal `(HasField ~sig (~fname ::: ~fty)) ntms)
  = Just
      [ TextPart "Field", TermPart fname
      , TextPart "does not exist in object signature"
      , TermPart $ simplifySig sig
      ]
fieldErr _ = Nothing

%error_handlers Python.(/.) pf fieldErr
%error_handlers Python.(/:) pf fieldErr

infixl 4 $.
||| Function call.
|||
||| @ meth The function to call.
abstract
($.) : {t : Telescope a} -> (meth : Function t) -> (args : a) -> PIO (teleReturn t args)
($.) {t = t} (MkFunction meth) args =
  unRaw <$>
    foreign FFI_Py "_idris_call"
      (Ptr -> (TList t args) -> PIO (Raw $ teleReturn t args))
      meth
      (strip t args)

infixl 4 $:
||| Function call, useful for chaining.
|||
||| @ meth PIO action returning a function, such as field lookup.
($:) : {t : Telescope a} -> (meth : PIO (Function t)) -> (args : a) -> PIO (teleReturn t args)
($:) meth args = meth >>= \m => m $. args


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
  foreign FFI_Py "_idris_pymodule" (String -> PIO (Obj sig)) modName

infixr 5 ~>
||| Infix alias for functions with fixed arguments.
(~>) : List Type -> Type -> Type
(~>) args ret = Function $ simple args ret

||| Turn a PIO action into a Python function.
||| The function can then be used as a target for threading.Thread etc.
marshalPIO : PIO a -> ([] ~> a)
marshalPIO {a = a} action =
  unsafePerformIO $
    foreign FFI_Py "_idris_marshal_PIO" (Raw (PIO a) -> PIO ([] ~> a)) (MkRaw action)

||| Defines which functions are exportable.
data Exportable : Type -> Type where
  ERet : PyTypes a -> Exportable (PIO a)
  EPi  : PyTypes a -> Exportable b -> Exportable (a -> b)

private
esize : Exportable a -> Int
esize (ERet _)  = 0
esize (EPi _ e) = 1 + esize e

||| Export the given function so that it's visible (and callable)
||| when the generated module is imported from Python.
abstract
export : {auto pf : Exportable a} -> (name : String) -> (f : a) -> PIO ()
export {a = a} {pf = pf} name f =
    foreign FFI_Py "_idris_export"
      (Int -> String -> Raw a -> PIO ())
      (esize pf)
      name
      (MkRaw f)

||| Execute the given action iff the Python module `__name__` is `__main__`.
|||
||| This is necessary because the Idris function `Main.main` is executed
||| whenever the generated Python module is *loaded*.
abstract
ifMain : PIO () -> PIO ()
ifMain m = foreign FFI_Py "_idris_if_main" (Raw (PIO ()) -> PIO ()) (MkRaw m)
