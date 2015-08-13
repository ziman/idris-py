module Python.Objects

%default total
%access public

infix 4 :::
||| The "has-type" declaration for fields.
record Field where
  constructor (:::)
  name : String
  type : Type

||| Python object signature is a list of its fields, plus mixins.
record Signature where
  constructor MkSignature
  name : String  -- for error reporting
  fields : List Field
  mixins : List Signature

||| Python object, typed by its signature.
-- TODO: make this abstract
record Obj (sig : Signature) where
  constructor MkObj
  pyObj : Ptr

infix 4 ::.
||| Convenience variant of (:::) for object fields.
(::.) : String -> Signature -> Field
(::.) name sig = name ::: Obj sig

infixr 4 <:
||| Add a mixin to the signature.
(<:) : Signature -> Signature -> Signature
(<:) (MkSignature n fs mixins) sig = MkSignature n fs (sig :: mixins)

||| Python type of Python types.
PyType : Signature
PyType = MkSignature "PyType" ["__name__" ::: String] []

||| Object that all Python objects inherit from.
PyObject : Signature
PyObject = MkSignature "PyObject" ["__class__" ::. PyType] []

||| Make a signature without mixins other than Object.
signature : String -> List Field -> Signature
signature n fs = MkSignature n fs [PyObject]

||| Any Python object.
Object : Type
Object = Obj PyObject
