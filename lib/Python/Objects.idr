module Python.Objects

import Python.Telescope

%default total
%access public export

data Field : Type where
  PAttr : (pt : Type) -> (tf : pt -> Type) -> Field
  Call : (t : Telescope a) -> Field
  NotField : Field

Attr : Type -> Field
Attr t = PAttr Unit $ const t

||| Python object signature is a list of its fields, plus mixins.
Signature : Type
Signature = String -> Field

implementation Semigroup Signature where
  (<+>) s t =
    \field => case s field of
        NotField => t field
        result   => result

implementation Monoid Signature where
  neutral = const NotField

||| Dynamically typed Python reference.
Dyn : Type
Dyn = Ptr

export
toDyn : a -> Dyn
toDyn = believe_me

||| Python object, typed by its signature.
-- TODO: make this abstract
record Obj (sig : Signature) where
  constructor MkObj
  ptr : Ptr

||| Python type of Python types.
PyType : Signature
PyType f = case f of
  "__name__" => Attr String
  _ => NotField

||| Object that all Python objects inherit from.
Object : Signature
Object f = case f of
  "__class__" => Attr $ Obj PyType
  _ => NotField

||| Python modules.
Module : Signature
Module f = case f of
  "__name__" => Attr String
  _ => Object f
