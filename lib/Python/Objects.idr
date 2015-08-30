module Python.Objects

import Python.Telescope

%default total
%access public

data Field : Type where
  Attr : Type -> Field
  ParAttr : (pt : Type) -> (tf : pt -> Type) -> Field
  Call : (t : Telescope a) -> Field
  NotField : Field

||| Python object signature is a list of its fields, plus mixins.
Signature : Type
Signature = String -> Field

instance Semigroup Signature where
  (<+>) s t =
    \field => case s field of
        NotField => t field
        result   => result

instance Monoid Signature where
  neutral = const NotField

||| Dynamically typed Python reference.
Dyn : Type
Dyn = Ptr

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
