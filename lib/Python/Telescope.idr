module Python.Telescope

import public Data.Erased

%hide Language.Reflection.Binder

%default total
%access public

data Binder : Type -> Type where
  ||| Relevant, mandatory, positional argument.
  Pi : (a : Type) -> Binder a

  ||| Erased, mandatory, positional argument.
  Forall : (a : Type) -> Binder (Erased a)

||| Type of sequences where the value of any element may affect
||| the type of the following elements.
|||
||| In other words, a dependent pair generalised to multiple elements.
data Telescope : Type -> Type where

  ||| Empty telescope.
  Return : Type -> Telescope Unit

  ||| Value on which subsequent types may depend.
  Bind :
    (bnd : Binder a)
    -> {b : a -> Type}
    -> (tf : (x : a) -> Telescope (b x))
    -> Telescope (Sigma a b)

term syntax "pi" {x} ":" [t] "." [rhs]
  = Bind (Pi t) (\x : t => rhs);

term syntax "forall" {x} ":" [t] "." [rhs]
  = Bind (Forall t) (\ex : Erased t => let x = unerase ex in rhs);

namespace TupleSugar
  ||| Alternative name for `MkUnit`, useful for the [list, syntax, sugar].
  Nil : Unit
  Nil = ()

  ||| Infix name for `MkSigma`, useful for the [list, syntax, sugar].
  (::) : (x : a) -> (y : b x) -> Sigma a b
  (::) = MkSigma

||| A list for runtime-relevant values, typed by the given telescope.
||| 
||| @ t  The telescope.
||| @ xs The tuple containing *all* telescope-typed elements.
|||
||| Note that the whole tuple `xs` is required for complete type information
||| but the resulting list may skip over some of the elements of `xs`,
||| and even those elements that are not skipped can be replaced by
||| any other value with the same type.
data TList : (t : Telescope a) -> (xs : a) -> Type where

  ||| Return `TList`.
  TNil : TList (Return x) []

  ||| Prepend an element in front of a `TList`, dependent variant.
  TCons :
    .{bnd : Binder a}
    -> .{b : a -> Type}
    -> .{tf : (y : a) -> Telescope (b y)}
    -> (x : a)
    -> (xs : TList (tf x) args)
    -> TList (Bind bnd tf) (x :: args)

  TSkip :
  .{bnd : Binder a}
    -> .{b : a -> Type}
    -> .{tf : (y : a) -> Telescope (b y)}
    -- There is no (x : argTy) stored here, it is skipped.
    -- For the type, we assume that the value is "x" coming from args.
    -> (xs : TList (tf x) args)
    -> TList (Bind bnd tf) (x :: args)

-- These are consumed by the FFI.
%used TCons x
%used TCons xs
%used TSkip xs

||| Strip the given tuple `xs` to the `TList` of runtime-relevant values.
strip : (t : Telescope a) -> (args : a) -> TList t args
strip (Return _) () = TNil
strip (Bind (Pi      _  ) tf) (x ** xs) = TCons x $ strip (tf x) xs
strip (Bind (Forall  _  ) tf) (x ** xs) = TSkip   $ strip (tf x) xs

||| Convert a list of types to the corresponding tuple type.
toTuple : (xs : List Type) -> Type
toTuple [] = Unit
toTuple (x :: xs) = Sigma x (const $ toTuple xs)

||| Convert a list of types to the corresponding simple telescope.
simple : (xs : List Type) -> (ret : Type) -> Telescope (toTuple xs)
simple []        ret = Return ret
simple (a :: as) ret = Bind (Pi a) (\x => simple as ret)

retTy : (t : Telescope a) -> (args : a) -> Type
retTy (Return x) () = x
retTy (Bind bnd tf) (MkSigma x xs) = retTy (tf x) xs
