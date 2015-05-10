module Python.Telescope

import public Data.Erased

%default total
%access public

%hide Language.Reflection.Binder
%hide Language.Reflection.Erased

||| Information about an argument.
data Binder : Type -> Type where

  ||| Mandatory, positional, relevant argument.
  Pi : (a : Type) -> Binder a

  ||| Optional argument with a non-`None` default.
  ||| To default to `None`, use `Optional`, which is equivalent to `Pi . Maybe`.
  Default : (a : Type) -> (dflt : a) -> Binder (Maybe a)

  ||| Runtime-irrelevant argument.
  Forall : (a : Type) -> Binder (Erased a)

||| Optional argument; `None` if not given.
Optional : (a : Type) -> Binder (Maybe a)
Optional a = Pi (Maybe a)

namespace TupleSugar
  ||| Alternative name for `MkUnit`, useful for the [list, syntax, sugar].
  Nil : Unit
  Nil = ()

  ||| Infix name for `MkSigma`, useful for the [list, syntax, sugar].
  (::) : (x : a) -> (y : b x) -> Sigma a b
  (::) = MkSigma

||| Type of sequences where the value of any element may affect
||| the type of the following elements.
|||
||| In other words, a dependent pair generalised to multiple elements.
data Telescope : Type -> Type where

  ||| Empty telescope.
  Return : Type -> Telescope Unit

  ||| A binder in front of a telescope.
  Bind :
    (bnd : Binder a)
    -> {b : a -> Type}
    -> (tf : (x : a) -> Telescope (b x))
    -> Telescope (Sigma a b)

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

  -- Note: we could restrict possible binders in both constructors below
  -- (because (ir-)relevance depends on the binder)
  -- but I reckon it's not worth the clutter.

  ||| Prepend an element in front of a `TList`.
  TCons :
    .{bnd : Binder a}
    -> .{b : a -> Type}
    -> .{x : a}
    -> (y : a)  -- note that `y` is a fresh variable => can be anything of the same type
    -> .{tf : (x : a) -> Telescope (b x)}
    -> (xs : TList (tf x) args)
    -> TList (Bind bnd tf) (x :: args)

  ||| Skip an element in the telescope.
  TSkip :
    .{bnd : Binder a}
    -> .{b : a -> Type}
    -- there is no (y : a) here, it is skipped
    -> .{tf : (x : a) -> Telescope (b x)}
    -> (xs : TList (tf x) args)
    -> TList (Bind bnd tf) (x :: args)

-- These are consumed by the FFI.
%used TCons x
%used TCons xs
%used TSkip xs

||| Strip the given tuple `xs` to the `TList` of runtime-relevant values.
strip : (t : Telescope a) -> (args : a) -> TList t args
strip (Return _) () = TNil
strip (Bind (Pi _       ) t) (MkSigma x xs) = TCons x $ strip (t x) xs
strip (Bind (Forall _   ) t) (MkSigma x xs) = TSkip   $ strip (t x) xs
strip (Bind (Default _ d) t) (MkSigma x xs) with (x)  -- with-block to work around polymorphism-related error messages
  | Just y  = TCons (Just y) $ strip (t $ Just  y) xs
  | Nothing = TCons (Just d) $ strip (t $ Nothing) xs

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
