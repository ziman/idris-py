module Python.Telescope

import public Data.Erased

%default total
%access public

%hide Language.Reflection.Binder
%hide Language.Reflection.Erased

||| Information about an argument.
data Binder : Type -> Type where

  ||| Runtime-relevant argument.
  Pi : (a : Type) -> Binder a

  ||| Optional runtime-relevant argument.
  Optional : (a : Type) -> (dflt : a) -> Binder (Maybe a)

  ||| Runtime-irrelevant argument.
  Forall : (a : Type) -> Binder (Erased a)

||| Alternative name for `MkUnit`, useful for the [list, syntax, sugar].
Nil : Unit
Nil = ()

||| Infix name for `Cons`, useful for the [list, syntax, sugar].
(::) : (x : a) -> (y : b x) -> Sigma a b
(::) = MkSigma

||| Sequence where the value of any element may affect
||| the type of the following elements.
data Telescope : Type -> Type where

  ||| Empty telescope.
  Empty :
    Telescope Unit

  ||| A binder in front of a telescope.
  Bind :
    (bnd : Binder a)
    -> {b : a -> Type}
    -> (t : (x : a) -> Telescope (b x))
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

  ||| Empty `TList`.
  TNil : TList Empty []

  -- Note: we could restrict possible binders in both constructors below
  -- (because (ir-)relevance depends on the binder)
  -- but I reckon it's not worth the clutter.

  ||| Prepend an element in front of a `TList`.
  TCons :
    .{bnd : Binder a}
    -> .{b : a -> Type}
    -> .{x : a}
    -> (y : a)  -- note that `y` is a fresh variable => can be anything of the same type
    -> .{t : (x : a) -> Telescope (b x)}
    -> (xs : TList (t x) args)
    -> TList (Bind bnd t) (x :: args)

  ||| Skip an element in the telescope.
  TSkip :
    .{bnd : Binder a}
    -> .{b : a -> Type}
    -- there is no (y : a) here, it is skipped
    -> .{t : (x : a) -> Telescope (b x)}
    -> (xs : TList (t x) args)
    -> TList (Bind bnd t) (x :: args)

-- These are consumed by the FFI.
%used TCons x
%used TCons xs
%used TSkip xs

||| Strip the given tuple `xs` to the `TList` of runtime-relevant values.
strip : (t : Telescope c) -> (xs : c) -> TList t xs
strip Empty () = TNil
strip (Bind (Pi _        ) t) (MkSigma x xs) = TCons x $ strip (t x) xs
strip (Bind (Forall _    ) t) (MkSigma x xs) = TSkip   $ strip (t x) xs
strip (Bind (Optional _ d) t) (MkSigma x xs) with (x)  -- with-block to work around polymorphism-related error messages
  | Just y  = TCons (Just y) $ strip (t $ Just  y) xs
  | Nothing = TCons (Just d) $ strip (t $ Nothing) xs

||| Convert a list of types to the corresponding tuple type.
toTuple : (xs : List Type) -> Type
toTuple [] = Unit
toTuple (x :: xs) = Sigma x (const $ toTuple xs)

||| Convert a list of types to the corresponding simple telescope.
simple : (xs : List Type) -> Telescope (toTuple xs)
simple []        = Empty
simple (a :: as) = Bind (Pi a) (\x => simple as)
