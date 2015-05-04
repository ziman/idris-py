module Python.Telescope

import public Data.Erased

%default total
%access public

%hide Language.Reflection.Binder
%hide Language.Reflection.Erased

data Binder : Type -> Type where
  ||| Runtime-relevant argument.
  Pi     : (a : Type) -> Binder a

  ||| Optional runtime-relevant argument.
  Default : (dflt : a) -> Binder (Maybe a)

  ||| Runtime-irrelevant argument.
  Forall : (a : Type) -> Binder (Erased a)

record NilUnit : Type where
  Nil : NilUnit

data ConsSigma : (a : Type) -> (b : a -> Type) -> Type where
  Cons : (x : a) -> (y : b x) -> ConsSigma a b

(::) : (x : a) -> (y : b x) -> ConsSigma a b
(::) = Cons

data Telescope : Type -> Type where
  Empty :
    Telescope NilUnit

  Bind :
    (bnd : Binder a)
    -> {b : a -> Type}
    -> (t : (x : a) -> Telescope (b x))
    -> Telescope (ConsSigma a b)

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
  TNil : TList Empty []

  TCons :
    {bnd : Binder a}
    -> {b : a -> Type}
    -> {x : a}  -- this is not erased because
    -> (y : a)  -- note that `y` is a fresh variable => can be anything of the same type
    -> {t : (x : a) -> Telescope (b x)}
    -> (xs : TList (t x) args)
    -> TList (Bind bnd t) (x :: args)

  TSkip :
    {bnd : Binder a}
    -> {b : a -> Type}
    -- there is no (y : a) here, it is skipped
    -> {t : (x : a) -> Telescope (b x)}
    -> (xs : TList (t x) args)
    -> TList (Bind bnd t) (x :: args)

%used TCons x
%used TCons xs
%used TSkip xs

strip : (t : Telescope c) -> (xs : c) -> TList t xs
strip Empty [] = TNil
strip (Bind (Pi _     ) t) (Cons x xs) = TCons x $ strip (t x) xs
strip (Bind (Forall _ ) t) (Cons x xs) = TSkip   $ strip (t x) xs
strip (Bind (Default d) t) (Cons x xs) with (x)  -- with-block to work around polymorphism-related error messages
  | Just y  = TCons (Just y) $ strip (t $ Just  y) xs
  | Nothing = TCons (Just d) $ strip (t $ Nothing) xs

toTuple : (xs : List Type) -> Type
toTuple [] = NilUnit
toTuple (x :: xs) = ConsSigma x (const $ toTuple xs)

simple : (xs : List Type) -> Telescope (toTuple xs)
simple []        = Empty
simple (a :: as) = Bind (Pi a) (\x => simple as)
