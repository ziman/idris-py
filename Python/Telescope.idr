module Python.Telescope

%default total
%access public

data Binder : Type where
  ||| Runtime-relevant argument.
  Pi     : Binder

  ||| Runtime-irrelevant argument.
  Forall : Binder

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
    (bnd : Binder)
    -> (a : Type)
    -> (b : a -> Type)
    -> (t : (x : a) -> Telescope (b x))
    -> Telescope (ConsSigma a b)

data TList : Telescope a -> a -> Type where
  TNil : TList Empty []

  TCons :
    {a : Type}
    -> {b : a -> Type}
    -> (x : a)
    -> {t : (x : a) -> Telescope (b x)}
    -> (xs : TList (t x) args)
    -> TList (Bind Pi a b t) (x :: args)

  TSkip :
    {a : Type}
    -> {b : a -> Type}
    -> .{x : a}
    -> {t : (x : a) -> Telescope (b x)}
    -> (xs : TList (t x) args)
    -> TList (Bind Forall a b t) (x :: args)

%used TCons x
%used TCons xs
%used TSkip xs

strip : (t : Telescope c) -> (xs : c) -> TList t xs
strip Empty [] = TNil
strip (Bind Pi     a b t) (Cons x xs) = TCons x $ strip (t x) xs
strip (Bind Forall a b t) (Cons x xs) = TSkip   $ strip (t x) xs

toTuple : (xs : List Type) -> Type
toTuple [] = NilUnit
toTuple (x :: xs) = ConsSigma x (const $ toTuple xs)

simple : (xs : List Type) -> Telescope (toTuple xs)
simple []        = Empty
simple (a :: as) = Bind Pi a (const $ toTuple as) (\x => simple as)
