module Python.Telescope

%default total
%access public

data Binder : Type where
  ||| Runtime-relevant argument.
  Pi     : Binder

  ||| Runtime-irrelevant argument.
  Forall : Binder

data Telescope : Type -> Type where
  Empty :
    Telescope Unit

  Bind :
    (bnd : Binder)
    -> (a : Type)
    -> (b : a -> Type)
    -> (t : (x : a) -> Telescope (b x))
    -> Telescope (Sigma a b)

data TList : Telescope a -> a -> Type where
  TNil : TList Empty ()

  TCons :
    {a : Type}
    -> {b : a -> Type}
    -> (x : a)
    -> {t : (x : a) -> Telescope (b x)}
    -> (xs : TList (t x) args)
    -> TList (Bind Pi a b t) (MkSigma x args)

  TSkip :
    {a : Type}
    -> {b : a -> Type}
    -> .{x : a}
    -> {t : (x : a) -> Telescope (b x)}
    -> (xs : TList (t x) args)
    -> TList (Bind Forall a b t) (MkSigma x args)

%used TCons x
%used TCons xs
%used TSkip xs

strip : (t : Telescope c) -> (x : c) -> TList t x
strip Empty () = TNil
strip (Bind Pi     a b t) (MkSigma x args) = TCons x $ strip (t x) args
strip (Bind Forall a b t) (MkSigma x args) = TSkip   $ strip (t x) args

toTuple : (xs : List Type) -> Type
toTuple [] = ()
toTuple (x :: xs) = Sigma x (const $ toTuple xs)

simple : (xs : List Type) -> Telescope (toTuple xs)
simple []        = Empty
simple (a :: as) = Bind Pi a (const $ toTuple as) (\x => simple as)
