module Python.Telescope

import public Data.Erased

%default total
%access public

||| Dependence of a binder: dependent or non-dependent.
data BinderDep : Type where
  Dependent : BinderDep
  Simple    : BinderDep

data Binder : BinderDep -> Type -> Type where
  ||| Relevant, mandatory, positional argument.
  Pi     : (a : Type) -> Binder Dependent a

  ||| Erased, mandatory, positional argument.
  Forall : (a : Type) -> Binder Dependent (Erased.Erased a)

  ||| Optional argument with a non-`None` default.
  ||| Passing "Nothing" to that argument will cause using the default value.
  ||| If you want "Nothing" to mean Python's "None", use simply `Pi (Maybe a)`.
  Default : (a : Type) -> (dflt : a) -> Binder Simple (Maybe a)
    -- ^ we make the type explicit although we don't have to

||| Type of sequences where the value of any element may affect
||| the type of the following elements.
|||
||| In other words, a dependent pair generalised to multiple elements.
data Telescope : Type -> Type where

  ||| Empty telescope.
  Return : Type -> Telescope Unit

  ||| Argument whose value may affect subsequent types.
  Dep :
    (bnd : Binder Dependent a)
    -> {b : a -> Type}
    -> (tf : (x : a) -> Telescope (b x))
    -> Telescope (Sigma a b)

  ||| Argument whose value does not affect typing.
  Simp :
    (bnd : Binder Simple a)
    -> (t : Telescope b)
    -> Telescope (Sigma a $ const b)

pi : (tf : (x : a) -> Telescope (b x)) -> Telescope (Sigma a b)
pi = Dep (Pi _)

forall : (tf : (x : a) -> Telescope (b $ Erase x)) -> Telescope (Sigma (Erased a) b)
forall {a=a} tf = Dep (Forall a) (\(Erase x) => tf x)

default : (dflt : a) -> (t : Telescope b) -> Telescope (Sigma (Maybe a) $ const b)
default dflt = Simp (Default _ dflt)

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
  TConsD :
    .{bnd : Binder Dependent a}
    -> .{b : a -> Type}
    -> .{tf : (y : a) -> Telescope (b y)}
    -> (x : a)
    -> (xs : TList (tf x) args)
    -> TList (Dep bnd tf) (x :: args)

  ||| Prepend an element in front of a `TList`, non-dependent variant.
  TConsS :
    .{bnd : Binder Simple a}
    -> .{b : Type}
    -> .{t : Telescope b}
    -> (y : a)  -- `y` may be anything of a suitable type, not necessarily `x`
    -> (xs : TList t args)
    -> TList (Simp bnd t) (x :: args)

  ||| Skip an element in the telescope.
  ||| (Comes only in the dependent form.)
  TSkip :
    .{bnd : Binder Dependent a}
    -> .{b : a -> Type}
    -- There is no (x : a) stored here, it is skipped.
    -- For the type, we assume that the value is "x" coming from args.
    -> .{tf : (x : a) -> Telescope (b x)}
    -> (xs : TList (tf x) args)
    -> TList (Dep bnd tf) (x :: args)

-- These are consumed by the FFI.
%used TConsD x
%used TConsD xs
%used TConsS x
%used TConsS xs
%used TSkip xs

||| Strip the given tuple `xs` to the `TList` of runtime-relevant values.
strip : (t : Telescope a) -> (args : a) -> TList t args
strip (Return _) () = TNil
strip (Dep (Pi _    ) tf) (x ** xs) = TConsD x $ strip (tf x) xs
strip (Dep (Forall _) tf) (x ** xs) = TSkip    $ strip (tf x) xs
strip (Simp (Default _ d) t) (Just x  ** xs) = TConsS (Just x) $ strip t xs
strip (Simp (Default _ d) t) (Nothing ** xs) = TConsS (Just d) $ strip t xs

||| Convert a list of types to the corresponding tuple type.
toTuple : (xs : List Type) -> Type
toTuple [] = Unit
toTuple (x :: xs) = Sigma x (const $ toTuple xs)

||| Convert a list of types to the corresponding simple telescope.
simple : (xs : List Type) -> (ret : Type) -> Telescope (toTuple xs)
simple []        ret = Return ret
simple (a :: as) ret = Dep (Pi a) (\x => simple as ret)

retTy : (t : Telescope a) -> (args : a) -> Type
retTy (Return x) () = x
retTy (Dep  bnd tf) (MkSigma x xs) = retTy (tf x) xs
retTy (Simp bnd t ) (MkSigma x xs) = retTy  t     xs
