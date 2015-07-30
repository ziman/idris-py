module Python.Telescope

import public Data.Erased

%default total
%access public

namespace TupleSugar
  ||| Alternative name for `MkUnit`, useful for the [list, syntax, sugar].
  Nil : Unit
  Nil = ()

  ||| Infix name for `MkSigma`, useful for the [list, syntax, sugar].
  (::) : (x : a) -> (y : b x) -> Sigma a b
  (::) = MkSigma

data DepBinder : Type -> Type where
  ||| Relevant, mandatory, positional argument.
  Pi     : (a : Type) -> DepBinder a

  ||| Erased, mandatory, positional argument.
  Forall : (a : Type) -> DepBinder (Erased.Erased a)

data NondepBinder : Type -> Type where
  ||| Optional argument with a non-`None` default.
  ||| Passing "Nothing" to that argument will cause using the default value.
  ||| If you want "Nothing" to mean Python's "None", use simply `Pi (Maybe a)`.
  Default : (a : Type) -> (dflt : a) -> NondepBinder (Maybe a)
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
    (bnd : DepBinder a)
    -> {b : a -> Type}
    -> (tf : (x : a) -> Telescope (b x))
    -> Telescope (Sigma a b)

  ||| Argument whose value does not affect typing.
  Nondep :
    (bnd : NondepBinder a)
    -> (t : Telescope b)
    -> Telescope (Sigma a $ const b)


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
    .{bnd : DepBinder a}
    -> .{b : a -> Type}
    -> .{tf : (y : a) -> Telescope (b y)}
    -> (x : a)
    -> (xs : TList (tf x) args)
    -> TList (Dep bnd tf) (x :: args)

  ||| Prepend an element in front of a `TList`, non-dependent variant.
  TConsN :
    .{bnd : NondepBinder a}
    -> .{b : Type}
    -> .{t : Telescope b}
    -> (y : a)  -- `y` may be anything of a suitable type, not necessarily `x`
    -> (xs : TList t args)
    -> TList (Nondep bnd t) (x :: args)

  ||| Skip an element in the telescope.
  ||| (Comes only in the dependent form.)
  TSkip :
    .{bnd : DepBinder a}
    -> .{b : a -> Type}
    -- There is no (x : a) stored here, it is skipped.
    -- For the type, we assume that the value is "x" coming from args.
    -> .{tf : (x : a) -> Telescope (b x)}
    -> (xs : TList (tf x) args)
    -> TList (Dep bnd tf) (x :: args)

-- These are consumed by the FFI.
%used TConsD x
%used TConsD xs
%used TConsN x
%used TConsN xs
%used TSkip xs

||| Strip the given tuple `xs` to the `TList` of runtime-relevant values.
strip : (t : Telescope a) -> (args : a) -> TList t args
strip (Return _) () = TNil
strip (Dep (Pi _    ) tf) (x ** xs) = TConsD x $ strip (tf x) xs
strip (Dep (Forall _) tf) (x ** xs) = TConsD x $ strip (tf x) xs
strip (Nondep (Default _ d) t) (Just x  ** xs) = TConsN (Just x) $ strip t xs
strip (Nondep (Default _ d) t) (Nothing ** xs) = TConsN (Just d) $ strip t xs

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
retTy (Dep    bnd tf) (MkSigma x xs) = retTy (tf x) xs
retTy (Nondep bnd t ) (MkSigma x xs) = retTy  t     xs
