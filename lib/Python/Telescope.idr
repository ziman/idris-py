module Python.Telescope

import public Data.Erased

%hide Language.Reflection.Binder

%default total
%access public

data Binder : (argTy : Type) -> (depTy : Type) -> (argTy -> depTy) -> Type where
  ||| Relevant, mandatory, positional argument.
  Pi : (a : Type) -> Binder a a Basics.id

  ||| Erased, mandatory, positional argument.
  Forall : (a : Type) -> Binder (Erased a) (Erased a) Basics.id

  ||| An argument with a default value.
  Default : (a : Type) -> (d : a) -> Binder (Maybe a) a (fromMaybe d)

||| Type of sequences where the value of any element may affect
||| the type of the following elements.
|||
||| In other words, a dependent pair generalised to multiple elements.
data Telescope : Type -> Type where

  ||| Empty telescope.
  Return : Type -> Telescope Unit

  ||| Value on which subsequent types may depend.
  Bind :
    (bnd : Binder argTy depTy fromArg)
    -> {b : depTy -> Type}
    -> (tf : (x : depTy) -> Telescope (b x))
    -> Telescope (Sigma argTy (b . fromArg))

term syntax "pi" {x} ":" [t] "." [rhs]
  = Bind (Pi t) (\x : t => rhs);

term syntax "forall" {x} ":" [t] "." [rhs]
  = Bind (Forall t) (\ex : Erased t => let x = unerase ex in rhs);

term syntax "default" {x} ":" [t] "=" [dflt] "." [rhs]
  = Bind (Default t dflt) (\x : t => rhs);

namespace SigmaSugar
  Nil : Type
  Nil = Unit

  (::) : Type -> Type -> Type
  (::) a b = Sigma a (const b)

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
    .{bnd : Binder argTy depTy fromArg}
    -> .{b : depTy -> Type}
    -> .{tf : (q : depTy) -> Telescope (b q)}
    -> (x : argTy)
    -> (xs : TList (tf $ fromArg x) args)
    -> TList (Bind bnd tf) (x :: args)

  TSkip :
  .{bnd : Binder argTy depTy fromArg}
    -> .{b : depTy -> Type}
    -> .{tf : (y : depTy) -> Telescope (b y)}
    -- There is no (x : argTy) stored here, it is skipped.
    -- For the type, we assume that the value is "x" coming from args.
    -> (xs : TList (tf $ fromArg x) args)
    -> TList (Bind bnd tf) (x :: args)

-- These are consumed by the FFI.
%used TCons x
%used TCons xs
%used TSkip xs

||| Strip the given tuple `xs` to the `TList` of runtime-relevant values.
strip : (t : Telescope a) -> (args : a) -> TList t args
strip (Return _) () = TNil
strip (Bind (Pi      _  ) tf) (x ** xs) = TCons x $ strip (tf (id x)) xs
strip (Bind (Forall  _  ) tf) (x ** xs) = TSkip   $ strip (tf x) xs
strip (Bind (Default _ d) tf) (x ** xs)
  = TCons x $ strip (tf $ fromMaybe d x) xs

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
retTy (Bind {fromArg = fromArg} bnd tf) (MkSigma x xs) = retTy (tf $ fromArg x) xs
