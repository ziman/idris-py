module Python.Telescope

import public Data.Erased

%default total
%access public export

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
    -> Telescope (DPair argTy (b . fromArg))

term syntax "pi" {x} ":" [t] "." [rhs]
  = Bind (Pi t) (\x : t => rhs);

term syntax "forall" {x} ":" [t] "." [rhs]
  = Bind (Forall t) (\ex : Erased t => let x = unerase ex in rhs);

term syntax "default" {x} ":" [t] "=" [dflt] "." [rhs]
  = Bind (Default t dflt) (\x : t => rhs);

namespace DPairSugar
  Nil : Type
  Nil = Unit

  (::) : Type -> Type -> Type
  (::) a b = DPair a (const b)

namespace TupleSugar
  ||| Alternative name for `MkUnit`, useful for the [list, syntax, sugar].
  Nil : Unit
  Nil = ()

  ||| Infix name for `MkDPair`, useful for the [list, syntax, sugar].
  (::) : (x : a) -> (y : b x) -> DPair a b
  (::) = MkDPair

||| Convert a list of types to the corresponding tuple type.
toTuple : (xs : List Type) -> Type
toTuple [] = Unit
toTuple (x :: xs) = DPair x (const $ toTuple xs)

||| Convert a list of types to the corresponding simple telescope.
simple : (xs : List Type) -> (ret : Type) -> Telescope (toTuple xs)
simple []        ret = Return ret
simple (a :: as) ret = Bind (Pi a) (\x => simple as ret)

retTy : (t : Telescope a) -> (args : a) -> Type
retTy (Return x) () = x
retTy (Bind {fromArg = fromArg} bnd tf) (MkDPair x xs) = retTy (tf $ fromArg x) xs
