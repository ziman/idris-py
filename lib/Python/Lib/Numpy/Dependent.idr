module Python.Lib.Numpy.Dependent

import Python
import Python.Prim
import Python.Lib.Builtins
import Python.Lib.Numpy

import Data.Vect

%access public
%default total

record NType a where
  constructor MkNTy
  numpyName : String

NFloat : NType Float
NFloat = MkNTy "float"

NInt : NType Int
NInt = MkNTy "int"

abstract
record Array (rows : Nat) (cols : Nat) (ty : NType a) where
  constructor MkArr
  ndarray : Obj NDArray

instance Show (Array m n ty) where
  show (MkArr o) = unsafePerformIO (o /. "__str__" $: [])

private
unsafeNumpy : (Obj Numpy -> PIO a) -> a
unsafeNumpy action = unsafePerformIO (Numpy.import_ >>= action)

private
unsafeNpArr : (Obj Numpy -> PIO $ Obj NDArray) -> Array m n ty
unsafeNpArr = MkArr . unsafeNumpy

abstract
array : (ty : NType a) -> Vect m (Vect n a) -> Array m n ty
array {a=a} ty xs = MkArr (
      unsafeNumpy $ \np =>
        np /. "array" $: [Erase a, mkList $ map mkList xs, numpyName ty]
    )
  where
    mkList : {a : Type} -> {n : Nat} -> Vect n a -> Obj (PyList a)
    mkList xs = let ys = toList xs in toPyList ys

abstract
reshape : Array m n ty -> {auto pf : m * n = m' * n'} -> Array m' n' ty
reshape {m'=m'} {n'=n'} (MkArr x) =
  unsafeNpArr $ \np =>
    np /. "ndarray" /: "reshape" $: [x, cast m', cast n']

{-  -- takes ages to typecheck
abstract
add : Array m n ty -> Array m n ty -> Array m n ty 
add (MkArr x) (MkArr y) =
  unsafeNpArr $ \np => np /. "ndarray" /: "__add__" $: [x, y]

abstract
sub : Array m n ty -> Array m n ty -> Array m n ty
sub (MkArr x) (MkArr y) =
  unsafeNpArr $ \np => np /. "ndarray" /: "__sub__" $: [x, y]

abstract
mul : Array m n ty -> Array m n ty -> Array m n ty
mul (MkArr x) (MkArr y) =
  unsafeNpArr $ \np => np /. "ndarray" /: "__mul__" $: [x, y]

abstract
div : Array m n ty -> Array m n ty -> Array m n ty
div (MkArr x) (MkArr y) =
  unsafeNpArr $ \np => np /. "ndarray" /: "__div__" $: [x, y]

abstract
abs : Array m n ty -> Array m n ty
abs (MkArr x) = unsafeNpArr $ \np => np /. "abs" $: [x]

abstract
tile : (r, c : Nat) -> Array m n ty -> Array (r*m) (c*n) ty
tile r c (MkArr x) =
  unsafeNpArr $ \np => np /. "tile" $: [x, map cast [r,c]] 

abstract
fromInteger : Num a => {ty : NType a} -> (x : Integer) -> Array m n ty
fromInteger {m=m} {n=n} {ty=ty} x = unsafeNpArr $ \np => do
  xs <- np /. "array" $: [Erase _, toPyList [toPyList [Classes.fromInteger x]], numpyName ty]
  np /. "tile" $: [xs, map cast [m, n]]

instance Num (Array m n ty) where
  (+) = add
  (-) = sub
  (*) = mul
  abs = Numpy.Dependent.abs
  fromInteger x = Numpy.Dependent.fromInteger
-}
