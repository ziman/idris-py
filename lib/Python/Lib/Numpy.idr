module Python.Lib.Numpy

import Python
import Python.IO

import Data.Vect

%access public
%default total

record DType a where
  constructor MkDType
  dtypeName : String

DFloat : DType Float
DFloat = MkDType "float"

DInt : DType Int
DInt = MkDType "int"

abstract
record Array (rows : Nat) (cols : Nat) (dtype : DType a) where
  constructor MkArr
  ndarray : Dyn

instance Show (Array m n ty) where
  show (MkArr o) = toString o

||| The Numpy module
Numpy : Type
Numpy = Ref PyModule

private partial
import_ : PIO Numpy
import_ = importModule "numpy"

private
unsafeNumpy : (Numpy -> PIO a) -> a
unsafeNumpy action = unsafePerformIO (import_ >>= action)

private
unsafeNpArr : (Numpy -> PIO Dyn) -> Array m n dtype
unsafeNpArr = MkArr . unsafeNumpy

abstract
array : (dtype : DType a) -> Vect m (Vect n a) -> Array m n dtype
array {a=a} dtype xs = MkArr (
      unsafeNumpy $ \np =>
        np //. "array" $$: [toDyn . mkList $ map mkList xs, toDyn $ numpyName dtype]
    )
  where
    mkList : {a : Type} -> {n : Nat} -> Vect n a -> Obj (PyList a)
    mkList xs = let ys = toList xs in toPyList ys

abstract
reshape : Array m n dtype -> {auto pf : m * n = m' * n'} -> Array m' n' dtype
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
fromInteger : Num a => {ty : DType a} -> (x : Integer) -> Array m n ty
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
