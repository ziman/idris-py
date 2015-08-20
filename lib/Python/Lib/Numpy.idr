module Python.Lib.Numpy

import Python
import Python.IO
import Python.RTS
import Python.Builtins

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

data NDArray : Signature where
  ndReshape : NDArray "reshape" $ [Ref NDArray, Nat, Nat] ~> Ref NDArray

data Numpy : Signature where
  npArray : Numpy "array" $ [Dyn, String] ~> Ref NDArray

abstract
record Matrix (rows : Nat) (cols : Nat) (dtype : DType a) where
  constructor MkMtx
  ndarray : Ref NDArray

instance Show (Matrix m n ty) where
  show (MkMtx $ MkRef o) = toString o

private
import_ : PIO (Ref Numpy)
import_ = MkRef <$> importModule "numpy"

private
unsafeNumpy : (Ref Numpy -> PIO a) -> a
unsafeNumpy action = unsafePerformIO (import_ >>= action)

private
unsafeNpMtx : (Ref Numpy -> PIO (Ref NDArray)) -> Matrix m n dtype
unsafeNpMtx = MkMtx . unsafeNumpy

abstract
array : (dtype : DType a) -> Vect m (Vect n a) -> Matrix m n dtype
array {a=a} (MkDType dtype) xs =
    unsafeNpMtx $ \np =>
      np /. "array" $. [toDyn . mkList $ map mkList xs, dtype]
  where
    mkList : {a : Type} -> {n : Nat} -> Vect n a -> Ref PyList
    mkList xs = cast $ toList xs

private
unsafeMtxIO : PIO (Ref NDArray) -> Matrix m n ty
unsafeMtxIO = MkMtx . unsafePerformIO

abstract
reshape : Matrix m n dtype -> {auto pf : m * n = m' * n'} -> Matrix m' n' dtype
reshape {m'=m'} {n'=n'} (MkMtx x) =
  unsafeMtxIO $
    x /. "reshape" $. [x, m', n']

{-
abstract
add : Matrix m n ty -> Matrix m n ty -> Matrix m n ty 
add (MkMtx x) (MkMtx y) = unsafeMtxIO $ x /. "__add__" $: [y]

abstract
sub : Matrix m n ty -> Matrix m n ty -> Matrix m n ty
sub (MkMtx x) (MkMtx y) = unsafeMtxIO $ x /. "__sub__" $: [y]

abstract
mul : Matrix m n ty -> Matrix m n ty -> Matrix m n ty
mul (MkMtx x) (MkMtx y) = unsafeMtxIO $ x /. "__mul__" $: [y]

abstract
div : Matrix m n ty -> Matrix m n ty -> Matrix m n ty
div (MkMtx x) (MkMtx y) = unsafeMtxIO $ x /. "__div__" $: [y]

abstract
abs : Matrix m n ty -> Matrix m n ty
abs (MkMtx x) = unsafeNpMtx $ \np => np /. "abs" $: [x]

abstract
dot : Matrix m n ty -> Matrix n k ty -> Matrix m k ty 
dot (MkMtx x) (MkMtx y) = unsafeNpMtx $ \np => np /. "dot" $: [x, y]

abstract
(/) : Matrix m n ty -> Matrix m n ty -> Matrix m n ty 
(/) = Numpy.div

abstract
transpose : Matrix m n ty -> Matrix n m ty
transpose (MkMtx x) = unsafeMtxIO $ x /. "transpose" $: []

abstract
tile : (r, c : Nat) -> Matrix m n ty -> Matrix (r*m) (c*n) ty
tile r c (MkMtx x) =
  unsafeNpMtx $ \np => np /. "tile" $: [x, listToList [r, c]] 

private
fromRef : (x : Ref) -> Matrix m n ty
fromRef {m=m} {n=n} {ty=MkDType dtype} x = unsafeNpMtx $ \np => do
  xs <- np /. "array" $: [x, toRef dtype]
  np /. "tile" $: [xs, listToList [m, n]]

abstract
fromInteger : (x : Integer) -> Matrix m n ty
fromInteger = fromRef . toRef

abstract
fromFloat : (x : Float) -> Matrix m n ty
fromFloat = fromRef . toRef

instance Num (Matrix m n ty) where
  (+) = add
  (-) = sub
  (*) = mul
  abs = Numpy.abs
  fromInteger = Numpy.fromInteger
-}
