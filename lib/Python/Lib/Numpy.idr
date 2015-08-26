module Python.Lib.Numpy

import Python
import Data.Erased
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
record Matrix (rows : Nat) (cols : Nat) (dt : DType ty) where
  ptr : Dyn

Matrix_sig : (r, c : Nat) -> (dt : DType ty) -> Signature
Matrix_sig r c dt f = case f of

    -- reshape requires preserving the number of elements
    "reshape" => fun (rr ** (cc ** (eq : Erased (r*c=rr*cc) ** ()))) $
        pi $ \rr : Nat =>
          pi $ \cc : Nat =>
            forall {a = r*c = rr*cc} $ \(Erase eq) =>
              Return $ Matrix rr cc dt

    -- transpose swaps rows/columns
    "transpose" => [] ~> Matrix c r dt

    -- less interesting ops
    "dot" => [Matrix r c dt] ~> Matrix r c dt

    _ => (Arith_sig (Matrix r c dt) <+> Object_sig) f

record PyVect (n : Nat) (a : Type) where
  constructor MkPyVect
  pyList : PyList a

pyVect : Vect n a -> PyVect n a
pyVect xs = MkPyVect $ toPyList (toList xs)

pyVect2D : Vect r (Vect c a) -> PyVect r (PyVect c a)
pyVect2D = pyVect . map pyVect

Numpy_sig : Signature
Numpy_sig f = case f of

  "abs" => fun (r ** (c ** (ty ** (dt : Erased (DType $ unerase ty) ** (m : Matrix (unerase r) (unerase c) (unerase dt) ** ()))))) $
      forall $ \(Erase r) =>
        forall $ \(Erase c) =>
          forall $ \(Erase ty) =>
            forall $ \(Erase dt) =>
              pi $ \m : Matrix r c dt =>
                Return $ Matrix r c dt

  "array" => fun (r ** (c ** (ty ** (dt : Erased (DType $ unerase ty) ** (x : PyVect (unerase r) (PyVect (unerase c) (unerase ty)) ** ()))))) $
      forall $ \(Erase r) =>
        forall $ \(Erase c) =>
          forall $ \(Erase ty) =>
            forall $ \(Erase dt) =>
              pi $ \x : PyVect r (PyVect c ty) =>
                Return $ Matrix r c dt
{-
  "array"   => [Dyn, String] ~> Arr
  "tile"    => [Arr, Ref PyList] ~> Arr
-}
  _ => Module_sig f

{-
instance Show (Matrix m n ty) where
  show m = unsafePerformIO (m /. "__str__" $. [])

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
    mkList xs = toPyList $ toList xs

private
unsafeMtxIO : PIO (Ref NDArray) -> Matrix m n ty
unsafeMtxIO = MkMtx . unsafePerformIO

abstract
reshape : Matrix m n dtype -> {auto pf : m * n = m' * n'} -> Matrix m' n' dtype
reshape {m'=m'} {n'=n'} (MkMtx x) =
  unsafeNpMtx $ \np =>
    np /. "reshape" $. [x, toPyList [m', n']]

private
binop :
  (op : String)
  -> {auto pf : NDArray op = [Ref NDArray, Ref NDArray] ~> Ref NDArray}
  -> Matrix m n ty -> Matrix m n ty -> Matrix m n ty
binop op (MkMtx x) (MkMtx y) = unsafeNpMtx $ \np => np /. "ndarray" /. op $. [x, y]

abstract
add : Matrix m n ty -> Matrix m n ty -> Matrix m n ty 
add = binop "__add__"

abstract
sub : Matrix m n ty -> Matrix m n ty -> Matrix m n ty
sub = binop "__sub__"

abstract
mul : Matrix m n ty -> Matrix m n ty -> Matrix m n ty
mul = binop "__mul__"

abstract
div : Matrix m n ty -> Matrix m n ty -> Matrix m n ty
div = binop "__div__"

abstract
abs : Matrix m n ty -> Matrix m n ty
abs (MkMtx x) = unsafeNpMtx $ \np => np /. "abs" $. [x]

abstract
dot : Matrix m n ty -> Matrix n k ty -> Matrix m k ty 
dot (MkMtx x) (MkMtx y) = unsafeNpMtx $ \np => np /. "dot" $. [x, y]

abstract
(/) : Matrix m n ty -> Matrix m n ty -> Matrix m n ty 
(/) = Numpy.div

abstract
transpose : Matrix m n ty -> Matrix n m ty
transpose (MkMtx x) = unsafeNpMtx $ \np => np /. "transpose" $. [x]

abstract
tile : (r, c : Nat) -> Matrix m n ty -> Matrix (r*m) (c*n) ty
tile r c (MkMtx x) =
  unsafeNpMtx $ \np => np /. "tile" $. [x, toPyList [r, c]] 

private
fromDyn : (x : Dyn) -> Matrix m n ty
fromDyn {m=m} {n=n} {ty = MkDType dtype} x =
  unsafeNpMtx $ \np => do
    xs <- np /. "array" $. [x, dtype]
    np /. "tile" $. [xs, toPyList [m, n]]

abstract
fromInteger : (x : Integer) -> Matrix m n ty
fromInteger = fromDyn . toDyn

abstract
fromFloat : (x : Float) -> Matrix m n ty
fromFloat = fromDyn . toDyn

instance Num (Matrix m n ty) where
  (+) = add
  (-) = sub
  (*) = mul
  abs = Numpy.abs
  fromInteger = Numpy.fromInteger
-}
