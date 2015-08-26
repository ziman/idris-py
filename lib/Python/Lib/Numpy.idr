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

abstract
record Numpy where
  ptr : Dyn

Numpy_sig : Signature
Numpy_sig f = case f of

  "abs" => fun (r ** (c ** (ty ** (dt : Erased (DType $ unerase ty) ** (m : Matrix (unerase r) (unerase c) (unerase dt) ** ()))))) $
      forall $ \(Erase r) =>
        forall $ \(Erase c) =>
          forall $ \(Erase ty) =>
            forall $ \(Erase dt) =>
              pi $ \m : Matrix r c dt =>
                Return $ Matrix r c dt

  "array" => fun (r ** (c ** (ty ** (x : PyVect (unerase r) (PyVect (unerase c) (unerase ty)) ** (dt : DType (unerase ty) ** ()))))) $
      forall $ \(Erase r) =>
        forall $ \(Erase c) =>
          forall $ \(Erase ty) =>
            pi $ \x : PyVect r (PyVect c ty) =>
              pi $ \dt : DType ty =>
                Return $ Matrix r c dt

  _ => Module_sig f

instance Show (Matrix m n dt) where
  show m = unsafePerformIO (m /. "__str__" $. [])

private
import_ : PIO Numpy
import_ = importModule "numpy"

private
unsafeNumpy : (Numpy -> PIO a) -> a
unsafeNumpy action = unsafePerformIO (import_ >>= action)

abstract
(/) : Matrix m n ty -> Matrix m n ty -> Matrix m n ty 
(/) x y = unsafePerformIO (x /. "__div__" $. [y])

abstract
fromInteger : {dt : DType a} -> (x : Integer) -> Matrix m n dt
fromInteger {a=a} {m=m} {n=n} {dt=dt} x =
  unsafeNumpy $ \np =>
    np /. "array" $. [m, n, a, pyVect2D $ replicate m (replicate n x), dt]

abstract
fromFloat : (x : Float) -> Matrix m n ty
fromFloat = fromDyn . toDyn

instance Num (Matrix m n ty) where
  (+) = add
  (-) = sub
  (*) = mul
  abs = Numpy.abs
  fromInteger = Numpy.fromInteger
