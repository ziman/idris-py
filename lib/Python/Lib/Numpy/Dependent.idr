module Python.Lib.Numpy.Dependent

import Python
import Python.Prim
import Python.Lib.Builtins
import Python.Lib.Numpy

import Data.Vect

%access public
%default total

record NType where
  constructor MkNTy
  numpyName : String
  idrisType : Type

NFloat : NType
NFloat = MkNTy "float" Float

NInt : NType
NInt = MkNTy "int" Int

abstract
record Array (rows : Nat) (cols : Nat) (ty : NType) where
  constructor MkArr
  ndarray : Obj NDArray

instance Show (Array m n ty) where
  show (MkArr o) = unsafePerformIO (o /. "__str__" $: [])

private
unsafeNumpy : (Obj Numpy -> PIO a) -> a
unsafeNumpy action = unsafePerformIO (Numpy.import_ >>= action)

abstract
array : (ty : NType) -> Vect m (Vect n $ idrisType ty) -> Array m n ty
array ty xs = MkArr (
      unsafeNumpy $ \np =>
        np /. "array" $: [Erase _, mkList $ map mkList xs, numpyName ty]
    )
  where
    mkList : Vect n a -> Obj (PyList a)
    mkList xs = let ys = toList xs in toPyList ys
