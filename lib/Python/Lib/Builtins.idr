module Python.Lib.Builtins

import Data.Erased

import Python
import Python.Prim

%access public
%default total

Builtins : Signature
Builtins = signature "Builtins"
    [ "list" ::. Function (
        Dep (Forall Type) rest {- $ \a =>
          Dep (Pi $ List (unerase a)) $ \xs =>
            Return (Obj $ PyList (unerase a))
        -}
      )
    ]
  where
    rest : (ea : Erased Type) -> Telescope (Sigma (List $ unerase ea) (\_ => ()))
    rest (Erase a) = 
      Dep (Pi $ List a) $ \xs =>
        Return (Obj $ PyList a)

abstract
import_ : PIO $ Obj Builtins
import_ = getGlobal "__builtins__"

||| Convert an Idris list to a true Python list object.
toPyList : List a -> Obj (PyList a)
toPyList {a = a} xs = unsafePerformIO $ do
  builtins <- Builtins.import_
  builtins /. "list" $: [Erase a, xs]
