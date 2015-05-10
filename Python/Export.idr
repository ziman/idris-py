module Python.Export

import Python.Objects
import Python.IO

%default total
%access public

||| Defines which functions are exportable.
data Exportable : Type -> Type where
  ERet : PyTypes a -> Exportable (PIO a)
  EPi  : PyTypes a -> Exportable b -> Exportable (a -> b)

private
esize : Exportable a -> Int
esize (ERet _)  = 0
esize (EPi _ e) = 1 + esize e

||| Export the given function so that it's visible (and callable)
||| when the generated module is imported from Python.
abstract
export : {auto pf : Exportable a} -> (name : String) -> (f : a) -> PIO ()
export {a = a} {pf = pf} name f =
    foreign FFI_Py "_idris_export"
      (Int -> String -> Raw a -> PIO ())
      (esize pf)
      name
      (MkRaw f)

||| Execute the given action iff the Python module `__name__` is `__main__`.
|||
||| This is necessary because the Idris function `Main.main` is executed
||| whenever the generated Python module is *loaded*.
abstract
ifMain : PIO () -> PIO ()
ifMain m = foreign FFI_Py "_idris_if_main" (Raw (PIO ()) -> PIO ()) (MkRaw m)
