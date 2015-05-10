module Python.Functions

import Python.Objects
import Python.Fields
import Python.Telescope
import Python.IO

%default total
%access public

||| A special field for callable objects.
abstract
record CallField (t : Telescope a) where
  constructor MkCallField

Function : (t : Telescope a) -> Signature
Function t = signature "Function"
  [ "__call__" ::: CallField t
  ]

infixr 5 ~>
||| Infix alias for functions with fixed arguments.
(~>) : List Type -> Type -> Signature
(~>) args ret = Function $ simple args ret

||| Duct-typed function call.
call :
  {t : Telescope a}
  -> (f : Obj sig)
  -> {auto pf : sig `HasField` ("__call__" ::: CallField t)}
  -> (args : a)
  -> PIO (Telescope.retTy t args)
call {t = t} (MkObj f) args =
  unRaw <$>
    foreign FFI_Py "_idris_call"
      (Ptr -> (TList t args) -> PIO (Raw $ Telescope.retTy t args))
      f
      (strip t args)

infixl 4 $.
||| Function call specialised to ``Function t``.
||| Works better with inference than ``call``.
abstract
($.) :
  {t : Telescope a}
  -> (f : Obj $ Function t)
  -> (args : a)
  -> PIO (Telescope.retTy t args)
($.) f args = call f args

infixl 4 $:
||| Function call, useful for chaining.
($:) :
  {t : Telescope a}
  -> (f : PIO (Obj $ Function t))
  -> (args : a)
  -> PIO (Telescope.retTy t args)
($:) meth args = meth >>= \m => m $. args
