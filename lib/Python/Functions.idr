module Python.Functions

import Python.Objects
import Python.Fields
import Python.Telescope
import Python.IO

%default total
%access public

Function : (t : Telescope a) -> Signature
Function t f = case f of
  "__call__" => Call t
  _ => Object f

infix 5 ~>
||| Infix alias for functions with fixed arguments.
(~>) : List Type -> Type -> Signature
(~>) args ret = Function $ simple args ret

infix 5 ~~>
(~~>) : List Type -> Type -> Field
(~~>) args ret = Attr $ Obj (args ~> ret)

fun : (a : Type) -> (t : Telescope a) -> Field
fun a t = Attr . Obj $ Function t

||| Duck-typed function call.
call :
  {t : Telescope a}
  -> (f : Obj sig)
  -> {auto pf : sig "__call__" = Call t}
  -> (args : a)
  -> PIO $ retTy t args
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
  -> (f : Obj sig)
  -> {auto pf : sig "__call__" = Call t}
  -> (args : a)
  -> PIO $ retTy t args
{-
  {t : Telescope a}
  -> (f : Obj $ Function t)
  -> (args : a)
  -> PIO $ retTy t args
-}
($.) f args = call f args

infixl 4 $:
||| Function call, useful for chaining.
($:) :
  {t : Telescope a}
  -> (f : PIO (Obj $ Function t))
  -> (args : a)
  -> PIO $ retTy t args
($:) meth args = meth >>= \m => m $. args
