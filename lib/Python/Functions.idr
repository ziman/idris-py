module Python.Functions

import Python.Objects
import Python.Fields
import Python.Telescope
import Python.IO

%default total
%access public export

Function : (t : Telescope a) -> Signature
Function {a = a} t f = case f of
  "__call__" => Call {a = a} t
  _ => Object f

infix 5 ~>
||| Infix alias for functions with fixed arguments.
(~>) : List Type -> Type -> Type
(~>) args ret = Obj . Function $ simple args ret

infix 5 ~~>
(~~>) : List Type -> Type -> Field
(~~>) args ret = Attr $ args ~> ret

fun : (t : Telescope a) -> Field
fun t = Attr . Obj $ Function t

||| Strip the given tuple `xs` to the list of runtime-relevant values.
strip : (t : Telescope a) -> (args : a) -> List Dyn
strip (Return _)              ()        = []
strip (Bind (Pi      _  ) tf) (x ** xs) = toDyn x :: strip (tf x) xs
strip (Bind (Forall  _  ) tf) (x ** xs) = strip (tf x) xs
strip (Bind (Default _ d) tf) (x ** xs) = toDyn x :: strip (tf $ fromMaybe d x) xs

infixl 4 $.
||| Duck-typed function call.
($.) :
  {t : Telescope a}
  -> (f : Obj sig)
  -> {auto pf : sig "__call__" = Call t}
  -> (args : a)
  -> PIO $ retTy t args
($.) {t = t} (MkObj f) args =
  unRaw <$>
    foreign FFI_Py "_idris_call"
      (Ptr -> List Dyn -> PIO (Raw $ Telescope.retTy t args))
      f
      (strip t args)

infixl 4 $:
||| Duck-typed function call, useful for chaining.
($:) :
  {t : Telescope a}
  -> (f : PIO (Obj sig))
  -> {auto pf : sig "__call__" = Call t}
  -> (args : a)
  -> PIO $ retTy t args
($:) meth args = meth >>= \m => m $. args
