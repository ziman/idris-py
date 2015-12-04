module Python.Fields

import Python.Objects
import Python.IO

%default total
%access public
%language ErrorReflection

infixl 4 /.
||| Attribute accessor.
|||
||| @ obj Obj with the given signature.
||| @ f   Name of the requested field.
abstract
(/.) :
  (obj : Obj sig) -> (f : String)
  -> {auto pf : sig f = Attr t}
  -> t
(/.) {t = t} (MkObj obj) f =
  unRaw . unsafePerformIO $
    foreign FFI_Py "getattr" (Ptr -> String -> PIO (Raw t)) obj f

data FieldParams : Type -> Type where
  FP : String -> .(params : pt) -> FieldParams pt

fpName : FieldParams pt -> String
fpName (FP n ps) = n

fpParams : FieldParams pt -> pt
fpParams (FP n ps) = ps

infixl 4 //.
abstract
(//.) :
  (obj : Obj sig) -> (fps : FieldParams pt)
  -> {auto pf : sig (fpName fps) = PAttr pt tf}
  -> tf (fpParams fps)
(//.) {tf=tf} (MkObj obj) (FP f ps) =
  unRaw . unsafePerformIO $
    foreign FFI_Py "getattr" (Ptr -> String -> PIO (Raw $ tf ps)) obj f

infixl 4 /:
||| Attribute accessor, useful for chaining.
|||
||| @ obj PIO action returning an object.
||| @ f   Name of the requested field.
(/:) :
  (obj : PIO (Obj sig)) -> (f : String)
  -> {auto pf : sig f = Attr t}
  -> PIO t
(/:) obj f = (/. f) <$> obj

infixl 4 //:
(//:) :
  (obj : PIO (Obj sig)) -> (fps : FieldParams pt)
  -> {auto pf : sig (fpName fps) = PAttr pt tf}
  -> PIO (tf (fpParams fps))
(//:) obj fps = (//. fps) <$> obj

-- Error reflection for better error messages.
fieldErr : Err -> Maybe (List ErrorReportPart)
fieldErr (CantSolveGoal `(~(App sig fn) = Attr ~ty) ntms)
  = Just
      [ TextPart "Field", TermPart fn
      , TextPart "does not exist in object signature", TermPart sig
      , TextPart "with the type", TermPart ty
      ]
fieldErr _ = Nothing

%error_handlers Python.Fields.(/.) pf fieldErr
%error_handlers Python.Fields.(//.) pf fieldErr
%error_handlers Python.Fields.(/:) pf fieldErr
%error_handlers Python.Fields.(//:) pf fieldErr
