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
(/.) : (obj : Obj sig) -> (f : String) -> {auto pf : sig f = Attr t} -> t
(/.) {t = t} (MkObj obj) f =
  unRaw . unsafePerformIO $
    foreign FFI_Py "getattr" (Ptr -> String -> PIO (Raw t)) obj f

infixl 4 /:
||| Attribute accessor, useful for chaining.
|||
||| @ obj PIO action returning an object.
||| @ f   Name of the requested field.
(/:) : (obj : PIO (Obj sig)) -> (f : String) -> {auto pf : sig f = Attr t} -> PIO t
(/:) obj f {pf = pf} = (/. f) <$> obj

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
%error_handlers Python.Fields.(/:) pf fieldErr
