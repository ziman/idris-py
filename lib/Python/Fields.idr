module Python.Fields

import Python.Objects
import Python.IO

%default total
%access public
%language ErrorReflection

{-
-- This is a separate function because signatures don't always
-- come in this form. Therefore we need to check.
private
simplifySig : TT -> TT
simplifySig `(MkSignature ~name ~fields ~mixins) = name
simplifySig sig = sig

-- Error reflection for better error messages.
mixinErr : Err -> Maybe (List ErrorReportPart)
mixinErr (CantSolveGoal `(HasMixin ~sig ~mixin) ntms)
  = Just
      [ TermPart mixin
      , TextPart "is not mixed into signature"
      , TermPart $ simplifySig sig
      ]
mixinErr _ = Nothing

%error_handlers Python.Fields.(>.) pf mixinErr
%error_handlers Python.Fields.(>:) pf mixinErr
-}

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

{-
-- Error reflection for better error messages.
fieldErr : Err -> Maybe (List ErrorReportPart)
fieldErr (CantSolveGoal `(HasField ~sig (~fname ::: ~fty)) ntms)
  = Just
      [ TextPart "Field", TermPart fname
      , TextPart "does not exist in object signature"
      , TermPart $ simplifySig sig
      ]
fieldErr _ = Nothing

%error_handlers Python.Fields.(/.) pf fieldErr
%error_handlers Python.Fields.(/:) pf fieldErr
-}
