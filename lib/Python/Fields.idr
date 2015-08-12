module Python.Fields

import Python.Objects
import Python.IO

%default total
%access public
%language ErrorReflection

using (n : String)
  ||| Proof that a signature contains a field of the given type.
  ||| (We don't use List.Elem to keep the signature name in the error message.)
  data HasField : Signature -> Field -> Type where
    FieldHere  :
         MkSignature n (f :: fs) ms `HasField` f
    FieldThere : MkSignature n fs ms `HasField` f
      -> MkSignature n (f' :: fs) ms `HasField` f
    InMixinThere : MkSignature n [] ms `HasField` f
      -> MkSignature n [] (sig :: ms) `HasField` f
    InMixinHere : sig `HasField` f
      -> MkSignature n [] (sig :: ms) `HasField` f

using (n : String)
  ||| Proof that an object can be cast to the signature of one of its mixins.
  data HasMixin : (sig : Signature) -> (mixin : Signature) -> Type where
    MixinHere : MkSignature n fs (m :: ms) `HasMixin` m
    MixinThere : MkSignature n fs ms `HasMixin` m -> MkSignature n fs (m' :: ms) `HasMixin` m

||| Cast an object to the signature of one of its mixins.
mixout : (mixin : Signature) -> {auto pf : sig `HasMixin` mixin} -> Obj sig -> Obj mixin
mixout mixin (MkObj obj) = MkObj obj

infixl 2 >.
||| Cast an object to the signature of one of its mixins.
(>.) : (obj : Obj sig) -> (mixin : Signature) -> {auto pf : sig `HasMixin` mixin} -> Obj mixin
(>.) obj mixin = mixout mixin obj

infixl 2 >:
||| Cast an object to the signature of one of its mixins, chained version.
(>:) : (obj : PIO (Obj sig)) -> (mixin : Signature) -> {auto pf : sig `HasMixin` mixin} -> PIO (Obj mixin)
(>:) obj mixin {pf = pf} = mixout mixin {pf = pf} <$> obj

-- This is a separate function because signatures don't always
-- come in this form. Therefore we need to check.
--
-- FIXME: elaboration of the body of this function takes very long
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

infixl 4 /.
||| Attribute accessor.
|||
||| @ obj Obj with the given signature.
||| @ f   Name of the requested field.
abstract
(/.) : (obj : Obj sig) -> (f : String) -> {auto pf : sig `HasField` (f ::: t)} -> PIO t
(/.) {t = t} (MkObj obj) f =
  unRaw <$>
    foreign FFI_Py "getattr" (Ptr -> String -> PIO (Raw t)) obj f

infixl 4 /:
||| Attribute accessor, useful for chaining.
|||
||| @ obj PIO action returning an object.
||| @ f   Name of the requested field.
(/:) : (obj : PIO (Obj sig)) -> (f : String) -> {auto pf : sig `HasField` (f ::: t)} -> PIO t
(/:) obj f {pf = pf} = obj >>= \o => (/.) o f {pf}

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
