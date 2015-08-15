module Python

import Python.IO

%default total
%access public

||| A dynamically typed Python reference
Ref : Type
Ref = Ptr

abstract
toRef : a -> Ref
toRef = believe_me

infixl 4 /.
abstract partial
(/.) : Ref -> String -> PIO Ref
(/.) ref f = foreign FFI_Py "getattr" (Ref -> String -> PIO Ref) ref f

infixl 4 /:
abstract partial
(/:) : PIO Ref -> String -> PIO Ref
(/:) ref f = ref >>= (/. f)

infixl 4 $.
abstract partial
($.) : Ref -> List Ref -> PIO Ref
($.) ref args = foreign FFI_Py "_idris_call" (Ref -> List Ref -> PIO Ref) ref args

infixl 4 $:
abstract partial
($:) : PIO Ref -> List Ref -> PIO Ref
($:) ref args = ref >>= ($. args)

abstract
toString : Ref -> String
toString x =
  unsafePerformIO $
    foreign FFI_Py "str" (Ref -> PIO String) x
