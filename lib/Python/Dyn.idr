module Python.Dyn

import Python.IO
import Python.RTS

%access public
%default total

||| A dynamically typed Python reference
Dyn : Type
Dyn = Ptr

abstract
toDyn : a -> Dyn
toDyn = believe_me

infixl 4 /.
abstract partial
(/.) : Dyn -> String -> PIO Dyn
(/.) ref f = foreign FFI_Py "getattr" (Dyn -> String -> PIO Dyn) ref f

infixl 4 /:
abstract partial
(/:) : PIO Dyn -> String -> PIO Dyn
(/:) ref f = ref >>= (/. f)

infixl 4 $.
abstract partial
($.) : Dyn -> List Dyn -> PIO Dyn
($.) ref args = foreign FFI_Py "_idris_call" (Dyn -> List Dyn -> PIO Dyn) ref args

infixl 4 $:
abstract partial
($:) : PIO Dyn -> List Dyn -> PIO Dyn
($:) ref args = ref >>= ($. args)

abstract
toString : Dyn -> String
toString x =
  unsafePerformIO $
    foreign FFI_Py "str" (Dyn -> PIO String) x
