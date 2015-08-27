module Python.Lib.Os

import Python

%access public
%default total

abstract
record Os where
  ptr : Dyn

Os_sig : Signature
Os_sig f = case f of
  "mkdir" => [String] ~> Unit
  _ => Module_sig f

instance Object Os Os_sig where {}

import_ : PIO Os
import_ = importModule "os"
