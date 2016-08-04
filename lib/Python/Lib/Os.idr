module Python.Lib.Os

import Python

%access public export
%default total

Os : Signature
Os f = case f of
  "mkdir" => [String] ~~> Unit
  _ => Module f

import_ : PIO (Obj Os)
import_ = importModule "os"
