module Python.Lib.Os

import Python

%access public
%default total

Os : Signature
Os = signature "Os"
  [ "mkdir" ::: [String] ~> ()
  ]

import_ : PIO (Obj Os)
import_ = importModule "os"
