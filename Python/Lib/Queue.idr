module Python.Lib.Queue

import Python

%default total
%access public

Queue : Type -> Signature
Queue a = signature "Queue"
  [ "put" ::: [a] ~> ()
  , "get" ::: [Int] ~> a
  ]

QueueM : Type -> Signature
QueueM a = signature "QueueM"
  [ "Queue" ::: [Int] ~> Obj (Queue a)
  ]

import_ : PIO $ Obj (QueueM a)
import_ = importModule "Queue"  -- this is lowercase in python3
