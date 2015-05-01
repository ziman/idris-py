module Python.Lib.Queue

import Python

%default total
%access public

Queue : Type -> Signature
Queue a = signature "Queue"
  [ "put" ::: [a] ~> ()
  , "get" ::: [Int] ~> a
  , "task_done" ::: [] ~> ()
  ]

QueueM : Type -> Signature
QueueM a = signature "QueueM"
  [ "Queue" ::: [Int] ~> Obj (Queue a)
  ]

-- Hack: since we don't have polymorphism yet, we have to import
-- the whole module parametrised by the queue element type.
import_ : PIO $ Obj (QueueM a)
import_ = importModule "Queue"  -- this is lowercase in python3
