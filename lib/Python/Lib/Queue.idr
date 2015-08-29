module Python.Lib.Queue

import Python
import Data.Erased

%default total
%access public

abstract
record Queue (a : Type) where
  ptr : Dyn

Queue_sig : Type -> Signature
Queue_sig a f = case f of
  "put" => [a] ~> Unit
  "get" => [Int] ~> a
  "task_done" => [] ~> Unit
  _ => Object_sig f

instance Object (Queue a) (Queue_sig a) where {}

abstract
record QueueM where
  ptr : Dyn

QueueM_sig : Signature
QueueM_sig f = case f of
  "Queue" => ParAttr _ $
    \a : Type =>
      [Int] ~~> Queue a

  _ => Module_sig f

instance Object QueueM QueueM_sig where {}
        
import_ : PIO QueueM
import_ = importModule "queue"  -- "Queue" in python2
