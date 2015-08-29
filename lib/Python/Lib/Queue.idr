module Python.Lib.Queue

import Python
import Data.Erased

%default total
%access public

Queue : Type -> Signature
Queue a f = case f of
  "put" => [a]   ~~> ()
  "get" => [Int] ~~> a
  "task_done" => [] ~~> ()
  _ => Object f

QueueM : Signature
QueueM f = case f of
  "Queue" => fun $
    forall $ \a : Type =>
      default 0 $  -- maxSize : Int
        Return $ Obj (Queue a)

  _ => Module f

import_ : PIO $ Obj QueueM
import_ = importModule "Queue"  -- this is lowercase in python3
