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

QueueM : Signature
QueueM = signature "QueueM"
  [ "Queue" :::
      Function 
        (Bind (Forall Type) $ \a =>  -- The type of elements
          Bind (Pi Int) $ \i =>      -- Maximum size of queue
            Empty
        )
        (\(Cons (Erase a) (Cons maxSize _)) => Obj (Queue a))
  ]

import_ : PIO $ Obj QueueM
import_ = importModule "Queue"  -- this is lowercase in python3
