module Python.Lib.Threading

import Python
import Python.Prim
import Python.Exceptions

import Python.Lib.Queue

%default total
%access public

Thread : Signature
Thread = signature "Thread"
  [ "start" ::: [] ~> ()
  , "join" ::: [] ~> ()
  ]

Threading : Signature
Threading = signature "threading"
  [ "Thread" ::: [None, [] ~> ()] ~> Obj Thread
  ]

import_ : PIO $ Obj Threading
import_ = importModule "threading"

||| Fork a side thread. The thread will send its result
||| through the queue that is returned by this function.
forkPIO : PIO a -> PIO (Obj $ Queue a)
forkPIO work = do
  queue <- Queue.import_ /: "Queue" $: [1]
  let worker = do
        result <- work
        queue /. "put" $: [result]

  thread <- Threading.import_ /: "Thread" $: [none, marshalPIO worker]
  thread /. "start" $: []

  return queue

||| Wait for the result of a side thread.
wait : Obj (Queue a) -> PIO a
wait q = q /. "get" $: [1]
