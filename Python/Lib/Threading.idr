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
  [ "Thread" ::: [PIO ()] ~> Obj Thread
  ]

import_ : PIO $ Obj Threading
import_ = importModule "threading"

forkPIO : PIO a -> PIO (Obj $ Queue a)
forkPIO work = do
  queue <- Queue.import_ /: "Queue" $: [1]
  let worker = do
        result <- work
        queue /. "put" $: [result]

  thread <- Threading.import_ /: "Thread" $: [worker]
  thread /. "start" $: []

  return queue

wait : Obj (Queue a) -> PIO a
wait q = q /. "get" $: [1]
