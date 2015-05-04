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
  -- the first arg must always be Nothing
  [ "Thread" ::: [Maybe (), [] ~> ()] ~> Obj Thread
  ]

import_ : PIO $ Obj Threading
import_ = importModule "threading"

||| Fork a side thread. The thread will send its result
||| through the queue that is returned by this function.
forkPIO : PIO a -> PIO (Obj $ Queue a)
forkPIO {a = a} work = do
    queue <- Queue.import_ /: "Queue" $: (1 ** ())
    thread <- Threading.import_ /: "Thread" $: (Nothing ** (marshalPIO (worker queue) ** ()))
    thread /. "start" $: ()

    return queue
  where
    worker : (Obj $ Queue a) -> PIO ()
    worker queue = do
      result <- work
      queue /. "put" $: (result ** ())

||| Wait for the result of a side thread.
wait : Obj (Queue a) -> PIO a
wait q = q /. "get" $: (1 ** ())
