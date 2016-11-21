module Python.Lib.Threading

import Python
import Python.Prim
import Python.Exceptions

import Python.Lib.Queue

%default total
%access public export

Thread : Signature
Thread f = case f of
  "start" => [] ~~> ()
  "join"  => [] ~~> ()
  _ => Object f

Threading : Signature
Threading f = case f of
  -- the first arg must always be Nothing, according to Python spec
  "Thread" => [Maybe Void, [] ~> ()] ~~> Obj Thread
  _ => Module f

import_ : PIO $ Obj Threading
import_ = importModule "threading"

||| Fork a side thread. The thread will send its result
||| through the queue that is returned by this function.
forkPIO : PIO a -> PIO (Obj $ Queue a)
forkPIO {a = a} work = do
    queue <- Queue.import_ /: "Queue" $: [Erase a, Just 1]
    thread <- Threading.import_ /: "Thread" $: [Nothing, marshalPIO $ worker queue]
    thread /. "start" $. []

    pure queue
  where
    worker : (Obj $ Queue a) -> PIO ()
    worker queue = do
      result <- work
      queue /. "put" $. [result]

||| Wait for the result of a side thread.
wait : Obj (Queue a) -> PIO a
wait q = q /. "get" $. [1]
