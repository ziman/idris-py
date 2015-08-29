module Python.Lib.Threading

import Python

import Python.Lib.Queue

%default total
%access public

abstract record Thread where
  ptr : Dyn

Thread_sig : Signature
Thread_sig f = case f of
  "start" => [] ~> ()
  "join"  => [] ~> ()
  _ => Object_sig f

instance Object Thread Thread_sig where {}

abstract record Threading where
  ptr : Dyn

Threading_sig : Signature
Threading_sig f = case f of
  -- the first arg must always be Nothing, according to Python spec
  "Thread" => [Maybe Unit, [] ~~> Unit] ~> Thread
  _ => Module_sig f

instance Object Threading Threading_sig where {}

import_ : PIO Threading
import_ = importModule "threading"

meth :
  Object a sig
  => (o : a)
  -> (f : String)
  -> (args : HList as)
  -> {auto pf : sig f = Attr (Function as ret)}
  -> PIO ret
meth {as=as} {ret=ret} o f args = the (as ~~> ret) (o /. f) $. args

||| Fork a side thread. The thread will send its result
||| through the queue that is returned by this function.
forkPIO : PIO a -> PIO (Queue a)
forkPIO {a = a} work = do
    queueM <- Queue.import_
    threading <- Threading.import_

    queue <- queueM //. ("Queue", a) $. [Just 1]
    thread <- meth threading "Thread" [Nothing, marshalPIO $ worker queue]

    meth thread "start" []

    return queue
  where
    worker : Queue a -> PIO ()
    worker q = do
      result <- work
      meth q "put" [result]

wait : Queue a -> PIO a
wait q = meth q "get" [the Int 1]
