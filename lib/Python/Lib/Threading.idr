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

meth : Object a sig
  => (x : a)
  -> (f : String)
  -> (args : as)
  -> {t : Telescope as}
  -> {auto pf : sig f = Attr (Function t)}
  -> PIO $ retTy t args
meth {t=t} x f args = the (Function t) (x /. f) $. args

||| Fork a side thread. The thread will send its result
||| through the queue that is returned by this function.
forkPIO : PIO a -> PIO (Queue a)
forkPIO {a = a} work = do
    queueM <- Queue.import_
    threading <- Threading.import_

    queue <- mkQ queueM
    thread <- mkT queue threading

    meth thread "start" ()

    return queue
  where
    worker : Queue a -> PIO ()
    worker q = do
      result <- work
      the ([a] ~~> ()) (q /. "put") $. [result]

    mkQ : QueueM -> PIO (Queue a)
    mkQ qM = meth qM "Queue" (Erase a ** (Just 1 ** ()))

    mkT : Queue a -> Threading -> PIO Thread
    mkT q tM = meth tM "Thread" (Nothing ** (marshalPIO (worker q) ** ()))

wait : Queue a -> PIO a
wait q = the ([Int] ~~> a) (q /. "get") $. [1]
