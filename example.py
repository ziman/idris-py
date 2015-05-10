#!/usr/bin/env python

import sys
import collections

Unit = collections.namedtuple('Unit', [])()
World = collections.namedtuple('World', [])()

class IdrisError(Exception):
  pass

def _idris_error(msg):
  raise IdrisError(msg)

_MODULES = dict()

def _idris_pymodule(name):
  mod = _MODULES.get(name)
  if mod is None:
    mod = __import__(name)
    _MODULES[name] = mod
  return mod

def _idris_call(f, args):
  return f(*list(args))

def _idris_foreach(it, st, f):
  for x in it:
    # Apply st, x, world
    st = APPLY0(APPLY0(APPLY0(f, st), x), World)
  return st

def _idris_try(f, fail, succ):
  try:
    result = APPLY0(f, World)  # apply to world
    return APPLY0(succ, result)
  except Exception as e:
    return APPLY0(fail, e)

def _idris_raise(e):
  raise e

def _idris_marshal_PIO(action):
  return lambda: APPLY0(action, World)  # delayed apply-to-world

def _idris_export(argcnt, name, fun):
  def pyfn(*args):
    if len(args) != argcnt:
      raise TypeError('function %s takes exactly %d arguments, got %d' % (name, argcnt, len(args)))

    f = fun  # work around Python's scoping rules
    for x in args:
      f[0] = APPLY0(f[0], x)

    return APPLY0(f, World)  # last step: apply to world

  globals()[name] = pyfn

def _idris_if_main(main):
  if __name__ == '__main__':
    APPLY0(main, World)  # apply to world

class _ConsIter(object):
  def __init__(self, node):
    self.node = node

  def next(self):
    if self.node.isNil:
      raise StopIteration
    else:
      result = self.node.head
      self.node = self.node.tail
      return result

class ConsList(object):
  def __init__(self, isNil=True, head=None, tail=None):
    self.isNil = isNil
    self.head  = head
    self.tail  = tail

  def __nonzero__(self):
    return not self.isNil

  def __len__(self):
    cnt = 0
    while not self.isNil:
      self = self.tail
      cnt += 1
    return cnt

  def cons(self, x):
    return ConsList(isNil=False, head=x, tail=self)

  def __iter__(self):
    return _ConsIter(self)

# Tail calls + trampolines inspired by:
# http://kylem.net/programming/tailcall.html
TailCall = collections.namedtuple('TailCall', 'f args')

class TailWrapper(object):
  def __init__(self, f):
    self.f = f

  def __call__(self, *args):
    f = self.f
    while True:
       ret = f(*args)
       if type(ret) is TailCall:
         f, args = ret
         if type(f) is TailWrapper:
           f = f.f
         continue
       return ret

def tailcaller(f):
  return TailWrapper(f)

@tailcaller  # Python.Functions.$.
def _idris_Python_46_Functions_46__36__46_(e0, e1, e2, e3):
  return TailCall(
    _idris_Python_46_Functions_46_call,
    (None, None, e1, e2, None, e3)
  )

@tailcaller  # Python.Functions.$:
def _idris_Python_46_Functions_46__36__58_(e0, e1, e2, e3):
  return (65794, None, None, None, e2, (65761, e1, e3))  # {U_io_bind1}, {U_Python.Functions.{$:0}1}

@tailcaller  # Prelude.Basics..
def _idris_Prelude_46_Basics_46__46_(e0, e1, e2, e3, e4, _idris_x):
  return TailCall(APPLY0, (e3, APPLY0(e4, _idris_x)))

@tailcaller  # Python.Fields./.
def _idris_Python_46_Fields_46__47__46_(e0, e1, e2, e3, e4):
  return TailCall(
    _idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0,
    (None, None, None, (65763, None), (65759, e2, e3))  # {U_Python.IO.unRaw1}, {U_Python.Fields.{/.0}1}
  )

@tailcaller  # Python.Fields./:
def _idris_Python_46_Fields_46__47__58_(e0, e1, e2, e3, e4):
  return (65794, None, None, None, e2, (65760, e3))  # {U_io_bind1}, {U_Python.Fields.{/:0}1}

# Prelude.Algebra.<+>
def _idris_Prelude_46_Algebra_46__60__43__62_(e0, e1):
  return e1

@tailcaller  # Python.Fields.>:
def _idris_Python_46_Fields_46__62__58_(e0, e1, e2, e3):
  return TailCall(
    _idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0,
    (None, None, None, (65758, None, None, None), e1)  # {U_Python.Fields.mixout1}
  )

@tailcaller  # Prelude.Monad.>>=
def _idris_Prelude_46_Monad_46__62__62__61_(e0, e1, e2, e3):
  if e3[0] == 0:  # constructor of Prelude.Monad.Monad
    in0, in1, = e3[1:]
    aux1 = TailCall(APPLY0, (APPLY0(in1, e1), e2))
  return aux1

# @@constructor of Prelude.Algebra.Monoid#Semigroup a
def _idris__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a(
  e0, e1
):
  if e1[0] == 0:  # constructor of Prelude.Algebra.Monoid
    in0, in1, = e1[1:]
    aux1 = in0
  return aux1

# @@constructor of Prelude.Monad.Monad#Applicative m
def _idris__64__64_constructor_32_of_32_Prelude_46_Monad_46_Monad_35_Applicative_32_m(
  e0, e1
):
  if e1[0] == 0:  # constructor of Prelude.Monad.Monad
    in0, in1, = e1[1:]
    aux1 = in0
  return aux1

@tailcaller  # Python.Functions.call
def _idris_Python_46_Functions_46_call(e0, e1, e2, e3, e4, e5):
  return TailCall(
    _idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0,
    (None, None, None, (65763, None), (65762, e3, e2, e5))  # {U_Python.IO.unRaw1}, {U_Python.Functions.{call0}1}
  )

@tailcaller  # call__IO
def _idris_call_95__95_IO(e0, e1, e2):
  return TailCall(APPLY0, (e2, None))

@tailcaller  # Python.Exceptions.catch
def _idris_Python_46_Exceptions_46_catch(e0, e1, e2):
  return (65794, None, None, None, e1, (65728, e2))  # {U_io_bind1}, {U_Python.Exceptions.{catch6}1}

@tailcaller  # Python.Prim.collect
def _idris_Python_46_Prim_46_collect(e0, e1):
  return TailCall(
    _idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0,
    (
      None,
      None,
      None,
      (65716, None, ConsList()),  # {U_Prelude.List.reverse, reverse'1}
      _idris_Python_46_Prim_46_foreach(None, None, e1, ConsList(), (65775,))  # {U_Python.Prim.{collect1}1}
    )
  )

@tailcaller  # Prelude.Foldable.concat
def _idris_Prelude_46_Foldable_46_concat(e0, e1, e2, e3):
  if e3[0] == 0:  # constructor of Prelude.Algebra.Monoid
    in0, in1, = e3[1:]
    aux1 = in0
  if e3[0] == 0:  # constructor of Prelude.Algebra.Monoid
    in2, in3, = e3[1:]
    aux2 = in3
  return TailCall(
    APPLY0,
    (APPLY0(_idris_Prelude_46_Foldable_46_foldr(None, None, None, e2), aux1), aux2)
  )

# Python.Export.esize
def _idris_Python_46_Export_46_esize(e0, e1):
  if e1[0] == 1:  # Python.Export.EPi
    in0, = e1[1:]
    aux1 = 1 + _idris_Python_46_Export_46_esize(None, in0)
  elif e1[0] == 0:  # Python.Export.ERet
    aux1 = 0
  return aux1

# Python.Export.export
def _idris_Python_46_Export_46_export(e0, e1, e2, e3, _idris_w):
  return _idris_export(_idris_Python_46_Export_46_esize(None, e1), e2, e3)

@tailcaller  # Prelude.Foldable.foldr
def _idris_Prelude_46_Foldable_46_foldr(e0, e1, e2, e3):
  return TailCall(APPLY0, (APPLY0(e3, e1), e2))

@tailcaller  # Python.Prim.foreach
def _idris_Python_46_Prim_46_foreach(e0, e1, e2, e3, e4):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Functions_46__36__58_(
      None,
      (0,),  # Python.Telescope.Return
      _idris_Python_46_Fields_46__47__46_(None, None, e2, "__iter__", None),
      Unit
    ),
    (65777, e2, e3, e4)  # {U_Python.Prim.{foreach1}1}
  )

@tailcaller  # Python.Lib.Threading.forkPIO
def _idris_Python_46_Lib_46_Threading_46_forkPIO(e0, e1):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Functions_46__36__58_(
      None,
      (1, (2,), (65765,)),  # Python.Telescope.Bind, Python.Telescope.Forall, {U_Python.Lib.Threading.{forkPIO1}1}
      _idris_Python_46_Fields_46__47__58_(
        None,
        None,
        _idris_Python_46_Lib_46_Queue_46_import_95_(),
        "Queue",
        None
      ),
      (0, (0,), (0, 1, Unit))  # Builtins.MkSigma, Data.Erased.Erase, Builtins.MkSigma
    ),
    (65770, e1)  # {U_Python.Lib.Threading.{forkPIO6}1}
  )

# Python.Export.ifMain
def _idris_Python_46_Export_46_ifMain(e0, _idris_w):
  return _idris_if_main(e0)

# Python.importModule
def _idris_Python_46_importModule(e0, e1, _idris_w):
  return _idris_pymodule(e1)

@tailcaller  # Python.Lib.BeautifulSoup.import_
def _idris_Python_46_Lib_46_BeautifulSoup_46_import_95_():
  return (65792, None, "bs4")  # {U_Python.importModule1}

@tailcaller  # Python.Lib.Os.import_
def _idris_Python_46_Lib_46_Os_46_import_95_():
  return (65792, None, "os")  # {U_Python.importModule1}

@tailcaller  # Python.Lib.Queue.import_
def _idris_Python_46_Lib_46_Queue_46_import_95_():
  return (65792, None, "Queue")  # {U_Python.importModule1}

@tailcaller  # Python.Lib.Requests.import_
def _idris_Python_46_Lib_46_Requests_46_import_95_():
  return (65792, None, "requests")  # {U_Python.importModule1}

@tailcaller  # Python.Lib.Threading.import_
def _idris_Python_46_Lib_46_Threading_46_import_95_():
  return (65792, None, "threading")  # {U_Python.importModule1}

@tailcaller  # io_bind
def _idris_io_95_bind(e0, e1, e2, e3, e4, _idris_w):
  return TailCall(APPLY0, (io_bind2(e0, e1, e2, e3, e4, _idris_w), APPLY0(e3, _idris_w)))

# io_return
def _idris_io_95_return(e0, e1, e2, _idris_w):
  return e2

@tailcaller  # Python.Prim.iterate
def _idris_Python_46_Prim_46_iterate(e0, e1, e2, e3, e4):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Functions_46__36__58_(
      None,
      (0,),  # Python.Telescope.Return
      _idris_Python_46_Fields_46__47__46_(None, None, e2, "__iter__", None),
      Unit
    ),
    (65778, e3, e4)  # {U_Python.Prim.{iterate0}1}
  )

@tailcaller  # Prelude.Strings.length
def _idris_Prelude_46_Strings_46_length():
  return (
    65713,  # {U_Prelude.Basics..1}
    None,
    None,
    None,
    (65713, None, None, None, (65717,), (65796,)),  # {U_Prelude.Basics..1}, {U_Prelude.Strings.{length0}1}, {U_prim__zextInt_BigInt1}
    (65797,)  # {U_prim_lenString1}
  )

@tailcaller  # Main.main
def _idris_Main_46_main():
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    (65756, None, (0,), "greet", _idris_Prelude_46_putStr(None, "Hello world!\n")),  # {U_Python.Export.export1}, Python.Export.ERet
    (65712,)  # {U_Main.{main0}1}
  )

@tailcaller  # Main.main'
def _idris_Main_46_main_39_():
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Lib_46_Requests_46_import_95_(),
    (65707,)  # {U_Main.{main'69}1}
  )

@tailcaller  # Python.marshalPIO
def _idris_Python_46_marshalPIO(e0, e1):
  return TailCall(_idris_unsafePerformIO, (None, None, (65793, e1)))  # {U_Python.{marshalPIO0}1}

# Python.Fields.mixout
def _idris_Python_46_Fields_46_mixout(e0, e1, e2, e3):
  return e3

# mkForeignPrim
def _idris_mkForeignPrim():
  return None

# Prelude.Algebra.neutral
def _idris_Prelude_46_Algebra_46_neutral(e0, e1):
  if e1[0] == 0:  # constructor of Prelude.Algebra.Monoid
    in0, in1, = e1[1:]
    aux1 = in1
  return aux1

@tailcaller  # Python.Prim.next
def _idris_Python_46_Prim_46_next(e0, e1):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Exceptions_46_try(
      None,
      _idris_Python_46_Functions_46__36__58_(
        None,
        (0,),  # Python.Telescope.Return
        _idris_Python_46_Fields_46__47__46_(None, None, e1, "next", None),
        Unit
      )
    ),
    (65782,)  # {U_Python.Prim.{next12}1}
  )

# prim__addInt
def _idris_prim_95__95_addInt(op0, op1):
  return op0 + op1

# prim__concat
def _idris_prim_95__95_concat(op0, op1):
  return op0 + op1

# prim__null
def _idris_prim_95__95_null():
  return None

# prim__readFile
def _idris_prim_95__95_readFile(op0, op1):
  return _idris_error("unimplemented external: prim__readFile")

# prim__registerPtr
def _idris_prim_95__95_registerPtr(op0, op1):
  return _idris_error("unimplemented external: prim__registerPtr")

# prim__stderr
def _idris_prim_95__95_stderr():
  return _idris_error("unimplemented external: prim__stderr")

# prim__stdin
def _idris_prim_95__95_stdin():
  return _idris_error("unimplemented external: prim__stdin")

# prim__stdout
def _idris_prim_95__95_stdout():
  return _idris_error("unimplemented external: prim__stdout")

# prim__toStrBigInt
def _idris_prim_95__95_toStrBigInt(op0):
  return str(op0)

# prim__toStrInt
def _idris_prim_95__95_toStrInt(op0):
  return str(op0)

# prim__vm
def _idris_prim_95__95_vm():
  return _idris_error("unimplemented external: prim__vm")

# prim__writeFile
def _idris_prim_95__95_writeFile(op0, op1, op2):
  return _idris_error("unimplemented external: prim__writeFile")

# prim__writeString
def _idris_prim_95__95_writeString(op0, op1):
  return sys.stdout.write(op1)

# prim__zextInt_BigInt
def _idris_prim_95__95_zextInt_95_BigInt(op0):
  return op0

@tailcaller  # prim_io_bind
def _idris_prim_95_io_95_bind(e0, e1, e2, e3):
  return TailCall(APPLY0, (e3, e2))

# prim_lenString
def _idris_prim_95_lenString(op0):
  return len(op0)

@tailcaller  # Prelude.Applicative.pure
def _idris_Prelude_46_Applicative_46_pure(e0, e1, e2):
  return TailCall(APPLY0, (e2, e1))

@tailcaller  # Prelude.putStr
def _idris_Prelude_46_putStr(e0, e1):
  return (65794, None, None, None, (65718, e1), (65719,))  # {U_io_bind1}, {U_Prelude.{putStr0}1}, {U_Prelude.{putStr1}1}

@tailcaller  # Python.Exceptions.raise
def _idris_Python_46_Exceptions_46_raise(e0, e1):
  return TailCall(
    _idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0,
    (None, None, None, (65763, None), (65729, e1))  # {U_Python.IO.unRaw1}, {U_Python.Exceptions.{raise0}1}
  )

@tailcaller  # run__IO
def _idris_run_95__95_IO(e0, e1):
  return TailCall(APPLY0, (e1, None))

@tailcaller  # Python.Exceptions.showException
def _idris_Python_46_Exceptions_46_showException(e0):
  return TailCall(_idris_unsafePerformIO, (None, None, (65730, e0)))  # {U_Python.Exceptions.{showException0}1}

# Python.Telescope.strip
def _idris_Python_46_Telescope_46_strip(e0, e1, e2):
  if e1[0] == 1:  # Python.Telescope.Bind
    in0, in1, = e1[1:]
    if in0[0] == 1:  # Python.Telescope.Default
      if e2[0] == 0:  # Builtins.MkSigma
        in2, in3, = e2[1:]
        if in2 is not None:  # Prelude.Maybe.Just
          in4 = in2
          aux4 = _idris_Python_46_Telescope_46_strip(None, APPLY0(in1, in4), in3).cons(in4)
        elif in2 is None:  # Prelude.Maybe.Nothing
          aux4 = _idris_Python_46_Telescope_46_strip(None, APPLY0(in1, None), in3).cons(None)
        aux3 = aux4
      aux2 = aux3
    elif in0[0] == 2:  # Python.Telescope.Forall
      if e2[0] == 0:  # Builtins.MkSigma
        in5, in6, = e2[1:]
        aux5 = _idris_Python_46_Telescope_46_strip(None, APPLY0(in1, in5), in6)
      aux2 = aux5
    elif in0[0] == 0:  # Python.Telescope.Pi
      if e2[0] == 0:  # Builtins.MkSigma
        in7, in8, = e2[1:]
        aux6 = _idris_Python_46_Telescope_46_strip(None, APPLY0(in1, in7), in8).cons(in7)
      aux2 = aux6
    aux1 = aux2
  elif e1[0] == 0:  # Python.Telescope.Return
    aux1 = ConsList()
  return aux1

@tailcaller  # Python.Exceptions.try
def _idris_Python_46_Exceptions_46_try(e0, e1):
  return (65794, None, None, None, (65750, e1), (65747,))  # {U_io_bind1}, {U_Python.Exceptions.{try4}1}, {U_Python.Exceptions.{try24}1}

# Python.IO.unRaw
def _idris_Python_46_IO_46_unRaw(e0, e1):
  return e1

@tailcaller  # unsafePerformIO
def _idris_unsafePerformIO(e0, e1, e2):
  return TailCall(APPLY0, (unsafePerformIO1(e0, e1, e2), APPLY0(e2, None)))

# unsafePerformPrimIO
def _idris_unsafePerformPrimIO():
  return None

@tailcaller  # Python.Lib.Threading.wait
def _idris_Python_46_Lib_46_Threading_46_wait(e0, e1):
  return TailCall(
    _idris_Python_46_Functions_46__36__58_,
    (
      None,
      (1, (0,), (65771,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Threading.{wait0}1}
      _idris_Python_46_Fields_46__47__46_(None, None, e1, "get", None),
      (0, 1, Unit)  # Builtins.MkSigma
    )
  )

# world
def _idris_world(e0):
  return e0

@tailcaller  # Python.Functions.{$:0}
def _idris_Python_46_Functions_46__123__36__58_0_125_(e1, e3, in0):
  return TailCall(
    _idris_Python_46_Functions_46__36__46_,
    (None, e1, in0, e3)
  )

# Python.Fields.{/.0}
def _idris_Python_46_Fields_46__123__47__46_0_125_(e2, e3, in0):
  return getattr(e2, e3)

@tailcaller  # Python.Fields.{/:0}
def _idris_Python_46_Fields_46__123__47__58_0_125_(e3, in0):
  return TailCall(
    _idris_Python_46_Fields_46__47__46_,
    (None, None, in0, e3, None)
  )

@tailcaller  # {APPLY0}
def APPLY0(fn0, arg0):
  if fn0[0] < 65729:
    if fn0[0] < 65683:
      if fn0[0] < 65660:
        if fn0[0] < 65649:
          if fn0[0] < 65643:
            if fn0[0] < 65640:
              if fn0[0] == 65638:  # {U_Main.{case block in main'_lam0}1}
                aux1 = TailCall(
                  _idris_Main_46__123_case_32_block_32_in_32_main_39__95_lam0_125_,
                  (arg0,)
                )
              elif fn0[0] == 65639:  # {U_Main.{case block in main'_lam1}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_case_32_block_32_in_32_main_39__95_lam1_125_,
                  (P_c0, P_c1, arg0)
                )
            else:
              if fn0[0] == 65640:  # {U_Main.{case block in main'_lam2}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_case_32_block_32_in_32_main_39__95_lam2_125_,
                  (P_c0, P_c1, arg0)
                )
              elif fn0[0] == 65641:  # {U_Main.{case block in main'_lam3}1}
                P_c0, P_c1, P_c2, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_case_32_block_32_in_32_main_39__95_lam3_125_,
                  (P_c0, P_c1, P_c2, arg0)
                )
              elif fn0[0] == 65642:  # {U_Main.{main'0}1}
                aux1 = TailCall(_idris_Main_46__123_main_39_0_125_, (arg0,))
          else:
            if fn0[0] < 65646:
              if fn0[0] == 65643:  # {U_Main.{main'10}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_10_125_,
                  (arg0,)
                )
              elif fn0[0] == 65644:  # {U_Main.{main'11}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_11_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65645:  # {U_Main.{main'12}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_12_125_,
                  (P_c0, arg0)
                )
            else:
              if fn0[0] == 65646:  # {U_Main.{main'13}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_13_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65647:  # {U_Main.{main'14}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_14_125_,
                  (arg0,)
                )
              elif fn0[0] == 65648:  # {U_Main.{main'15}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_15_125_,
                  (arg0,)
                )
        else:
          if fn0[0] < 65654:
            if fn0[0] < 65651:
              if fn0[0] == 65649:  # {U_Main.{main'16}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_16_125_,
                  (arg0,)
                )
              elif fn0[0] == 65650:  # {U_Main.{main'17}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_17_125_,
                  (P_c0, arg0)
                )
            else:
              if fn0[0] == 65651:  # {U_Main.{main'18}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_18_125_,
                  (arg0,)
                )
              elif fn0[0] == 65652:  # {U_Main.{main'19}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_19_125_,
                  (arg0,)
                )
              elif fn0[0] == 65653:  # {U_Main.{main'1}1}
                aux1 = TailCall(_idris_Main_46__123_main_39_1_125_, (arg0,))
          else:
            if fn0[0] < 65657:
              if fn0[0] == 65654:  # {U_Main.{main'20}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_20_125_,
                  (arg0,)
                )
              elif fn0[0] == 65655:  # {U_Main.{main'21}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_21_125_,
                  (arg0,)
                )
              elif fn0[0] == 65656:  # {U_Main.{main'22}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_22_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65657:  # {U_Main.{main'23}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_23_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65658:  # {U_Main.{main'24}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_24_125_,
                  (arg0,)
                )
              elif fn0[0] == 65659:  # {U_Main.{main'25}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_25_125_,
                  (arg0,)
                )
      else:
        if fn0[0] < 65671:
          if fn0[0] < 65665:
            if fn0[0] < 65662:
              if fn0[0] == 65660:  # {U_Main.{main'26}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_26_125_,
                  (arg0,)
                )
              elif fn0[0] == 65661:  # {U_Main.{main'27}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_27_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65662:  # {U_Main.{main'28}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_28_125_,
                  (arg0,)
                )
              elif fn0[0] == 65663:  # {U_Main.{main'29}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_29_125_,
                  (arg0,)
                )
              elif fn0[0] == 65664:  # {U_Main.{main'2}1}
                aux1 = TailCall(_idris_Main_46__123_main_39_2_125_, (arg0,))
          else:
            if fn0[0] < 65668:
              if fn0[0] == 65665:  # {U_Main.{main'30}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_30_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65666:  # {U_Main.{main'31}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_31_125_,
                  (arg0,)
                )
              elif fn0[0] == 65667:  # {U_Main.{main'32}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_32_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65668:  # {U_Main.{main'33}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_33_125_,
                  (arg0,)
                )
              elif fn0[0] == 65669:  # {U_Main.{main'34}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_34_125_,
                  (arg0,)
                )
              elif fn0[0] == 65670:  # {U_Main.{main'35}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_35_125_,
                  (arg0,)
                )
        else:
          if fn0[0] < 65677:
            if fn0[0] < 65674:
              if fn0[0] == 65671:  # {U_Main.{main'36}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_36_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65672:  # {U_Main.{main'37}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_37_125_,
                  (arg0,)
                )
              elif fn0[0] == 65673:  # {U_Main.{main'38}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_38_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65674:  # {U_Main.{main'39}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_39_125_,
                  (arg0,)
                )
              elif fn0[0] == 65675:  # {U_Main.{main'3}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_3_125_,
                  (P_c0, P_c1, arg0)
                )
              elif fn0[0] == 65676:  # {U_Main.{main'40}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_40_125_,
                  (P_c0, arg0)
                )
          else:
            if fn0[0] < 65680:
              if fn0[0] == 65677:  # {U_Main.{main'41}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_41_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65678:  # {U_Main.{main'42}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_42_125_,
                  (P_c0, P_c1, arg0)
                )
              elif fn0[0] == 65679:  # {U_Main.{main'43}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_43_125_,
                  (P_c0, arg0)
                )
            else:
              if fn0[0] == 65680:  # {U_Main.{main'44}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_44_125_,
                  (arg0,)
                )
              elif fn0[0] == 65681:  # {U_Main.{main'45}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_45_125_,
                  (arg0,)
                )
              elif fn0[0] == 65682:  # {U_Main.{main'46}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_46_125_,
                  (P_c0, arg0)
                )
    else:
      if fn0[0] < 65706:
        if fn0[0] < 65694:
          if fn0[0] < 65688:
            if fn0[0] < 65685:
              if fn0[0] == 65683:  # {U_Main.{main'47}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_47_125_,
                  (arg0,)
                )
              elif fn0[0] == 65684:  # {U_Main.{main'48}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_48_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65685:  # {U_Main.{main'49}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_49_125_,
                  (arg0,)
                )
              elif fn0[0] == 65686:  # {U_Main.{main'4}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_4_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65687:  # {U_Main.{main'50}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_50_125_,
                  (P_c0, arg0)
                )
          else:
            if fn0[0] < 65691:
              if fn0[0] == 65688:  # {U_Main.{main'51}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_51_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65689:  # {U_Main.{main'52}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_52_125_,
                  (arg0,)
                )
              elif fn0[0] == 65690:  # {U_Main.{main'53}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_53_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65691:  # {U_Main.{main'54}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_54_125_,
                  (arg0,)
                )
              elif fn0[0] == 65692:  # {U_Main.{main'55}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_55_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65693:  # {U_Main.{main'56}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_56_125_,
                  (P_c0, arg0)
                )
        else:
          if fn0[0] < 65700:
            if fn0[0] < 65697:
              if fn0[0] == 65694:  # {U_Main.{main'57}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_57_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65695:  # {U_Main.{main'58}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_58_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65696:  # {U_Main.{main'59}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_59_125_,
                  (P_c0, arg0)
                )
            else:
              if fn0[0] == 65697:  # {U_Main.{main'5}1}
                aux1 = TailCall(_idris_Main_46__123_main_39_5_125_, (arg0,))
              elif fn0[0] == 65698:  # {U_Main.{main'60}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_60_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65699:  # {U_Main.{main'61}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_61_125_,
                  (P_c0, arg0)
                )
          else:
            if fn0[0] < 65703:
              if fn0[0] == 65700:  # {U_Main.{main'62}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_62_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65701:  # {U_Main.{main'63}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_63_125_,
                  (P_c0, P_c1, arg0)
                )
              elif fn0[0] == 65702:  # {U_Main.{main'64}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_64_125_,
                  (P_c0, arg0)
                )
            else:
              if fn0[0] == 65703:  # {U_Main.{main'65}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_65_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65704:  # {U_Main.{main'66}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_66_125_,
                  (P_c0, P_c1, arg0)
                )
              elif fn0[0] == 65705:  # {U_Main.{main'67}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_67_125_,
                  (P_c0, arg0)
                )
      else:
        if fn0[0] < 65717:
          if fn0[0] < 65711:
            if fn0[0] < 65708:
              if fn0[0] == 65706:  # {U_Main.{main'68}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_68_125_,
                  (arg0,)
                )
              elif fn0[0] == 65707:  # {U_Main.{main'69}1}
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_69_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65708:  # {U_Main.{main'6}1}
                aux1 = TailCall(_idris_Main_46__123_main_39_6_125_, (arg0,))
              elif fn0[0] == 65709:  # {U_Main.{main'7}1}
                aux1 = TailCall(_idris_Main_46__123_main_39_7_125_, (arg0,))
              elif fn0[0] == 65710:  # {U_Main.{main'8}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Main_46__123_main_39_8_125_,
                  (P_c0, arg0)
                )
          else:
            if fn0[0] < 65714:
              if fn0[0] == 65711:  # {U_Main.{main'9}1}
                aux1 = TailCall(_idris_Main_46__123_main_39_9_125_, (arg0,))
              elif fn0[0] == 65712:  # {U_Main.{main0}1}
                aux1 = TailCall(_idris_Main_46__123_main0_125_, (arg0,))
              elif fn0[0] == 65713:  # {U_Prelude.Basics..1}
                P_c0, P_c1, P_c2, P_c3, P_c4, = fn0[1:]
                aux1 = TailCall(
                  _idris_Prelude_46_Basics_46__46_,
                  (P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
                )
            else:
              if fn0[0] == 65714:  # {U_Prelude.Functor.{Prelude.IO' ffi instance of Prelude.Functor.Functor, method map_lam0}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Prelude_46_Functor_46__123_Prelude_46_IO_39__32_ffi_32_instance_32_of_32_Prelude_46_Functor_46_Functor_44__32_method_32_map_95_lam0_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65715:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable1}
                P_c0, P_c1, P_c2, P_c3, = fn0[1:]
                aux1 = TailCall(
                  _idris_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List,
                  (P_c0, P_c1, P_c2, P_c3, arg0)
                )
              elif fn0[0] == 65716:  # {U_Prelude.List.reverse, reverse'1}
                P_c0, P_c1, = fn0[1:]
                aux1 = TailCall(
                  _idris_Prelude_46_List_46_reverse_58_reverse_39__58_0,
                  (P_c0, P_c1, arg0)
                )
        else:
          if fn0[0] < 65723:
            if fn0[0] < 65720:
              if fn0[0] == 65717:  # {U_Prelude.Strings.{length0}1}
                aux1 = TailCall(
                  _idris_Prelude_46_Strings_46__123_length0_125_,
                  (arg0,)
                )
              elif fn0[0] == 65718:  # {U_Prelude.{putStr0}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Prelude_46__123_putStr0_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65719:  # {U_Prelude.{putStr1}1}
                aux1 = TailCall(
                  _idris_Prelude_46__123_putStr1_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65720:  # {U_Python.Exceptions.{case block in case block in try_lam0}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_case_32_block_32_in_32_case_32_block_32_in_32_try_95_lam0_125_,
                  (P_c0, P_c1, arg0)
                )
              elif fn0[0] == 65721:  # {U_Python.Exceptions.{case block in try_lam0}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_case_32_block_32_in_32_try_95_lam0_125_,
                  (P_c0, P_c1, arg0)
                )
              elif fn0[0] == 65722:  # {U_Python.Exceptions.{catch0}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_catch0_125_,
                  (arg0,)
                )
          else:
            if fn0[0] < 65726:
              if fn0[0] == 65723:  # {U_Python.Exceptions.{catch1}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_catch1_125_,
                  (arg0,)
                )
              elif fn0[0] == 65724:  # {U_Python.Exceptions.{catch2}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_catch2_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65725:  # {U_Python.Exceptions.{catch3}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_catch3_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65726:  # {U_Python.Exceptions.{catch4}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_catch4_125_,
                  (arg0,)
                )
              elif fn0[0] == 65727:  # {U_Python.Exceptions.{catch5}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_catch5_125_,
                  (arg0,)
                )
              elif fn0[0] == 65728:  # {U_Python.Exceptions.{catch6}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_catch6_125_,
                  (P_c0, arg0)
                )
  else:
    if fn0[0] < 65775:
      if fn0[0] < 65752:
        if fn0[0] < 65740:
          if fn0[0] < 65734:
            if fn0[0] < 65731:
              if fn0[0] == 65729:  # {U_Python.Exceptions.{raise0}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_raise0_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65730:  # {U_Python.Exceptions.{showException0}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_showException0_125_,
                  (P_c0, arg0)
                )
            else:
              if fn0[0] == 65731:  # {U_Python.Exceptions.{try0}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try0_125_,
                  (arg0,)
                )
              elif fn0[0] == 65732:  # {U_Python.Exceptions.{try10}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try10_125_,
                  (arg0,)
                )
              elif fn0[0] == 65733:  # {U_Python.Exceptions.{try11}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try11_125_,
                  (arg0,)
                )
          else:
            if fn0[0] < 65737:
              if fn0[0] == 65734:  # {U_Python.Exceptions.{try12}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try12_125_,
                  (arg0,)
                )
              elif fn0[0] == 65735:  # {U_Python.Exceptions.{try13}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try13_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65736:  # {U_Python.Exceptions.{try14}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try14_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65737:  # {U_Python.Exceptions.{try15}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try15_125_,
                  (arg0,)
                )
              elif fn0[0] == 65738:  # {U_Python.Exceptions.{try16}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try16_125_,
                  (arg0,)
                )
              elif fn0[0] == 65739:  # {U_Python.Exceptions.{try17}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try17_125_,
                  (P_c0, arg0)
                )
        else:
          if fn0[0] < 65746:
            if fn0[0] < 65743:
              if fn0[0] == 65740:  # {U_Python.Exceptions.{try18}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try18_125_,
                  (arg0,)
                )
              elif fn0[0] == 65741:  # {U_Python.Exceptions.{try19}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try19_125_,
                  (arg0,)
                )
              elif fn0[0] == 65742:  # {U_Python.Exceptions.{try1}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try1_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65743:  # {U_Python.Exceptions.{try20}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try20_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65744:  # {U_Python.Exceptions.{try21}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try21_125_,
                  (arg0,)
                )
              elif fn0[0] == 65745:  # {U_Python.Exceptions.{try22}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try22_125_,
                  (arg0,)
                )
          else:
            if fn0[0] < 65749:
              if fn0[0] == 65746:  # {U_Python.Exceptions.{try23}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try23_125_,
                  (arg0,)
                )
              elif fn0[0] == 65747:  # {U_Python.Exceptions.{try24}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try24_125_,
                  (arg0,)
                )
              elif fn0[0] == 65748:  # {U_Python.Exceptions.{try2}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try2_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65749:  # {U_Python.Exceptions.{try3}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try3_125_,
                  (arg0,)
                )
              elif fn0[0] == 65750:  # {U_Python.Exceptions.{try4}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try4_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65751:  # {U_Python.Exceptions.{try5}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try5_125_,
                  (arg0,)
                )
      else:
        if fn0[0] < 65763:
          if fn0[0] < 65757:
            if fn0[0] < 65754:
              if fn0[0] == 65752:  # {U_Python.Exceptions.{try6}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try6_125_,
                  (arg0,)
                )
              elif fn0[0] == 65753:  # {U_Python.Exceptions.{try7}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try7_125_,
                  (P_c0, arg0)
                )
            else:
              if fn0[0] == 65754:  # {U_Python.Exceptions.{try8}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try8_125_,
                  (arg0,)
                )
              elif fn0[0] == 65755:  # {U_Python.Exceptions.{try9}1}
                aux1 = TailCall(
                  _idris_Python_46_Exceptions_46__123_try9_125_,
                  (arg0,)
                )
              elif fn0[0] == 65756:  # {U_Python.Export.export1}
                P_c0, P_c1, P_c2, P_c3, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Export_46_export,
                  (P_c0, P_c1, P_c2, P_c3, arg0)
                )
          else:
            if fn0[0] < 65760:
              if fn0[0] == 65757:  # {U_Python.Export.ifMain1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Export_46_ifMain,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65758:  # {U_Python.Fields.mixout1}
                P_c0, P_c1, P_c2, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Fields_46_mixout,
                  (P_c0, P_c1, P_c2, arg0)
                )
              elif fn0[0] == 65759:  # {U_Python.Fields.{/.0}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Fields_46__123__47__46_0_125_,
                  (P_c0, P_c1, arg0)
                )
            else:
              if fn0[0] == 65760:  # {U_Python.Fields.{/:0}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Fields_46__123__47__58_0_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65761:  # {U_Python.Functions.{$:0}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Functions_46__123__36__58_0_125_,
                  (P_c0, P_c1, arg0)
                )
              elif fn0[0] == 65762:  # {U_Python.Functions.{call0}1}
                P_c0, P_c1, P_c2, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Functions_46__123_call0_125_,
                  (P_c0, P_c1, P_c2, arg0)
                )
        else:
          if fn0[0] < 65769:
            if fn0[0] < 65766:
              if fn0[0] == 65763:  # {U_Python.IO.unRaw1}
                P_c0, = fn0[1:]
                aux1 = TailCall(_idris_Python_46_IO_46_unRaw, (P_c0, arg0))
              elif fn0[0] == 65764:  # {U_Python.Lib.Threading.{forkPIO0}1}
                aux1 = TailCall(
                  _idris_Python_46_Lib_46_Threading_46__123_forkPIO0_125_,
                  (arg0,)
                )
              elif fn0[0] == 65765:  # {U_Python.Lib.Threading.{forkPIO1}1}
                aux1 = TailCall(
                  _idris_Python_46_Lib_46_Threading_46__123_forkPIO1_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65766:  # {U_Python.Lib.Threading.{forkPIO2}1}
                aux1 = TailCall(
                  _idris_Python_46_Lib_46_Threading_46__123_forkPIO2_125_,
                  (arg0,)
                )
              elif fn0[0] == 65767:  # {U_Python.Lib.Threading.{forkPIO3}1}
                aux1 = TailCall(
                  _idris_Python_46_Lib_46_Threading_46__123_forkPIO3_125_,
                  (arg0,)
                )
              elif fn0[0] == 65768:  # {U_Python.Lib.Threading.{forkPIO4}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Lib_46_Threading_46__123_forkPIO4_125_,
                  (P_c0, arg0)
                )
          else:
            if fn0[0] < 65772:
              if fn0[0] == 65769:  # {U_Python.Lib.Threading.{forkPIO5}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Lib_46_Threading_46__123_forkPIO5_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65770:  # {U_Python.Lib.Threading.{forkPIO6}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Lib_46_Threading_46__123_forkPIO6_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65771:  # {U_Python.Lib.Threading.{wait0}1}
                aux1 = TailCall(
                  _idris_Python_46_Lib_46_Threading_46__123_wait0_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65772:  # {U_Python.Prim.collect1}
                P_c0, = fn0[1:]
                aux1 = TailCall(_idris_Python_46_Prim_46_collect, (P_c0, arg0))
              elif fn0[0] == 65773:  # {U_Python.Prim.{case block in Python.Prim.iterate, iter_lam0}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_case_32_block_32_in_32_Python_46_Prim_46_iterate_44__32_iter_95_lam0_125_,
                  (P_c0, P_c1, arg0)
                )
              elif fn0[0] == 65774:  # {U_Python.Prim.{collect0}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_collect0_125_,
                  (P_c0, arg0)
                )
    else:
      if fn0[0] < 65798:
        if fn0[0] < 65786:
          if fn0[0] < 65780:
            if fn0[0] < 65777:
              if fn0[0] == 65775:  # {U_Python.Prim.{collect1}1}
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_collect1_125_,
                  (arg0,)
                )
              elif fn0[0] == 65776:  # {U_Python.Prim.{foreach0}1}
                P_c0, P_c1, P_c2, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_foreach0_125_,
                  (P_c0, P_c1, P_c2, arg0)
                )
            else:
              if fn0[0] == 65777:  # {U_Python.Prim.{foreach1}1}
                P_c0, P_c1, P_c2, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_foreach1_125_,
                  (P_c0, P_c1, P_c2, arg0)
                )
              elif fn0[0] == 65778:  # {U_Python.Prim.{iterate0}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_iterate0_125_,
                  (P_c0, P_c1, arg0)
                )
              elif fn0[0] == 65779:  # {U_Python.Prim.{next0}1}
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_next0_125_,
                  (arg0,)
                )
          else:
            if fn0[0] < 65783:
              if fn0[0] == 65780:  # {U_Python.Prim.{next10}1}
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_next10_125_,
                  (arg0,)
                )
              elif fn0[0] == 65781:  # {U_Python.Prim.{next11}1}
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_next11_125_,
                  (arg0,)
                )
              elif fn0[0] == 65782:  # {U_Python.Prim.{next12}1}
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_next12_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65783:  # {U_Python.Prim.{next1}1}
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_next1_125_,
                  (arg0,)
                )
              elif fn0[0] == 65784:  # {U_Python.Prim.{next2}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_next2_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65785:  # {U_Python.Prim.{next3}1}
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_next3_125_,
                  (arg0,)
                )
        else:
          if fn0[0] < 65792:
            if fn0[0] < 65789:
              if fn0[0] == 65786:  # {U_Python.Prim.{next4}1}
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_next4_125_,
                  (arg0,)
                )
              elif fn0[0] == 65787:  # {U_Python.Prim.{next5}1}
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_next5_125_,
                  (arg0,)
                )
              elif fn0[0] == 65788:  # {U_Python.Prim.{next6}1}
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_next6_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65789:  # {U_Python.Prim.{next7}1}
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_next7_125_,
                  (arg0,)
                )
              elif fn0[0] == 65790:  # {U_Python.Prim.{next8}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_next8_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65791:  # {U_Python.Prim.{next9}1}
                aux1 = TailCall(
                  _idris_Python_46_Prim_46__123_next9_125_,
                  (arg0,)
                )
          else:
            if fn0[0] < 65795:
              if fn0[0] == 65792:  # {U_Python.importModule1}
                P_c0, P_c1, = fn0[1:]
                aux1 = TailCall(_idris_Python_46_importModule, (P_c0, P_c1, arg0))
              elif fn0[0] == 65793:  # {U_Python.{marshalPIO0}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris_Python_46__123_marshalPIO0_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65794:  # {U_io_bind1}
                P_c0, P_c1, P_c2, P_c3, P_c4, = fn0[1:]
                aux1 = TailCall(_idris_io_95_bind, (P_c0, P_c1, P_c2, P_c3, P_c4, arg0))
            else:
              if fn0[0] == 65795:  # {U_io_return1}
                P_c0, P_c1, P_c2, = fn0[1:]
                aux1 = TailCall(_idris_io_95_return, (P_c0, P_c1, P_c2, arg0))
              elif fn0[0] == 65796:  # {U_prim__zextInt_BigInt1}
                aux1 = TailCall(
                  _idris_prim_95__95_zextInt_95_BigInt,
                  (arg0,)
                )
              elif fn0[0] == 65797:  # {U_prim_lenString1}
                aux1 = TailCall(_idris_prim_95_lenString, (arg0,))
      else:
        if fn0[0] < 65809:
          if fn0[0] < 65803:
            if fn0[0] < 65800:
              if fn0[0] == 65798:  # {U_{Python.Lib.Threading.forkPIO, worker_lam0}1}
                aux1 = TailCall(
                  _idris__123_Python_46_Lib_46_Threading_46_forkPIO_44__32_worker_95_lam0_125_,
                  (arg0,)
                )
              elif fn0[0] == 65799:  # {U_{Python.Lib.Threading.forkPIO, worker_lam1}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris__123_Python_46_Lib_46_Threading_46_forkPIO_44__32_worker_95_lam1_125_,
                  (P_c0, arg0)
                )
            else:
              if fn0[0] == 65800:  # {U_{Python.Prim.iterate, iter_lam0}1}
                aux1 = TailCall(
                  _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam0_125_,
                  (arg0,)
                )
              elif fn0[0] == 65801:  # {U_{Python.Prim.iterate, iter_lam10}1}
                aux1 = TailCall(
                  _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam10_125_,
                  (arg0,)
                )
              elif fn0[0] == 65802:  # {U_{Python.Prim.iterate, iter_lam11}1}
                aux1 = TailCall(
                  _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam11_125_,
                  (arg0,)
                )
          else:
            if fn0[0] < 65806:
              if fn0[0] == 65803:  # {U_{Python.Prim.iterate, iter_lam12}1}
                aux1 = TailCall(
                  _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam12_125_,
                  (arg0,)
                )
              elif fn0[0] == 65804:  # {U_{Python.Prim.iterate, iter_lam13}1}
                P_c0, P_c1, P_c2, = fn0[1:]
                aux1 = TailCall(
                  _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam13_125_,
                  (P_c0, P_c1, P_c2, arg0)
                )
              elif fn0[0] == 65805:  # {U_{Python.Prim.iterate, iter_lam1}1}
                aux1 = TailCall(
                  _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam1_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65806:  # {U_{Python.Prim.iterate, iter_lam2}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam2_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65807:  # {U_{Python.Prim.iterate, iter_lam3}1}
                aux1 = TailCall(
                  _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam3_125_,
                  (arg0,)
                )
              elif fn0[0] == 65808:  # {U_{Python.Prim.iterate, iter_lam4}1}
                aux1 = TailCall(
                  _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam4_125_,
                  (arg0,)
                )
        else:
          if fn0[0] < 65815:
            if fn0[0] < 65812:
              if fn0[0] == 65809:  # {U_{Python.Prim.iterate, iter_lam5}1}
                aux1 = TailCall(
                  _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam5_125_,
                  (arg0,)
                )
              elif fn0[0] == 65810:  # {U_{Python.Prim.iterate, iter_lam6}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = TailCall(
                  _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam6_125_,
                  (P_c0, P_c1, arg0)
                )
              elif fn0[0] == 65811:  # {U_{Python.Prim.iterate, iter_lam7}1}
                aux1 = TailCall(
                  _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam7_125_,
                  (arg0,)
                )
            else:
              if fn0[0] == 65812:  # {U_{Python.Prim.iterate, iter_lam8}1}
                aux1 = TailCall(
                  _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam8_125_,
                  (arg0,)
                )
              elif fn0[0] == 65813:  # {U_{Python.Prim.iterate, iter_lam9}1}
                P_c0, = fn0[1:]
                aux1 = TailCall(
                  _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam9_125_,
                  (P_c0, arg0)
                )
              elif fn0[0] == 65814:  # {U_{io_bind1}1}
                P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, = fn0[1:]
                aux1 = TailCall(io_bind1, (P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, arg0))
          else:
            if fn0[0] < 65818:
              if fn0[0] == 65815:  # {U_{unsafePerformIO0}1}
                aux1 = TailCall(unsafePerformIO0, (arg0,))
              elif fn0[0] == 65816:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable2}
                P_c0, P_c1, P_c2, = fn0[1:]
                aux1 = (65715, P_c0, P_c1, P_c2, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable1}
              elif fn0[0] == 65817:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable3}
                P_c0, P_c1, = fn0[1:]
                aux1 = (65816, P_c0, P_c1, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable2}
            else:
              if fn0[0] == 65818:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable4}
                P_c0, = fn0[1:]
                aux1 = (65817, P_c0, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable3}
              elif fn0[0] == 65819:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable5}
                aux1 = (65818, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable4}
  return aux1

# {EVAL0}
def EVAL0(arg0):
  return arg0

@tailcaller  # Prelude.Functor.{Prelude.IO' ffi instance of Prelude.Functor.Functor, method map_lam0}
def _idris_Prelude_46_Functor_46__123_Prelude_46_IO_39__32_ffi_32_instance_32_of_32_Prelude_46_Functor_46_Functor_44__32_method_32_map_95_lam0_125_(
  e3, in0
):
  return (65795, None, None, APPLY0(e3, in0))  # {U_io_return1}

# {Python.Lib.Threading.forkPIO, worker_lam0}
def _idris__123_Python_46_Lib_46_Threading_46_forkPIO_44__32_worker_95_lam0_125_(
  in1
):
  return (0,)  # Python.Telescope.Return

@tailcaller  # {Python.Prim.iterate, iter_lam0}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam0_125_(in2):
  return (65795, None, None, in2)  # {U_io_return1}

# Python.Functions.{call0}
def _idris_Python_46_Functions_46__123_call0_125_(e3, e2, e5, in0):
  return _idris_call(e3, _idris_Python_46_Telescope_46_strip(None, e2, e5))

@tailcaller  # Python.Prim.{case block in Python.Prim.iterate, iter_lam0}
def _idris_Python_46_Prim_46__123_case_32_block_32_in_32_Python_46_Prim_46_iterate_44__32_iter_95_lam0_125_(
  e6, e9, in1
):
  return TailCall(
    _idris_Python_46_Prim_46_iterate_58_iter_58_0,
    (None, None, None, None, None, None, None, e6, in1, e9)
  )

@tailcaller  # Python.Exceptions.{case block in case block in try_lam0}
def _idris_Python_46_Exceptions_46__123_case_32_block_32_in_32_case_32_block_32_in_32_try_95_lam0_125_(
  e2, in0, in1
):
  if e2[0] == 0:  # constructor of Prelude.Monad.Monad
    in2, in3, = e2[1:]
    aux1 = in2
  return TailCall(
    APPLY0,
    (
      _idris_Prelude_46_Applicative_46_pure(None, None, aux1),
      (
        1,  # Python.Exceptions.Except
        {
          "ArithmeticError": (3,),  # Python.Exceptions.ArithmeticError
          "AssertionError": (7,),  # Python.Exceptions.AssertionError
          "AttributeError": (8,),  # Python.Exceptions.AttributeError
          "BufferError": (2,),  # Python.Exceptions.BufferError
          "EOFError": (14,),  # Python.Exceptions.EOFError
          "EnvironmentError": (9,),  # Python.Exceptions.EnvironmentError
          "FloatingPointError": (4,),  # Python.Exceptions.FloatingPointError
          "IOError": (10,),  # Python.Exceptions.IOError
          "ImportError": (15,),  # Python.Exceptions.ImportError
          "IndentationError": (26,),  # Python.Exceptions.IndentationError
          "IndexError": (17,),  # Python.Exceptions.IndexError
          "KeyError": (18,),  # Python.Exceptions.KeyError
          "LookupError": (16,),  # Python.Exceptions.LookupError
          "MemoryError": (19,),  # Python.Exceptions.MemoryError
          "NameError": (20,),  # Python.Exceptions.NameError
          "NotImplementedError": (24,),  # Python.Exceptions.NotImplementedError
          "OSError": (11,),  # Python.Exceptions.OSError
          "OverflowError": (5,),  # Python.Exceptions.OverflowError
          "ReferenceError": (22,),  # Python.Exceptions.ReferenceError
          "RuntimeError": (23,),  # Python.Exceptions.RuntimeError
          "StandardError": (1,),  # Python.Exceptions.StandardError
          "StopIteration": (0,),  # Python.Exceptions.StopIteration
          "SyntaxError": (25,),  # Python.Exceptions.SyntaxError
          "SystemError": (28,),  # Python.Exceptions.SystemError
          "TabError": (27,),  # Python.Exceptions.TabError
          "TypeError": (29,),  # Python.Exceptions.TypeError
          "UnboundLocalError": (21,),  # Python.Exceptions.UnboundLocalError
          "UnicodeDecodeError": (32,),  # Python.Exceptions.UnicodeDecodeError
          "UnicodeEncodeError": (33,),  # Python.Exceptions.UnicodeEncodeError
          "UnicodeError": (31,),  # Python.Exceptions.UnicodeError
          "UnicodeTranslateError": (34,),  # Python.Exceptions.UnicodeTranslateError
          "VMSError": (13,),  # Python.Exceptions.VMSError
          "ValueError": (30,),  # Python.Exceptions.ValueError
          "WindowsError": (12,),  # Python.Exceptions.WindowsError
          "ZeroDivisionError": (6,)  # Python.Exceptions.ZeroDivisionError
        }.get(in1, (35,)),  # Python.Exceptions.Other
        in0
      )
    )
  )

# Main.{case block in main'_lam0}
def _idris_Main_46__123_case_32_block_32_in_32_main_39__95_lam0_125_(in1):
  return (0,)  # Python.Telescope.Return

@tailcaller  # Python.Exceptions.{case block in try_lam0}
def _idris_Python_46_Exceptions_46__123_case_32_block_32_in_32_try_95_lam0_125_(
  e2, in0, in1
):
  if e2[0] == 0:  # constructor of Prelude.Monad.Monad
    in2, in3, = e2[1:]
    aux1 = in2
  return TailCall(
    APPLY0,
    (
      _idris_Prelude_46_Applicative_46_pure(None, None, aux1),
      (
        1,  # Python.Exceptions.Except
        {
          "ArithmeticError": (3,),  # Python.Exceptions.ArithmeticError
          "AssertionError": (7,),  # Python.Exceptions.AssertionError
          "AttributeError": (8,),  # Python.Exceptions.AttributeError
          "BufferError": (2,),  # Python.Exceptions.BufferError
          "EOFError": (14,),  # Python.Exceptions.EOFError
          "EnvironmentError": (9,),  # Python.Exceptions.EnvironmentError
          "FloatingPointError": (4,),  # Python.Exceptions.FloatingPointError
          "IOError": (10,),  # Python.Exceptions.IOError
          "ImportError": (15,),  # Python.Exceptions.ImportError
          "IndentationError": (26,),  # Python.Exceptions.IndentationError
          "IndexError": (17,),  # Python.Exceptions.IndexError
          "KeyError": (18,),  # Python.Exceptions.KeyError
          "LookupError": (16,),  # Python.Exceptions.LookupError
          "MemoryError": (19,),  # Python.Exceptions.MemoryError
          "NameError": (20,),  # Python.Exceptions.NameError
          "NotImplementedError": (24,),  # Python.Exceptions.NotImplementedError
          "OSError": (11,),  # Python.Exceptions.OSError
          "OverflowError": (5,),  # Python.Exceptions.OverflowError
          "ReferenceError": (22,),  # Python.Exceptions.ReferenceError
          "RuntimeError": (23,),  # Python.Exceptions.RuntimeError
          "StandardError": (1,),  # Python.Exceptions.StandardError
          "StopIteration": (0,),  # Python.Exceptions.StopIteration
          "SyntaxError": (25,),  # Python.Exceptions.SyntaxError
          "SystemError": (28,),  # Python.Exceptions.SystemError
          "TabError": (27,),  # Python.Exceptions.TabError
          "TypeError": (29,),  # Python.Exceptions.TypeError
          "UnboundLocalError": (21,),  # Python.Exceptions.UnboundLocalError
          "UnicodeDecodeError": (32,),  # Python.Exceptions.UnicodeDecodeError
          "UnicodeEncodeError": (33,),  # Python.Exceptions.UnicodeEncodeError
          "UnicodeError": (31,),  # Python.Exceptions.UnicodeError
          "UnicodeTranslateError": (34,),  # Python.Exceptions.UnicodeTranslateError
          "VMSError": (13,),  # Python.Exceptions.VMSError
          "ValueError": (30,),  # Python.Exceptions.ValueError
          "WindowsError": (12,),  # Python.Exceptions.WindowsError
          "ZeroDivisionError": (6,)  # Python.Exceptions.ZeroDivisionError
        }.get(in1, (35,)),  # Python.Exceptions.Other
        in0
      )
    )
  )

@tailcaller  # Python.Exceptions.{catch0}
def _idris_Python_46_Exceptions_46__123_catch0_125_(in2):
  return (65795, None, None, in2)  # {U_io_return1}

@tailcaller  # Python.Prim.{collect0}
def _idris_Python_46_Prim_46__123_collect0_125_(in0, in1):
  return (65795, None, None, in0.cons(in1))  # {U_io_return1}

# Python.Prim.{foreach0}
def _idris_Python_46_Prim_46__123_foreach0_125_(e2, e3, e4, in1):
  return _idris_foreach(e2, e3, e4)

# Python.Lib.Threading.{forkPIO0}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO0_125_(in1):
  return (0,)  # Python.Telescope.Return

@tailcaller  # {io_bind0}
def io_bind0(e0, e1, e2, e3, e4, _idris_w, in0):
  return TailCall(APPLY0, (e4, in0))

@tailcaller  # Python.Prim.{iterate0}
def _idris_Python_46_Prim_46__123_iterate0_125_(e3, e4, in0):
  return TailCall(
    _idris_Python_46_Prim_46_iterate_58_iter_58_0,
    (None, None, None, None, None, None, None, in0, e3, e4)
  )

# Prelude.Strings.{length0}
def _idris_Prelude_46_Strings_46__123_length0_125_(in0):
  return in0

@tailcaller  # Main.{main0}
def _idris_Main_46__123_main0_125_(in0):
  return (65757, _idris_Main_46_main_39_())  # {U_Python.Export.ifMain1}

# Main.{main'0}
def _idris_Main_46__123_main_39_0_125_(in2):
  return (0,)  # Python.Telescope.Return

# Python.{marshalPIO0}
def _idris_Python_46__123_marshalPIO0_125_(e1, in0):
  return _idris_marshal_PIO(e1)

@tailcaller  # Python.Prim.{next0}
def _idris_Python_46_Prim_46__123_next0_125_(in2):
  return (65795, None, None, in2)  # {U_io_return1}

# Prelude.{putStr0}
def _idris_Prelude_46__123_putStr0_125_(e1, in0):
  return sys.stdout.write(e1)

# Python.Exceptions.{raise0}
def _idris_Python_46_Exceptions_46__123_raise0_125_(e1, in0):
  return _idris_raise(e1)

@tailcaller  # {runMain0}
def runMain0():
  return TailCall(EVAL0, (APPLY0(_idris_Main_46_main(), None),))

# Python.Exceptions.{showException0}
def _idris_Python_46_Exceptions_46__123_showException0_125_(e0, in0):
  return str(e0)

# Python.Exceptions.{try0}
def _idris_Python_46_Exceptions_46__123_try0_125_(in1):
  return in1

# {unsafePerformIO0}
def unsafePerformIO0(in0):
  return in0

# Python.Lib.Threading.{wait0}
def _idris_Python_46_Lib_46_Threading_46__123_wait0_125_(in0):
  return (0,)  # Python.Telescope.Return

@tailcaller  # {Python.Lib.Threading.forkPIO, worker_lam1}
def _idris__123_Python_46_Lib_46_Threading_46_forkPIO_44__32_worker_95_lam1_125_(
  e2, in0
):
  return TailCall(
    _idris_Python_46_Functions_46__36__58_,
    (
      None,
      (1, (0,), (65798,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_{Python.Lib.Threading.forkPIO, worker_lam0}1}
      _idris_Python_46_Fields_46__47__46_(None, None, e2, "put", None),
      (0, in0, Unit)  # Builtins.MkSigma
    )
  )

@tailcaller  # {Python.Prim.iterate, iter_lam1}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam1_125_(in1):
  return (65800,)  # {U_{Python.Prim.iterate, iter_lam0}1}

@tailcaller  # Main.{case block in main'_lam1}
def _idris_Main_46__123_case_32_block_32_in_32_main_39__95_lam1_125_(e18, in2, in3):
  if e18[0] == 0:  # constructor of Prelude.Monad.Monad
    in4, in5, = e18[1:]
    aux1 = in4
  return TailCall(
    APPLY0,
    (
      _idris_Prelude_46_Applicative_46_pure(None, None, aux1),
      APPLY0(_idris_Prelude_46_Strings_46_length(), in2)
    )
  )

@tailcaller  # Python.Exceptions.{catch1}
def _idris_Python_46_Exceptions_46__123_catch1_125_(in1):
  return (65722,)  # {U_Python.Exceptions.{catch0}1}

@tailcaller  # Python.Prim.{collect1}
def _idris_Python_46_Prim_46__123_collect1_125_(in0):
  return (65774, in0)  # {U_Python.Prim.{collect0}1}

@tailcaller  # Python.Prim.{foreach1}
def _idris_Python_46_Prim_46__123_foreach1_125_(e2, e3, e4, in0):
  return TailCall(
    _idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0,
    (None, None, None, (65763, None), (65776, e2, e3, e4))  # {U_Python.IO.unRaw1}, {U_Python.Prim.{foreach0}1}
  )

# Python.Lib.Threading.{forkPIO1}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO1_125_(in0):
  return (1, (1,), (65764,))  # Python.Telescope.Bind, Python.Telescope.Default, {U_Python.Lib.Threading.{forkPIO0}1}

@tailcaller  # {io_bind1}
def io_bind1(e0, e1, e2, e3, e4, _idris_w, in0):
  return TailCall(APPLY0, (io_bind0(e0, e1, e2, e3, e4, _idris_w, in0), _idris_w))

# Main.{main'1}
def _idris_Main_46__123_main_39_1_125_(in5):
  return (0,)  # Python.Telescope.Return

@tailcaller  # Python.Prim.{next1}
def _idris_Python_46_Prim_46__123_next1_125_(in1):
  return (65779,)  # {U_Python.Prim.{next0}1}

@tailcaller  # Prelude.{putStr1}
def _idris_Prelude_46__123_putStr1_125_(in1):
  return (65795, None, None, Unit)  # {U_io_return1}

# Python.Exceptions.{try1}
def _idris_Python_46_Exceptions_46__123_try1_125_(in2):
  return (0, in2)  # Prelude.Either.Left

@tailcaller  # {unsafePerformIO1}
def unsafePerformIO1(e0, e1, e2):
  return (65815,)  # {U_{unsafePerformIO0}1}

@tailcaller  # {Python.Prim.iterate, iter_lam2}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam2_125_(in5, in6):
  return (65794, None, None, None, in5, in6)  # {U_io_bind1}

@tailcaller  # Main.{case block in main'_lam2}
def _idris_Main_46__123_case_32_block_32_in_32_main_39__95_lam2_125_(e18, e20, in2):
  return TailCall(
    APPLY0,
    (
      APPLY0(
        _idris_Prelude_46_Monad_46__62__62__61_(None, None, None, e18),
        _idris_Prelude_46_putStr(None, "thread " + e20 + " done" + "\n")
      ),
      (65639, e18, in2)  # {U_Main.{case block in main'_lam1}1}
    )
  )

@tailcaller  # Python.Exceptions.{catch2}
def _idris_Python_46_Exceptions_46__123_catch2_125_(in5, in6):
  return (65794, None, None, None, in5, in6)  # {U_io_bind1}

# Python.Lib.Threading.{forkPIO2}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO2_125_(in4):
  return (0,)  # Python.Telescope.Return

@tailcaller  # {io_bind2}
def io_bind2(e0, e1, e2, e3, e4, _idris_w):
  return (65814, e0, e1, e2, e3, e4, _idris_w)  # {U_{io_bind1}1}

# Main.{main'2}
def _idris_Main_46__123_main_39_2_125_(in7):
  return (0,)  # Python.Telescope.Return

@tailcaller  # Python.Prim.{next2}
def _idris_Python_46_Prim_46__123_next2_125_(in5, in6):
  return (65794, None, None, None, in5, in6)  # {U_io_bind1}

# Python.Exceptions.{try2}
def _idris_Python_46_Exceptions_46__123_try2_125_(in3):
  return in3

@tailcaller  # {Python.Prim.iterate, iter_lam3}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam3_125_(in5):
  return (65806, in5)  # {U_{Python.Prim.iterate, iter_lam2}1}

@tailcaller  # Main.{case block in main'_lam3}
def _idris_Main_46__123_case_32_block_32_in_32_main_39__95_lam3_125_(
  e18, e3, e20, in0
):
  return TailCall(
    APPLY0,
    (
      APPLY0(
        _idris_Prelude_46_Monad_46__62__62__61_(None, None, None, e18),
        _idris_Python_46_Fields_46__47__58_(
          None,
          None,
          _idris_Python_46_Functions_46__36__58_(
            None,
            (1, (0,), (65638,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{case block in main'_lam0}1}
            _idris_Python_46_Fields_46__47__46_(None, None, e3, "get", None),
            (0, "http://idris-lang.org", Unit)  # Builtins.MkSigma
          ),
          "text",
          None
        )
      ),
      (65640, e18, e20)  # {U_Main.{case block in main'_lam2}1}
    )
  )

@tailcaller  # Python.Exceptions.{catch3}
def _idris_Python_46_Exceptions_46__123_catch3_125_(in5):
  return (65724, in5)  # {U_Python.Exceptions.{catch2}1}

# Python.Lib.Threading.{forkPIO3}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO3_125_(in3):
  return (1, (0,), (65766,))  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Threading.{forkPIO2}1}

@tailcaller  # Main.{main'3}
def _idris_Main_46__123_main_39_3_125_(in15, in16, in17):
  return TailCall(
    _idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0,
    (None, None, in15, in16, in17)
  )

@tailcaller  # Python.Prim.{next3}
def _idris_Python_46_Prim_46__123_next3_125_(in5):
  return (65784, in5)  # {U_Python.Prim.{next2}1}

# Python.Exceptions.{try3}
def _idris_Python_46_Exceptions_46__123_try3_125_(in4):
  return (1, in4)  # Prelude.Either.Right

@tailcaller  # {Python.Prim.iterate, iter_lam4}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam4_125_(in4):
  return (65807,)  # {U_{Python.Prim.iterate, iter_lam3}1}

@tailcaller  # Python.Exceptions.{catch4}
def _idris_Python_46_Exceptions_46__123_catch4_125_(in4):
  return (65725,)  # {U_Python.Exceptions.{catch3}1}

@tailcaller  # Python.Lib.Threading.{forkPIO4}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO4_125_(in2, in6):
  return (65795, None, None, in2)  # {U_io_return1}

@tailcaller  # Main.{main'4}
def _idris_Main_46__123_main_39_4_125_(in15, in16):
  return (65675, in15, in16)  # {U_Main.{main'3}1}

@tailcaller  # Python.Prim.{next4}
def _idris_Python_46_Prim_46__123_next4_125_(in4):
  return (65785,)  # {U_Python.Prim.{next3}1}

# Python.Exceptions.{try4}
def _idris_Python_46_Exceptions_46__123_try4_125_(e1, in0):
  return _idris_try(
    e1,
    (65713, None, None, None, (65731,), (65742,)),  # {U_Prelude.Basics..1}, {U_Python.Exceptions.{try0}1}, {U_Python.Exceptions.{try1}1}
    (
      65713,  # {U_Prelude.Basics..1}
      None,
      None,
      None,
      (65713, None, None, None, (65748,), (65749,)),  # {U_Prelude.Basics..1}, {U_Python.Exceptions.{try2}1}, {U_Python.Exceptions.{try3}1}
      (65763, None)  # {U_Python.IO.unRaw1}
    )
  )

@tailcaller  # {Python.Prim.iterate, iter_lam5}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam5_125_(in3):
  return (65808,)  # {U_{Python.Prim.iterate, iter_lam4}1}

@tailcaller  # Python.Exceptions.{catch5}
def _idris_Python_46_Exceptions_46__123_catch5_125_(in3):
  return (65726,)  # {U_Python.Exceptions.{catch4}1}

@tailcaller  # Python.Lib.Threading.{forkPIO5}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO5_125_(in2, in5):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Functions_46__36__58_(
      None,
      (0,),  # Python.Telescope.Return
      _idris_Python_46_Fields_46__47__46_(None, None, in5, "start", None),
      Unit
    ),
    (65768, in2)  # {U_Python.Lib.Threading.{forkPIO4}1}
  )

@tailcaller  # Main.{main'5}
def _idris_Main_46__123_main_39_5_125_(in15):
  return (65686, in15)  # {U_Main.{main'4}1}

@tailcaller  # Python.Prim.{next5}
def _idris_Python_46_Prim_46__123_next5_125_(in3):
  return (65786,)  # {U_Python.Prim.{next4}1}

@tailcaller  # Python.Exceptions.{try5}
def _idris_Python_46_Exceptions_46__123_try5_125_(in7):
  return (65795, None, None, in7)  # {U_io_return1}

@tailcaller  # {Python.Prim.iterate, iter_lam6}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam6_125_(e7, e9, in8):
  return TailCall(
    _idris_Python_46_Prim_46_iterate_58_iter_58_0,
    (None, None, None, None, None, None, None, e7, in8, e9)
  )

@tailcaller  # Python.Exceptions.{catch6}
def _idris_Python_46_Exceptions_46__123_catch6_125_(e2, in0):
  if in0[0] == 1:  # Python.Exceptions.Except
    in7, in8, = in0[1:]
    aux1 = TailCall(APPLY0, (APPLY0(e2, in7), in8))
  elif in0[0] == 0:  # Python.Exceptions.OK
    in9, = in0[1:]
    aux2 = (0, (65723,), (65727,))  # constructor of Prelude.Monad.Monad, {U_Python.Exceptions.{catch1}1}, {U_Python.Exceptions.{catch5}1}
    if aux2[0] == 0:  # constructor of Prelude.Monad.Monad
      in10, in11, = aux2[1:]
      aux3 = in10
    aux1 = TailCall(APPLY0, (_idris_Prelude_46_Applicative_46_pure(None, None, aux3), in9))
  return aux1

@tailcaller  # Python.Lib.Threading.{forkPIO6}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO6_125_(e1, in2):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Functions_46__36__58_(
      None,
      (1, (0,), (65767,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Threading.{forkPIO3}1}
      _idris_Python_46_Fields_46__47__58_(
        None,
        None,
        _idris_Python_46_Lib_46_Threading_46_import_95_(),
        "Thread",
        None
      ),
      (
        0,  # Builtins.MkSigma
        None,
        (
          0,  # Builtins.MkSigma
          _idris_Python_46_marshalPIO(
            None,
            _idris_Python_46_Lib_46_Threading_46_forkPIO_58_worker_58_0(None, e1, in2)
          ),
          Unit
        )
      )
    ),
    (65769, in2)  # {U_Python.Lib.Threading.{forkPIO5}1}
  )

@tailcaller  # Main.{main'6}
def _idris_Main_46__123_main_39_6_125_(in14):
  return (65697,)  # {U_Main.{main'5}1}

@tailcaller  # Python.Prim.{next6}
def _idris_Python_46_Prim_46__123_next6_125_(in2):
  return (65795, None, None, in2)  # {U_io_return1}

@tailcaller  # Python.Exceptions.{try6}
def _idris_Python_46_Exceptions_46__123_try6_125_(in6):
  return (65751,)  # {U_Python.Exceptions.{try5}1}

@tailcaller  # {Python.Prim.iterate, iter_lam7}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam7_125_(in2):
  return (65795, None, None, in2)  # {U_io_return1}

@tailcaller  # Main.{main'7}
def _idris_Main_46__123_main_39_7_125_(in13):
  return (65708,)  # {U_Main.{main'6}1}

@tailcaller  # Python.Prim.{next7}
def _idris_Python_46_Prim_46__123_next7_125_(in1):
  return (65788,)  # {U_Python.Prim.{next6}1}

@tailcaller  # Python.Exceptions.{try7}
def _idris_Python_46_Exceptions_46__123_try7_125_(in10, in11):
  return (65794, None, None, None, in10, in11)  # {U_io_bind1}

@tailcaller  # {Python.Prim.iterate, iter_lam8}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam8_125_(in1):
  return (65811,)  # {U_{Python.Prim.iterate, iter_lam7}1}

# Main.{main'8}
def _idris_Main_46__123_main_39_8_125_(in18, in19):
  return in18 + in19

@tailcaller  # Python.Prim.{next8}
def _idris_Python_46_Prim_46__123_next8_125_(in5, in6):
  return (65794, None, None, None, in5, in6)  # {U_io_bind1}

@tailcaller  # Python.Exceptions.{try8}
def _idris_Python_46_Exceptions_46__123_try8_125_(in10):
  return (65753, in10)  # {U_Python.Exceptions.{try7}1}

@tailcaller  # {Python.Prim.iterate, iter_lam9}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam9_125_(in5, in6):
  return (65794, None, None, None, in5, in6)  # {U_io_bind1}

@tailcaller  # Main.{main'9}
def _idris_Main_46__123_main_39_9_125_(in18):
  return (65710, in18)  # {U_Main.{main'8}1}

@tailcaller  # Python.Prim.{next9}
def _idris_Python_46_Prim_46__123_next9_125_(in5):
  return (65790, in5)  # {U_Python.Prim.{next8}1}

@tailcaller  # Python.Exceptions.{try9}
def _idris_Python_46_Exceptions_46__123_try9_125_(in9):
  return (65754,)  # {U_Python.Exceptions.{try8}1}

@tailcaller  # {Python.Prim.iterate, iter_lam10}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam10_125_(in5):
  return (65813, in5)  # {U_{Python.Prim.iterate, iter_lam9}1}

@tailcaller  # Main.{main'10}
def _idris_Main_46__123_main_39_10_125_(in12):
  return TailCall(
    _idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0,
    (
      None,
      None,
      None,
      _idris_Prelude_46_Foldable_46_concat(None, None, (65709,), (0, (65711,), "")),  # {U_Main.{main'7}1}, constructor of Prelude.Algebra.Monoid, {U_Main.{main'9}1}
      in12
    )
  )

@tailcaller  # Python.Prim.{next10}
def _idris_Python_46_Prim_46__123_next10_125_(in4):
  return (65791,)  # {U_Python.Prim.{next9}1}

@tailcaller  # Python.Exceptions.{try10}
def _idris_Python_46_Exceptions_46__123_try10_125_(in8):
  return (65755,)  # {U_Python.Exceptions.{try9}1}

@tailcaller  # {Python.Prim.iterate, iter_lam11}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam11_125_(in4):
  return (65801,)  # {U_{Python.Prim.iterate, iter_lam10}1}

@tailcaller  # Main.{main'11}
def _idris_Main_46__123_main_39_11_125_(in10, in21):
  return (65795, None, None, in10 + 1)  # {U_io_return1}

@tailcaller  # Python.Prim.{next11}
def _idris_Python_46_Prim_46__123_next11_125_(in3):
  return (65780,)  # {U_Python.Prim.{next10}1}

@tailcaller  # Python.Exceptions.{try11}
def _idris_Python_46_Exceptions_46__123_try11_125_(in7):
  return (65795, None, None, in7)  # {U_io_return1}

@tailcaller  # {Python.Prim.iterate, iter_lam12}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam12_125_(in3):
  return (65802,)  # {U_{Python.Prim.iterate, iter_lam11}1}

@tailcaller  # Main.{main'12}
def _idris_Main_46__123_main_39_12_125_(in10, in20):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Prelude_46_putStr(None, str(in10 + 1) + ". " + in20 + "\n"),
    (65644, in10)  # {U_Main.{main'11}1}
  )

@tailcaller  # Python.Prim.{next12}
def _idris_Python_46_Prim_46__123_next12_125_(in0):
  if in0[0] == 1:  # Python.Exceptions.Except
    in7, in8, = in0[1:]
    if in7[0] == 0:  # Python.Exceptions.StopIteration
      aux3 = (0, (65783,), (65787,))  # constructor of Prelude.Monad.Monad, {U_Python.Prim.{next1}1}, {U_Python.Prim.{next5}1}
      if aux3[0] == 0:  # constructor of Prelude.Monad.Monad
        in9, in10, = aux3[1:]
        aux4 = in9
      aux2 = TailCall(APPLY0, (_idris_Prelude_46_Applicative_46_pure(None, None, aux4), None))
    else:
      aux2 = TailCall(
        _idris_Python_46_Exceptions_46_raise,
        (None, in8)
      )
    aux1 = aux2
  elif in0[0] == 0:  # Python.Exceptions.OK
    in11, = in0[1:]
    aux5 = (0, (65789,), (65781,))  # constructor of Prelude.Monad.Monad, {U_Python.Prim.{next7}1}, {U_Python.Prim.{next11}1}
    if aux5[0] == 0:  # constructor of Prelude.Monad.Monad
      in12, in13, = aux5[1:]
      aux6 = in12
    aux1 = TailCall(APPLY0, (_idris_Prelude_46_Applicative_46_pure(None, None, aux6), in11))
  return aux1

@tailcaller  # Python.Exceptions.{try12}
def _idris_Python_46_Exceptions_46__123_try12_125_(in6):
  return (65733,)  # {U_Python.Exceptions.{try11}1}

@tailcaller  # {Python.Prim.iterate, iter_lam13}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam13_125_(e9, e8, e7, in0):
  if in0 is not None:  # Prelude.Maybe.Just
    in7 = in0
    aux1 = TailCall(
      APPLY0,
      (
        APPLY0(
          _idris_Prelude_46_Monad_46__62__62__61_(
            None,
            None,
            None,
            (0, (65805,), (65809,))  # constructor of Prelude.Monad.Monad, {U_{Python.Prim.iterate, iter_lam1}1}, {U_{Python.Prim.iterate, iter_lam5}1}
          ),
          APPLY0(APPLY0(e9, e8), in7)
        ),
        (65810, e7, e9)  # {U_{Python.Prim.iterate, iter_lam6}1}
      )
    )
  elif in0 is None:  # Prelude.Maybe.Nothing
    aux2 = (0, (65812,), (65803,))  # constructor of Prelude.Monad.Monad, {U_{Python.Prim.iterate, iter_lam8}1}, {U_{Python.Prim.iterate, iter_lam12}1}
    if aux2[0] == 0:  # constructor of Prelude.Monad.Monad
      in9, in10, = aux2[1:]
      aux3 = in9
    aux1 = TailCall(APPLY0, (_idris_Prelude_46_Applicative_46_pure(None, None, aux3), e8))
  return aux1

@tailcaller  # Main.{main'13}
def _idris_Main_46__123_main_39_13_125_(in10, in11):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    (
      65794,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Fields_46__62__58_(
        None,
        _idris_Python_46_Fields_46__47__46_(None, None, in11, "strings", None),
        None,
        None
      ),
      (65713, None, None, None, (65643,), (65772, None))  # {U_Prelude.Basics..1}, {U_Main.{main'10}1}, {U_Python.Prim.collect1}
    ),
    (65645, in10)  # {U_Main.{main'12}1}
  )

@tailcaller  # Python.Exceptions.{try13}
def _idris_Python_46_Exceptions_46__123_try13_125_(in10, in11):
  return (65794, None, None, None, in10, in11)  # {U_io_bind1}

@tailcaller  # Main.{main'14}
def _idris_Main_46__123_main_39_14_125_(in10):
  return (65646, in10)  # {U_Main.{main'13}1}

@tailcaller  # Python.Exceptions.{try14}
def _idris_Python_46_Exceptions_46__123_try14_125_(in10):
  return (65735, in10)  # {U_Python.Exceptions.{try13}1}

@tailcaller  # Main.{main'15}
def _idris_Main_46__123_main_39_15_125_(in28):
  return (65795, None, None, in28)  # {U_io_return1}

@tailcaller  # Python.Exceptions.{try15}
def _idris_Python_46_Exceptions_46__123_try15_125_(in9):
  return (65736,)  # {U_Python.Exceptions.{try14}1}

@tailcaller  # Main.{main'16}
def _idris_Main_46__123_main_39_16_125_(in27):
  return (65648,)  # {U_Main.{main'15}1}

@tailcaller  # Python.Exceptions.{try16}
def _idris_Python_46_Exceptions_46__123_try16_125_(in8):
  return (65737,)  # {U_Python.Exceptions.{try15}1}

@tailcaller  # Main.{main'17}
def _idris_Main_46__123_main_39_17_125_(in31, in32):
  return (65794, None, None, None, in31, in32)  # {U_io_bind1}

@tailcaller  # Python.Exceptions.{try17}
def _idris_Python_46_Exceptions_46__123_try17_125_(in12, in13):
  aux1 = (0, (65734,), (65738,))  # constructor of Prelude.Monad.Monad, {U_Python.Exceptions.{try12}1}, {U_Python.Exceptions.{try16}1}
  if aux1[0] == 0:  # constructor of Prelude.Monad.Monad
    in14, in15, = aux1[1:]
    aux2 = in14
  return TailCall(
    APPLY0,
    (
      _idris_Prelude_46_Applicative_46_pure(None, None, aux2),
      (
        1,  # Python.Exceptions.Except
        {
          "ArithmeticError": (3,),  # Python.Exceptions.ArithmeticError
          "AssertionError": (7,),  # Python.Exceptions.AssertionError
          "AttributeError": (8,),  # Python.Exceptions.AttributeError
          "BufferError": (2,),  # Python.Exceptions.BufferError
          "EOFError": (14,),  # Python.Exceptions.EOFError
          "EnvironmentError": (9,),  # Python.Exceptions.EnvironmentError
          "FloatingPointError": (4,),  # Python.Exceptions.FloatingPointError
          "IOError": (10,),  # Python.Exceptions.IOError
          "ImportError": (15,),  # Python.Exceptions.ImportError
          "IndentationError": (26,),  # Python.Exceptions.IndentationError
          "IndexError": (17,),  # Python.Exceptions.IndexError
          "KeyError": (18,),  # Python.Exceptions.KeyError
          "LookupError": (16,),  # Python.Exceptions.LookupError
          "MemoryError": (19,),  # Python.Exceptions.MemoryError
          "NameError": (20,),  # Python.Exceptions.NameError
          "NotImplementedError": (24,),  # Python.Exceptions.NotImplementedError
          "OSError": (11,),  # Python.Exceptions.OSError
          "OverflowError": (5,),  # Python.Exceptions.OverflowError
          "ReferenceError": (22,),  # Python.Exceptions.ReferenceError
          "RuntimeError": (23,),  # Python.Exceptions.RuntimeError
          "StandardError": (1,),  # Python.Exceptions.StandardError
          "StopIteration": (0,),  # Python.Exceptions.StopIteration
          "SyntaxError": (25,),  # Python.Exceptions.SyntaxError
          "SystemError": (28,),  # Python.Exceptions.SystemError
          "TabError": (27,),  # Python.Exceptions.TabError
          "TypeError": (29,),  # Python.Exceptions.TypeError
          "UnboundLocalError": (21,),  # Python.Exceptions.UnboundLocalError
          "UnicodeDecodeError": (32,),  # Python.Exceptions.UnicodeDecodeError
          "UnicodeEncodeError": (33,),  # Python.Exceptions.UnicodeEncodeError
          "UnicodeError": (31,),  # Python.Exceptions.UnicodeError
          "UnicodeTranslateError": (34,),  # Python.Exceptions.UnicodeTranslateError
          "VMSError": (13,),  # Python.Exceptions.VMSError
          "ValueError": (30,),  # Python.Exceptions.ValueError
          "WindowsError": (12,),  # Python.Exceptions.WindowsError
          "ZeroDivisionError": (6,)  # Python.Exceptions.ZeroDivisionError
        }.get(in13, (35,)),  # Python.Exceptions.Other
        in12
      )
    )
  )

@tailcaller  # Main.{main'18}
def _idris_Main_46__123_main_39_18_125_(in31):
  return (65650, in31)  # {U_Main.{main'17}1}

@tailcaller  # Python.Exceptions.{try18}
def _idris_Python_46_Exceptions_46__123_try18_125_(in7):
  return (65795, None, None, in7)  # {U_io_return1}

@tailcaller  # Main.{main'19}
def _idris_Main_46__123_main_39_19_125_(in30):
  return (65651,)  # {U_Main.{main'18}1}

@tailcaller  # Python.Exceptions.{try19}
def _idris_Python_46_Exceptions_46__123_try19_125_(in6):
  return (65740,)  # {U_Python.Exceptions.{try18}1}

@tailcaller  # Main.{main'20}
def _idris_Main_46__123_main_39_20_125_(in29):
  return (65652,)  # {U_Main.{main'19}1}

@tailcaller  # Python.Exceptions.{try20}
def _idris_Python_46_Exceptions_46__123_try20_125_(in10, in11):
  return (65794, None, None, None, in10, in11)  # {U_io_bind1}

@tailcaller  # Main.{main'21}
def _idris_Main_46__123_main_39_21_125_(in28):
  return (65795, None, None, in28)  # {U_io_return1}

@tailcaller  # Python.Exceptions.{try21}
def _idris_Python_46_Exceptions_46__123_try21_125_(in10):
  return (65743, in10)  # {U_Python.Exceptions.{try20}1}

@tailcaller  # Main.{main'22}
def _idris_Main_46__123_main_39_22_125_(in27):
  return (65655,)  # {U_Main.{main'21}1}

@tailcaller  # Python.Exceptions.{try22}
def _idris_Python_46_Exceptions_46__123_try22_125_(in9):
  return (65744,)  # {U_Python.Exceptions.{try21}1}

@tailcaller  # Main.{main'23}
def _idris_Main_46__123_main_39_23_125_(in31, in32):
  return (65794, None, None, None, in31, in32)  # {U_io_bind1}

@tailcaller  # Python.Exceptions.{try23}
def _idris_Python_46_Exceptions_46__123_try23_125_(in8):
  return (65745,)  # {U_Python.Exceptions.{try22}1}

@tailcaller  # Main.{main'24}
def _idris_Main_46__123_main_39_24_125_(in31):
  return (65657, in31)  # {U_Main.{main'23}1}

@tailcaller  # Python.Exceptions.{try24}
def _idris_Python_46_Exceptions_46__123_try24_125_(in5):
  if in5[0] == 0:  # Prelude.Either.Left
    in12, = in5[1:]
    aux1 = TailCall(
      APPLY0,
      (
        APPLY0(
          _idris_Prelude_46_Monad_46__62__62__61_(
            None,
            None,
            None,
            (0, (65752,), (65732,))  # constructor of Prelude.Monad.Monad, {U_Python.Exceptions.{try6}1}, {U_Python.Exceptions.{try10}1}
          ),
          _idris_Python_46_Fields_46__47__58_(
            None,
            None,
            _idris_Python_46_Fields_46__47__46_(None, None, in12, "__class__", None),
            "__name__",
            None
          )
        ),
        (65739, in12)  # {U_Python.Exceptions.{try17}1}
      )
    )
  elif in5[0] == 1:  # Prelude.Either.Right
    in16, = in5[1:]
    aux2 = (0, (65741,), (65746,))  # constructor of Prelude.Monad.Monad, {U_Python.Exceptions.{try19}1}, {U_Python.Exceptions.{try23}1}
    if aux2[0] == 0:  # constructor of Prelude.Monad.Monad
      in17, in18, = aux2[1:]
      aux3 = in17
    aux1 = TailCall(
      APPLY0,
      (_idris_Prelude_46_Applicative_46_pure(None, None, aux3), (0, in16))  # Python.Exceptions.OK
    )
  return aux1

@tailcaller  # Main.{main'25}
def _idris_Main_46__123_main_39_25_125_(in30):
  return (65658,)  # {U_Main.{main'24}1}

@tailcaller  # Main.{main'26}
def _idris_Main_46__123_main_39_26_125_(in29):
  return (65659,)  # {U_Main.{main'25}1}

# Main.{main'27}
def _idris_Main_46__123_main_39_27_125_(in34):
  return (0,)  # Python.Telescope.Return

@tailcaller  # Main.{main'28}
def _idris_Main_46__123_main_39_28_125_(in28):
  return (65795, None, None, in28)  # {U_io_return1}

@tailcaller  # Main.{main'29}
def _idris_Main_46__123_main_39_29_125_(in27):
  return (65662,)  # {U_Main.{main'28}1}

@tailcaller  # Main.{main'30}
def _idris_Main_46__123_main_39_30_125_(in31, in32):
  return (65794, None, None, None, in31, in32)  # {U_io_bind1}

@tailcaller  # Main.{main'31}
def _idris_Main_46__123_main_39_31_125_(in31):
  return (65665, in31)  # {U_Main.{main'30}1}

@tailcaller  # Main.{main'32}
def _idris_Main_46__123_main_39_32_125_(in30):
  return (65666,)  # {U_Main.{main'31}1}

@tailcaller  # Main.{main'33}
def _idris_Main_46__123_main_39_33_125_(in29):
  return (65667,)  # {U_Main.{main'32}1}

@tailcaller  # Main.{main'34}
def _idris_Main_46__123_main_39_34_125_(in28):
  return (65795, None, None, in28)  # {U_io_return1}

@tailcaller  # Main.{main'35}
def _idris_Main_46__123_main_39_35_125_(in27):
  return (65669,)  # {U_Main.{main'34}1}

@tailcaller  # Main.{main'36}
def _idris_Main_46__123_main_39_36_125_(in31, in32):
  return (65794, None, None, None, in31, in32)  # {U_io_bind1}

@tailcaller  # Main.{main'37}
def _idris_Main_46__123_main_39_37_125_(in31):
  return (65671, in31)  # {U_Main.{main'36}1}

@tailcaller  # Main.{main'38}
def _idris_Main_46__123_main_39_38_125_(in30):
  return (65672,)  # {U_Main.{main'37}1}

@tailcaller  # Main.{main'39}
def _idris_Main_46__123_main_39_39_125_(in29):
  return (65673,)  # {U_Main.{main'38}1}

@tailcaller  # Main.{main'40}
def _idris_Main_46__123_main_39_40_125_(in35, in36):
  aux1 = (0, (65670,), (65674,))  # constructor of Prelude.Monad.Monad, {U_Main.{main'35}1}, {U_Main.{main'39}1}
  if aux1[0] == 0:  # constructor of Prelude.Monad.Monad
    in37, in38, = aux1[1:]
    aux2 = in37
  return TailCall(
    APPLY0,
    (
      _idris_Prelude_46_Applicative_46_pure(None, None, aux2),
      APPLY0(_idris_Prelude_46_Strings_46_length(), in35)
    )
  )

@tailcaller  # Main.{main'41}
def _idris_Main_46__123_main_39_41_125_(in26, in35):
  return TailCall(
    APPLY0,
    (
      APPLY0(
        _idris_Prelude_46_Monad_46__62__62__61_(
          None,
          None,
          None,
          (0, (65663,), (65668,))  # constructor of Prelude.Monad.Monad, {U_Main.{main'29}1}, {U_Main.{main'33}1}
        ),
        _idris_Prelude_46_putStr(None, "thread " + in26 + " done" + "\n")
      ),
      (65676, in35)  # {U_Main.{main'40}1}
    )
  )

@tailcaller  # Main.{main'42}
def _idris_Main_46__123_main_39_42_125_(in1, in26, in33):
  return TailCall(
    APPLY0,
    (
      APPLY0(
        _idris_Prelude_46_Monad_46__62__62__61_(
          None,
          None,
          None,
          (0, (65656,), (65660,))  # constructor of Prelude.Monad.Monad, {U_Main.{main'22}1}, {U_Main.{main'26}1}
        ),
        _idris_Python_46_Fields_46__47__58_(
          None,
          None,
          _idris_Python_46_Functions_46__36__58_(
            None,
            (1, (0,), (65661,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main'27}1}
            _idris_Python_46_Fields_46__47__46_(None, None, in1, "get", None),
            (0, "http://idris-lang.org", Unit)  # Builtins.MkSigma
          ),
          "text",
          None
        )
      ),
      (65677, in26)  # {U_Main.{main'41}1}
    )
  )

@tailcaller  # Main.{main'43}
def _idris_Main_46__123_main_39_43_125_(in1, in26):
  return TailCall(
    APPLY0,
    (
      APPLY0(
        _idris_Prelude_46_Monad_46__62__62__61_(
          None,
          None,
          None,
          (0, (65649,), (65654,))  # constructor of Prelude.Monad.Monad, {U_Main.{main'16}1}, {U_Main.{main'20}1}
        ),
        _idris_Prelude_46_putStr(None, "thread " + in26 + " starting" + "\n")
      ),
      (65678, in1, in26)  # {U_Main.{main'42}1}
    )
  )

# Main.{main'44}
def _idris_Main_46__123_main_39_44_125_(in48):
  return (0,)  # Python.Telescope.Return

@tailcaller  # Main.{main'45}
def _idris_Main_46__123_main_39_45_125_(in49):
  return TailCall(
    _idris_Prelude_46_putStr,
    (None, "Something's wrong, your root's homedir is writable!\n")
  )

@tailcaller  # Main.{main'46}
def _idris_Main_46__123_main_39_46_125_(in50, in51):
  if in50[0] == 11:  # Python.Exceptions.OSError
    aux1 = TailCall(
      _idris_Prelude_46_putStr,
      (
        None,
        "  -> (1) everything's fine: " + _idris_Python_46_Exceptions_46_showException(in51) + "\n"
      )
    )
  else:
    aux1 = TailCall(
      _idris_Python_46_Exceptions_46_raise,
      (None, in51)
    )
  return aux1

@tailcaller  # Main.{main'47}
def _idris_Main_46__123_main_39_47_125_(in50):
  return (65682, in50)  # {U_Main.{main'46}1}

# Main.{main'48}
def _idris_Main_46__123_main_39_48_125_(in53):
  return (0,)  # Python.Telescope.Return

@tailcaller  # Main.{main'49}
def _idris_Main_46__123_main_39_49_125_(in54):
  if in54[0] == 1:  # Python.Exceptions.Except
    in55, in56, = in54[1:]
    if in55[0] == 11:  # Python.Exceptions.OSError
      aux2 = TailCall(
        _idris_Prelude_46_putStr,
        (
          None,
          "  -> (2) everything's fine: " + _idris_Python_46_Exceptions_46_showException(in56) + "\n"
        )
      )
    else:
      aux2 = TailCall(
        _idris_Python_46_Exceptions_46_raise,
        (None, in56)
      )
    aux1 = aux2
  elif in54[0] == 0:  # Python.Exceptions.OK
    in57, = in54[1:]
    aux1 = TailCall(
      _idris_Prelude_46_putStr,
      (None, "Your root could probably use some security lessons!\n")
    )
  return aux1

@tailcaller  # Main.{main'50}
def _idris_Main_46__123_main_39_50_125_(in46, in52):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Exceptions_46_try(
      None,
      _idris_Python_46_Functions_46__36__58_(
        None,
        (1, (0,), (65684,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main'48}1}
        _idris_Python_46_Fields_46__47__46_(None, None, in46, "mkdir", None),
        (0, "/root/hello", Unit)  # Builtins.MkSigma
      )
    ),
    (65685,)  # {U_Main.{main'49}1}
  )

@tailcaller  # Main.{main'51}
def _idris_Main_46__123_main_39_51_125_(in46, in47):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Exceptions_46_catch(
      None,
      _idris_Python_46_Exceptions_46_try(
        None,
        (
          65794,  # {U_io_bind1}
          None,
          None,
          None,
          _idris_Python_46_Functions_46__36__58_(
            None,
            (1, (0,), (65680,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main'44}1}
            _idris_Python_46_Fields_46__47__46_(None, None, in46, "mkdir", None),
            (0, "/root/hello", Unit)  # Builtins.MkSigma
          ),
          (65681,)  # {U_Main.{main'45}1}
        )
      ),
      (65683,)  # {U_Main.{main'47}1}
    ),
    (65687, in46)  # {U_Main.{main'50}1}
  )

@tailcaller  # Main.{main'52}
def _idris_Main_46__123_main_39_52_125_(in46):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Prelude_46_putStr(None, "And now, let's fail!\n"),
    (65688, in46)  # {U_Main.{main'51}1}
  )

@tailcaller  # Main.{main'53}
def _idris_Main_46__123_main_39_53_125_(in45):
  return (65794, None, None, None, _idris_Python_46_Lib_46_Os_46_import_95_(), (65689,))  # {U_io_bind1}, {U_Main.{main'52}1}

@tailcaller  # Main.{main'54}
def _idris_Main_46__123_main_39_54_125_(in44):
  return (65794, None, None, None, _idris_Prelude_46_putStr(None, "\n"), (65690,))  # {U_io_bind1}, {U_Main.{main'53}1}

@tailcaller  # Main.{main'55}
def _idris_Main_46__123_main_39_55_125_(in42, in43):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Prelude_46_putStr(
      None,
      "thread B says " + _idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_Nat_58__33_show_58_0(in42) + "\n"
    ),
    (65691,)  # {U_Main.{main'54}1}
  )

@tailcaller  # Main.{main'56}
def _idris_Main_46__123_main_39_56_125_(in41, in42):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Prelude_46_putStr(
      None,
      "thread A says " + _idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_Nat_58__33_show_58_0(in41) + "\n"
    ),
    (65692, in42)  # {U_Main.{main'55}1}
  )

@tailcaller  # Main.{main'57}
def _idris_Main_46__123_main_39_57_125_(in40, in41):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Lib_46_Threading_46_wait(None, in40),
    (65693, in41)  # {U_Main.{main'56}1}
  )

@tailcaller  # Main.{main'58}
def _idris_Main_46__123_main_39_58_125_(in39, in40):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Lib_46_Threading_46_wait(None, in39),
    (65694, in40)  # {U_Main.{main'57}1}
  )

@tailcaller  # Main.{main'59}
def _idris_Main_46__123_main_39_59_125_(in25, in39):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Lib_46_Threading_46_forkPIO(None, APPLY0(in25, "B")),
    (65695, in39)  # {U_Main.{main'58}1}
  )

@tailcaller  # Main.{main'60}
def _idris_Main_46__123_main_39_60_125_(in1, in24):
  in25 = (65679, in1)  # {U_Main.{main'43}1}
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Lib_46_Threading_46_forkPIO(None, APPLY0(in25, "A")),
    (65696, in25)  # {U_Main.{main'59}1}
  )

@tailcaller  # Main.{main'61}
def _idris_Main_46__123_main_39_61_125_(in1, in23):
  return (65794, None, None, None, _idris_Prelude_46_putStr(None, "\n"), (65698, in1))  # {U_io_bind1}, {U_Main.{main'60}1}

@tailcaller  # Main.{main'62}
def _idris_Main_46__123_main_39_62_125_(in1, in22):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Prelude_46_putStr(None, "Total number of features: " + str(in22) + "\n"),
    (65699, in1)  # {U_Main.{main'61}1}
  )

@tailcaller  # Main.{main'63}
def _idris_Main_46__123_main_39_63_125_(in8, in1, in9):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Prim_46_iterate(None, None, in8, 0, (65647,)),  # {U_Main.{main'14}1}
    (65700, in1)  # {U_Main.{main'62}1}
  )

@tailcaller  # Main.{main'64}
def _idris_Main_46__123_main_39_64_125_(in1, in8):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Prelude_46_putStr(
      None,
      "Idris has got the following exciting features:\n"
    ),
    (65701, in8, in1)  # {U_Main.{main'63}1}
  )

@tailcaller  # Main.{main'65}
def _idris_Main_46__123_main_39_65_125_(in1, in6):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Fields_46__62__58_(
      None,
      _idris_Python_46_Functions_46__36__58_(
        None,
        (1, (0,), (65664,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main'2}1}
        _idris_Python_46_Fields_46__47__46_(None, None, in6, "select", None),
        (0, "div.entry-content li", Unit)  # Builtins.MkSigma
      ),
      None,
      None
    ),
    (65702, in1)  # {U_Main.{main'64}1}
  )

@tailcaller  # Main.{main'66}
def _idris_Main_46__123_main_39_66_125_(in3, in1, in4):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Functions_46__36__58_(
      None,
      (1, (0,), (65653,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main'1}1}
      _idris_Python_46_Fields_46__47__46_(None, None, in4, "BeautifulSoup", None),
      (0, in3, Unit)  # Builtins.MkSigma
    ),
    (65703, in1)  # {U_Main.{main'65}1}
  )

@tailcaller  # Main.{main'67}
def _idris_Main_46__123_main_39_67_125_(in1, in3):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Lib_46_BeautifulSoup_46_import_95_(),
    (65704, in3, in1)  # {U_Main.{main'66}1}
  )

@tailcaller  # Main.{main'68}
def _idris_Main_46__123_main_39_68_125_(in1):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Fields_46__47__58_(
      None,
      None,
      _idris_Python_46_Functions_46__36__58_(
        None,
        (1, (0,), (65642,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main'0}1}
        _idris_Python_46_Fields_46__47__46_(None, None, in1, "get", None),
        (0, "http://idris-lang.org", Unit)  # Builtins.MkSigma
      ),
      "text",
      None
    ),
    (65705, in1)  # {U_Main.{main'67}1}
  )

@tailcaller  # Main.{main'69}
def _idris_Main_46__123_main_39_69_125_(in0):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Functions_46__36__58_(
      None,
      (0,),  # Python.Telescope.Return
      _idris_Python_46_Fields_46__47__46_(None, None, in0, "Session", None),
      Unit
    ),
    (65706,)  # {U_Main.{main'68}1}
  )

@tailcaller  # Python.Lib.Threading.forkPIO, worker
def _idris_Python_46_Lib_46_Threading_46_forkPIO_58_worker_58_0(e0, e1, e2):
  return (65794, None, None, None, e1, (65799, e2))  # {U_io_bind1}, {U_{Python.Lib.Threading.forkPIO, worker_lam1}1}

@tailcaller  # Python.Prim.iterate, iter
def _idris_Python_46_Prim_46_iterate_58_iter_58_0(
  e0, e1, e2, e3, e4, e5, e6, e7, e8, e9
):
  return (
    65794,  # {U_io_bind1}
    None,
    None,
    None,
    _idris_Python_46_Prim_46_next(None, e7),
    (65804, e9, e8, e7)  # {U_{Python.Prim.iterate, iter_lam13}1}
  )

@tailcaller  # Prelude.List.reverse, reverse'
def _idris_Prelude_46_List_46_reverse_58_reverse_39__58_0(e0, e1, e2):
  if e2:  # Prelude.List.::
    in0, in1 = e2.head, e2.tail
    aux1 = TailCall(
      _idris_Prelude_46_List_46_reverse_58_reverse_39__58_0,
      (None, e1.cons(in0), in1)
    )
  elif not e2:  # Prelude.List.Nil
    aux1 = e1
  return aux1

# Decidable.Equality.Decidable.Equality.Char instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Char_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  return None

# Decidable.Equality.Decidable.Equality.Int instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Int_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  return None

# Decidable.Equality.Decidable.Equality.Integer instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Integer_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  return None

# Decidable.Equality.Decidable.Equality.String instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_String_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  return None

@tailcaller  # Prelude.Foldable.Prelude.List.List instance of Prelude.Foldable.Foldable, method foldr
def _idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
  e0, e1, e2, e3, e4
):
  if e4:  # Prelude.List.::
    in0, in1 = e4.head, e4.tail
    aux1 = TailCall(
      APPLY0,
      (
        APPLY0(e2, in0),
        APPLY0(
          APPLY0(
            APPLY0(_idris_Prelude_46_Foldable_46_foldr(None, None, None, (65819,)), e2),  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable5}
            e3
          ),
          in1
        )
      )
    )
  elif not e4:  # Prelude.List.Nil
    aux1 = e3
  return aux1

@tailcaller  # Prelude.Functor.Prelude.IO' ffi instance of Prelude.Functor.Functor, method map
def _idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
  e0, e1, e2, e3, e4
):
  return (65794, None, None, None, e4, (65714, e3))  # {U_io_bind1}, {U_Prelude.Functor.{Prelude.IO' ffi instance of Prelude.Functor.Functor, method map_lam0}1}

# Prelude.Prelude.Nat instance of Prelude.Show, method show
def _idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_Nat_58__33_show_58_0(e0):
  return str(e0)

# with block in Python.Telescope.strip
def _idris__95_Python_46_Telescope_46_strip_95_with_95_17(
  e0, e1, e2, e3, e4, e5, e6
):
  if e1 is not None:  # Prelude.Maybe.Just
    in0 = e1
    aux1 = _idris_Python_46_Telescope_46_strip(None, APPLY0(e6, in0), e4).cons(in0)
  elif e1 is None:  # Prelude.Maybe.Nothing
    aux1 = _idris_Python_46_Telescope_46_strip(None, APPLY0(e6, None), e4).cons(None)
  return aux1

@tailcaller  # Prelude.List.List instance of Prelude.Foldable.Foldable
def _idris_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List(
  meth0, meth1, meth2, meth3, meth4
):
  return TailCall(
    _idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0,
    (None, None, meth2, meth3, meth4)
  )

# case block in Void
def _idris_Void_95_case():
  return None

@tailcaller  # Python.Exceptions.case block in catch
def _idris_Python_46_Exceptions_46_catch_95_case(e0, e1, e2, e3, e4, e5):
  if e4[0] == 1:  # Python.Exceptions.Except
    in0, in1, = e4[1:]
    aux1 = TailCall(APPLY0, (APPLY0(e2, in0), in1))
  elif e4[0] == 0:  # Python.Exceptions.OK
    in2, = e4[1:]
    if e3[0] == 0:  # constructor of Prelude.Monad.Monad
      in3, in4, = e3[1:]
      aux2 = in3
    aux1 = TailCall(APPLY0, (_idris_Prelude_46_Applicative_46_pure(None, None, aux2), in2))
  return aux1

# Python.Exceptions.case block in fromString
def _idris_Python_46_Exceptions_46_fromString_95_case(e0, e1):
  return {
    "ArithmeticError": (3,),  # Python.Exceptions.ArithmeticError
    "AssertionError": (7,),  # Python.Exceptions.AssertionError
    "AttributeError": (8,),  # Python.Exceptions.AttributeError
    "BufferError": (2,),  # Python.Exceptions.BufferError
    "EOFError": (14,),  # Python.Exceptions.EOFError
    "EnvironmentError": (9,),  # Python.Exceptions.EnvironmentError
    "FloatingPointError": (4,),  # Python.Exceptions.FloatingPointError
    "IOError": (10,),  # Python.Exceptions.IOError
    "ImportError": (15,),  # Python.Exceptions.ImportError
    "IndentationError": (26,),  # Python.Exceptions.IndentationError
    "IndexError": (17,),  # Python.Exceptions.IndexError
    "KeyError": (18,),  # Python.Exceptions.KeyError
    "LookupError": (16,),  # Python.Exceptions.LookupError
    "MemoryError": (19,),  # Python.Exceptions.MemoryError
    "NameError": (20,),  # Python.Exceptions.NameError
    "NotImplementedError": (24,),  # Python.Exceptions.NotImplementedError
    "OSError": (11,),  # Python.Exceptions.OSError
    "OverflowError": (5,),  # Python.Exceptions.OverflowError
    "ReferenceError": (22,),  # Python.Exceptions.ReferenceError
    "RuntimeError": (23,),  # Python.Exceptions.RuntimeError
    "StandardError": (1,),  # Python.Exceptions.StandardError
    "StopIteration": (0,),  # Python.Exceptions.StopIteration
    "SyntaxError": (25,),  # Python.Exceptions.SyntaxError
    "SystemError": (28,),  # Python.Exceptions.SystemError
    "TabError": (27,),  # Python.Exceptions.TabError
    "TypeError": (29,),  # Python.Exceptions.TypeError
    "UnboundLocalError": (21,),  # Python.Exceptions.UnboundLocalError
    "UnicodeDecodeError": (32,),  # Python.Exceptions.UnicodeDecodeError
    "UnicodeEncodeError": (33,),  # Python.Exceptions.UnicodeEncodeError
    "UnicodeError": (31,),  # Python.Exceptions.UnicodeError
    "UnicodeTranslateError": (34,),  # Python.Exceptions.UnicodeTranslateError
    "VMSError": (13,),  # Python.Exceptions.VMSError
    "ValueError": (30,),  # Python.Exceptions.ValueError
    "WindowsError": (12,),  # Python.Exceptions.WindowsError
    "ZeroDivisionError": (6,)  # Python.Exceptions.ZeroDivisionError
  }.get(e0, (35,))  # Python.Exceptions.Other

@tailcaller  # case block in io_bind
def _idris_io_95_bind_95_case(e0, e1, e2, e3, e4, e5, e6, e7):
  return TailCall(APPLY0, (e7, e5))

@tailcaller  # Main.case block in main'
def _idris_Main_46_main_39__95_case(
  e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14,
  e15, e16, e17, e18, e19, e20, e21
):
  return TailCall(
    APPLY0,
    (
      APPLY0(
        _idris_Prelude_46_Monad_46__62__62__61_(None, None, None, e18),
        _idris_Prelude_46_putStr(None, "thread " + e20 + " starting" + "\n")
      ),
      (65641, e18, e3, e20)  # {U_Main.{case block in main'_lam3}1}
    )
  )

@tailcaller  # Main.case block in main'1
def _idris_Main_46_main_39_1_95_case(
  e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14,
  e15, e16, e17, e18, e19, e20, e21, e22, e23, e24, e25, e26, e27,
  e28, e29, e30, e31, e32, e33, e34, e35, e36, e37, e38, e39, e40,
  e41, e42
):
  if e40[0] == 11:  # Python.Exceptions.OSError
    aux1 = TailCall(
      _idris_Prelude_46_putStr,
      (
        None,
        "  -> (1) everything's fine: " + _idris_Python_46_Exceptions_46_showException(e41) + "\n"
      )
    )
  else:
    aux1 = TailCall(
      _idris_Python_46_Exceptions_46_raise,
      (None, e41)
    )
  return aux1

@tailcaller  # Main.case block in main'2
def _idris_Main_46_main_39_2_95_case(
  e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14,
  e15, e16, e17, e18, e19, e20, e21, e22, e23, e24, e25, e26, e27,
  e28, e29, e30, e31, e32, e33, e34, e35, e36, e37, e38, e39, e40,
  e41, e42, e43
):
  if e42[0] == 1:  # Python.Exceptions.Except
    in0, in1, = e42[1:]
    if in0[0] == 11:  # Python.Exceptions.OSError
      aux2 = TailCall(
        _idris_Prelude_46_putStr,
        (
          None,
          "  -> (2) everything's fine: " + _idris_Python_46_Exceptions_46_showException(in1) + "\n"
        )
      )
    else:
      aux2 = TailCall(
        _idris_Python_46_Exceptions_46_raise,
        (None, in1)
      )
    aux1 = aux2
  elif e42[0] == 0:  # Python.Exceptions.OK
    in2, = e42[1:]
    aux1 = TailCall(
      _idris_Prelude_46_putStr,
      (None, "Your root could probably use some security lessons!\n")
    )
  return aux1

@tailcaller  # Python.Prim.case block in next
def _idris_Python_46_Prim_46_next_95_case(e0, e1, e2, e3, e4):
  if e3[0] == 1:  # Python.Exceptions.Except
    in0, in1, = e3[1:]
    if in0[0] == 0:  # Python.Exceptions.StopIteration
      if e2[0] == 0:  # constructor of Prelude.Monad.Monad
        in2, in3, = e2[1:]
        aux3 = in2
      aux2 = TailCall(APPLY0, (_idris_Prelude_46_Applicative_46_pure(None, None, aux3), None))
    else:
      aux2 = TailCall(
        _idris_Python_46_Exceptions_46_raise,
        (None, in1)
      )
    aux1 = aux2
  elif e3[0] == 0:  # Python.Exceptions.OK
    in4, = e3[1:]
    if e2[0] == 0:  # constructor of Prelude.Monad.Monad
      in5, in6, = e2[1:]
      aux4 = in5
    aux1 = TailCall(APPLY0, (_idris_Prelude_46_Applicative_46_pure(None, None, aux4), in4))
  return aux1

@tailcaller  # Python.Exceptions.case block in try
def _idris_Python_46_Exceptions_46_try_95_case(e0, e1, e2, e3, e4):
  if e3[0] == 0:  # Prelude.Either.Left
    in0, = e3[1:]
    aux1 = TailCall(
      APPLY0,
      (
        APPLY0(
          _idris_Prelude_46_Monad_46__62__62__61_(None, None, None, e2),
          _idris_Python_46_Fields_46__47__58_(
            None,
            None,
            _idris_Python_46_Fields_46__47__46_(None, None, in0, "__class__", None),
            "__name__",
            None
          )
        ),
        (65721, e2, in0)  # {U_Python.Exceptions.{case block in try_lam0}1}
      )
    )
  elif e3[0] == 1:  # Prelude.Either.Right
    in4, = e3[1:]
    if e2[0] == 0:  # constructor of Prelude.Monad.Monad
      in5, in6, = e2[1:]
      aux2 = in5
    aux1 = TailCall(
      APPLY0,
      (_idris_Prelude_46_Applicative_46_pure(None, None, aux2), (0, in4))  # Python.Exceptions.OK
    )
  return aux1

@tailcaller  # Python.Prim.case block in Python.Prim.iterate, iter
def _idris_Python_46_Prim_46_Python_46_Prim_46_iterate_58_iter_58_0_95_case(
  e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12
):
  if e11 is not None:  # Prelude.Maybe.Just
    in0 = e11
    aux1 = TailCall(
      APPLY0,
      (
        APPLY0(
          _idris_Prelude_46_Monad_46__62__62__61_(None, None, None, e10),
          APPLY0(APPLY0(e9, e8), in0)
        ),
        (65773, e6, e9)  # {U_Python.Prim.{case block in Python.Prim.iterate, iter_lam0}1}
      )
    )
  elif e11 is None:  # Prelude.Maybe.Nothing
    if e10[0] == 0:  # constructor of Prelude.Monad.Monad
      in2, in3, = e10[1:]
      aux2 = in2
    aux1 = TailCall(APPLY0, (_idris_Prelude_46_Applicative_46_pure(None, None, aux2), e8))
  return aux1

@tailcaller  # Python.Exceptions.case block in case block in try
def _idris_Python_46_Exceptions_46_try_95_case_95_case(e0, e1, e2, e3, e4, e5):
  if e3[0] == 0:  # Prelude.Either.Left
    in0, = e3[1:]
    aux1 = TailCall(
      APPLY0,
      (
        APPLY0(
          _idris_Prelude_46_Monad_46__62__62__61_(None, None, None, e2),
          _idris_Python_46_Fields_46__47__58_(
            None,
            None,
            _idris_Python_46_Fields_46__47__46_(None, None, in0, "__class__", None),
            "__name__",
            None
          )
        ),
        (65720, e2, in0)  # {U_Python.Exceptions.{case block in case block in try_lam0}1}
      )
    )
  elif e3[0] == 1:  # Prelude.Either.Right
    in4, = e3[1:]
    if e2[0] == 0:  # constructor of Prelude.Monad.Monad
      in5, in6, = e2[1:]
      aux2 = in5
    aux1 = TailCall(
      APPLY0,
      (_idris_Prelude_46_Applicative_46_pure(None, None, aux2), (0, in4))  # Python.Exceptions.OK
    )
  return aux1

# <<Void eliminator>>
def _idris_Void_95_elim():
  return None

runMain0()
