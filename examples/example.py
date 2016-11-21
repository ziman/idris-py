#!/usr/bin/env python

import sys
import importlib
import math

Unit = object()
World = object()

class IdrisError(Exception):
  pass

def _idris_error(msg):
  raise IdrisError(msg)

def _idris_pymodule(name):
  return importlib.import_module(name)

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

def _idris_get_global(name):
  return globals()[name]

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

# Python.Functions.$.
def _idris_Python_46_Functions_46__36__46_(e0, e1, e2, e3, e4, e5):
  while True:
    return _idris_PE_95__60__36__62__95_cc6adb39(
      None,
      None,
      (65721, None),  # {U_Python.IO.unRaw1}
      (65719, e3, e2, e5)  # {U_Python.Functions.{$.0}1}
    )

# Python.Functions.$:
def _idris_Python_46_Functions_46__36__58_(e0, e1, e2, e3, e4, e5):
  while True:
    return (65739, None, None, None, e3, (65720, e2, e5))  # {U_io_bind1}, {U_Python.Functions.{$:0}1}

# Prelude.Bool.&&
def _idris_Prelude_46_Bool_46__38__38_(e0, e1):
  while True:
    if not e0:  # Prelude.Bool.False
      return False
    else:  # Prelude.Bool.True
      return EVAL0(e1)
    return _idris_error("unreachable due to case in tail position")

# Prelude.Basics..
def _idris_Prelude_46_Basics_46__46_(e0, e1, e2, e3, e4, _idris_x):
  while True:
    return APPLY0(e3, APPLY0(e4, _idris_x))

# Python.Fields./.
def _idris_Python_46_Fields_46__47__46_(e0, e1, e2, e3, e4):
  while True:
    return _idris_unsafePerformIO(None, None, (65717, e2, e3))  # {U_Python.Fields.{/.0}1}

# Python.Fields./:
def _idris_Python_46_Fields_46__47__58_(e0, e1, e2, e3, e4):
  while True:
    return _idris_PE_95__60__36__62__95_cc6adb39(None, None, (65718, e3), e2)  # {U_Python.Fields.{/:0}1}

# Prelude.Algebra.<+>
def _idris_Prelude_46_Algebra_46__60__43__62_(e0, e1):
  while True:
    return e1

# Prelude.Monad.>>=
def _idris_Prelude_46_Monad_46__62__62__61_(e0, e1, e2, e3):
  while True:
    return APPLY0(APPLY0(e1, e2), e3)

# Force
def _idris_Force(e0, e1, e2):
  while True:
    return EVAL0(e2)

# PE_<$>_cc6adb39
def _idris_PE_95__60__36__62__95_cc6adb39(e0, e1, e2, e3):
  while True:
    return _idris_PE_95_map_95_1b3102a6(None, None, e2, e3)

# PE_map_1b3102a6
def _idris_PE_95_map_95_1b3102a6(e0, e1, e2, e3):
  while True:
    return _idris_Prelude_46_Functor_46_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None, None, None, e2, e3
    )

# assert_unreachable
def _idris_assert_95_unreachable():
  while True:
    return None

# call__IO
def _idris_call_95__95_IO(e0, e1, e2):
  while True:
    return APPLY0(e2, None)

# Python.Exceptions.catch
def _idris_Python_46_Exceptions_46_catch(e0, e1, e2):
  while True:
    return (65739, None, None, None, e1, (65708, e2))  # {U_io_bind1}, {U_Python.Exceptions.{catch0}1}

# Python.Prim.collect
def _idris_Python_46_Prim_46_collect(e0, e1, e2, e3):
  while True:
    return _idris_PE_95__60__36__62__95_cc6adb39(
      None,
      None,
      (65705, None, ConsList()),  # {U_Prelude.List.reverse, reverse'1}
      _idris_Python_46_Prim_46_foreach(
        None,
        None,
        None,
        e2,
        ConsList(),
        (65732,),  # {U_Python.Prim.{collect1}1}
        None
      )
    )

# Prelude.Foldable.concat
def _idris_Prelude_46_Foldable_46_concat(e0, e1, e2, e3):
  while True:
    assert e3[0] == 0  # constructor of Prelude.Algebra.Monoid
    in0, in1 = e3[1:]
    aux1 = in0
    assert e3[0] == 0  # constructor of Prelude.Algebra.Monoid
    in2, in3 = e3[1:]
    aux2 = in3
    return APPLY0(
      APPLY0(_idris_Prelude_46_Foldable_46_foldr(None, e2, None, None), aux1),
      aux2
    )

# Prelude.Foldable.foldr
def _idris_Prelude_46_Foldable_46_foldr(e0, e1, e2, e3):
  while True:
    return APPLY0(APPLY0(e1, e2), e3)

# Python.Prim.foreach
def _idris_Python_46_Prim_46_foreach(e0, e1, e2, e3, e4, e5, e6):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Functions_46__36__46_(
        None,
        None,
        (0,),  # Python.Telescope.Return
        _idris_Python_46_Fields_46__47__46_(None, None, e3, u'__iter__', None),
        None,
        Unit
      ),
      (65734, e3, e4, e5)  # {U_Python.Prim.{foreach1}1}
    )

# Python.Lib.Threading.forkPIO
def _idris_Python_46_Lib_46_Threading_46_forkPIO(e0, e1):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Functions_46__36__58_(
        None,
        None,
        (1, (1,), (65723,)),  # Python.Telescope.Bind, Python.Telescope.Forall, {U_Python.Lib.Threading.{forkPIO1}1}
        _idris_Python_46_Fields_46__47__58_(
          None,
          None,
          (65737, None, u'Queue'),  # {U_Python.importModule1}
          u'Queue',
          None
        ),
        None,
        (0, (0,), (0, 1, Unit))  # Builtins.MkDPair, Data.Erased.Erase, Builtins.MkDPair
      ),
      (65728, e1)  # {U_Python.Lib.Threading.{forkPIO6}1}
    )

# Prelude.Maybe.fromMaybe
def _idris_Prelude_46_Maybe_46_fromMaybe(e0, e1, e2):
  while True:
    if e2 is not None:  # Prelude.Maybe.Just
      in0 = e2
      return in0
    else:  # Prelude.Maybe.Nothing
      return EVAL0(e1)
    return _idris_error("unreachable due to case in tail position")

# Python.Exceptions.fromString
def _idris_Python_46_Exceptions_46_fromString(e0):
  while True:
    return {
      u'ArithmeticError': (3,),  # Python.Exceptions.ArithmeticError
      u'AssertionError': (7,),  # Python.Exceptions.AssertionError
      u'AttributeError': (8,),  # Python.Exceptions.AttributeError
      u'BufferError': (2,),  # Python.Exceptions.BufferError
      u'EOFError': (14,),  # Python.Exceptions.EOFError
      u'EnvironmentError': (9,),  # Python.Exceptions.EnvironmentError
      u'FloatingPointError': (4,),  # Python.Exceptions.FloatingPointError
      u'IOError': (10,),  # Python.Exceptions.IOError
      u'ImportError': (15,),  # Python.Exceptions.ImportError
      u'IndentationError': (26,),  # Python.Exceptions.IndentationError
      u'IndexError': (17,),  # Python.Exceptions.IndexError
      u'KeyError': (18,),  # Python.Exceptions.KeyError
      u'LookupError': (16,),  # Python.Exceptions.LookupError
      u'MemoryError': (19,),  # Python.Exceptions.MemoryError
      u'NameError': (20,),  # Python.Exceptions.NameError
      u'NotImplementedError': (24,),  # Python.Exceptions.NotImplementedError
      u'OSError': (11,),  # Python.Exceptions.OSError
      u'OverflowError': (5,),  # Python.Exceptions.OverflowError
      u'ReferenceError': (22,),  # Python.Exceptions.ReferenceError
      u'RuntimeError': (23,),  # Python.Exceptions.RuntimeError
      u'StandardError': (1,),  # Python.Exceptions.StandardError
      u'StopIteration': (0,),  # Python.Exceptions.StopIteration
      u'SyntaxError': (25,),  # Python.Exceptions.SyntaxError
      u'SystemError': (28,),  # Python.Exceptions.SystemError
      u'TabError': (27,),  # Python.Exceptions.TabError
      u'TypeError': (29,),  # Python.Exceptions.TypeError
      u'UnboundLocalError': (21,),  # Python.Exceptions.UnboundLocalError
      u'UnicodeDecodeError': (32,),  # Python.Exceptions.UnicodeDecodeError
      u'UnicodeEncodeError': (33,),  # Python.Exceptions.UnicodeEncodeError
      u'UnicodeError': (31,),  # Python.Exceptions.UnicodeError
      u'UnicodeTranslateError': (34,),  # Python.Exceptions.UnicodeTranslateError
      u'VMSError': (13,),  # Python.Exceptions.VMSError
      u'ValueError': (30,),  # Python.Exceptions.ValueError
      u'WindowsError': (12,),  # Python.Exceptions.WindowsError
      u'ZeroDivisionError': (6,)  # Python.Exceptions.ZeroDivisionError
    }.get(e0, (35,))  # Python.Exceptions.Other

# Prelude.Bool.ifThenElse
def _idris_Prelude_46_Bool_46_ifThenElse(e0, e1, e2, e3):
  while True:
    if not e1:  # Prelude.Bool.False
      return EVAL0(e3)
    else:  # Prelude.Bool.True
      return EVAL0(e2)
    return _idris_error("unreachable due to case in tail position")

# Python.importModule
def _idris_Python_46_importModule(e0, e1, _idris_w):
  while True:
    return _idris_pymodule(e1)

# Prelude.Interfaces.intToBool
def _idris_Prelude_46_Interfaces_46_intToBool(e0):
  while True:
    if e0 == 0:
      return False
    else:
      return True
    return _idris_error("unreachable due to case in tail position")

# io_bind
def _idris_io_95_bind(e0, e1, e2, e3, e4, _idris_w):
  while True:
    return APPLY0(io_bind2(e0, e1, e2, e3, e4, _idris_w), APPLY0(e3, _idris_w))

# io_pure
def _idris_io_95_pure(e0, e1, e2, _idris_w):
  while True:
    return e2

# Python.Prim.iterate
def _idris_Python_46_Prim_46_iterate(e0, e1, e2, e3, e4, e5, e6):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Functions_46__36__46_(
        None,
        None,
        (0,),  # Python.Telescope.Return
        _idris_Python_46_Fields_46__47__46_(None, None, e3, u'__iter__', None),
        None,
        Unit
      ),
      (65735, e4, e5)  # {U_Python.Prim.{iterate0}1}
    )

# Prelude.Strings.length
def _idris_Prelude_46_Strings_46_length():
  while True:
    return (
      65702,  # {U_Prelude.Basics..1}
      None,
      None,
      None,
      (65702, None, None, None, (65707,), (65743,)),  # {U_Prelude.Basics..1}, {U_Prelude.Strings.{length0}1}, {U_prim__zextInt_BigInt1}
      (65744,)  # {U_prim_lenString1}
    )

# Main.main
def _idris_Main_46_main():
  while True:
    return (65739, None, None, None, (65737, None, u'requests'), (65695,))  # {U_io_bind1}, {U_Python.importModule1}, {U_Main.{main45}1}

# Python.marshalPIO
def _idris_Python_46_marshalPIO(e0, e1):
  while True:
    return _idris_unsafePerformIO(None, None, (65738, e1))  # {U_Python.{marshalPIO0}1}

# mkForeignPrim
def _idris_mkForeignPrim():
  while True:
    return None

# Prelude.Algebra.neutral
def _idris_Prelude_46_Algebra_46_neutral(e0, e1):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Algebra.Monoid
    in0, in1 = e1[1:]
    return in1
    return _idris_error("unreachable due to case in tail position")

# Python.Prim.next
def _idris_Python_46_Prim_46_next(e0, e1):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Exceptions_46_try(
        None,
        _idris_Python_46_Functions_46__36__46_(
          None,
          None,
          (0,),  # Python.Telescope.Return
          _idris_Python_46_Fields_46__47__46_(None, None, e1, u'next', None),
          None,
          Unit
        )
      ),
      (65736,)  # {U_Python.Prim.{next0}1}
    )

# Prelude.Bool.not
def _idris_Prelude_46_Bool_46_not(e0):
  while True:
    if not e0:  # Prelude.Bool.False
      return True
    else:  # Prelude.Bool.True
      return False
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.precCon
def _idris_Prelude_46_Show_46_precCon(e0):
  while True:
    if e0[0] == 6:  # Prelude.Show.App
      return 6
    elif e0[0] == 3:  # Prelude.Show.Backtick
      return 3
    elif e0[0] == 2:  # Prelude.Show.Dollar
      return 2
    elif e0[0] == 1:  # Prelude.Show.Eq
      return 1
    elif e0[0] == 0:  # Prelude.Show.Open
      return 0
    elif e0[0] == 5:  # Prelude.Show.PrefixMinus
      return 5
    else:  # Prelude.Show.User
      in0 = e0[1]
      return 4
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.primNumShow
def _idris_Prelude_46_Show_46_primNumShow(e0, e1, e2, e3):
  while True:
    in0 = APPLY0(e1, e3)
    aux2 = _idris_Prelude_46_Interfaces_46_Prelude_46_Show_46__64_Prelude_46_Interfaces_46_Ord_36_Prec_58__33__62__61__58_0(
      e2, (5,)  # Prelude.Show.PrefixMinus
    )
    if not aux2:  # Prelude.Bool.False
      aux3 = False
    else:  # Prelude.Bool.True
      aux3 = _idris_Prelude_46_Show_46__123_primNumShow2_125_(in0, e0, e1, e2, e3)
    aux1 = aux3
    if not aux1:  # Prelude.Bool.False
      return in0
    else:  # Prelude.Bool.True
      return (u'(' + (in0 + u')'))
    return _idris_error("unreachable due to case in tail position")

# prim__addInt
def _idris_prim_95__95_addInt(op0, op1):
  while True:
    return (op0 + op1)

# prim__asPtr
def _idris_prim_95__95_asPtr(op0):
  while True:
    return _idris_error("unimplemented external: prim__asPtr")

# prim__concat
def _idris_prim_95__95_concat(op0, op1):
  while True:
    return (op0 + op1)

# prim__eqBigInt
def _idris_prim_95__95_eqBigInt(op0, op1):
  while True:
    return (op0 == op1)

# prim__eqChar
def _idris_prim_95__95_eqChar(op0, op1):
  while True:
    return (op0 == op1)

# prim__eqManagedPtr
def _idris_prim_95__95_eqManagedPtr(op0, op1):
  while True:
    return _idris_error("unimplemented external: prim__eqManagedPtr")

# prim__eqPtr
def _idris_prim_95__95_eqPtr(op0, op1):
  while True:
    return _idris_error("unimplemented external: prim__eqPtr")

# prim__eqString
def _idris_prim_95__95_eqString(op0, op1):
  while True:
    return (op0 == op1)

# prim__null
def _idris_prim_95__95_null():
  while True:
    return None

# prim__peek16
def _idris_prim_95__95_peek16(op0, op1, op2):
  while True:
    return _idris_error("unimplemented external: prim__peek16")

# prim__peek32
def _idris_prim_95__95_peek32(op0, op1, op2):
  while True:
    return _idris_error("unimplemented external: prim__peek32")

# prim__peek64
def _idris_prim_95__95_peek64(op0, op1, op2):
  while True:
    return _idris_error("unimplemented external: prim__peek64")

# prim__peek8
def _idris_prim_95__95_peek8(op0, op1, op2):
  while True:
    return _idris_error("unimplemented external: prim__peek8")

# prim__peekDouble
def _idris_prim_95__95_peekDouble(op0, op1, op2):
  while True:
    return _idris_error("unimplemented external: prim__peekDouble")

# prim__peekPtr
def _idris_prim_95__95_peekPtr(op0, op1, op2):
  while True:
    return _idris_error("unimplemented external: prim__peekPtr")

# prim__peekSingle
def _idris_prim_95__95_peekSingle(op0, op1, op2):
  while True:
    return _idris_error("unimplemented external: prim__peekSingle")

# prim__poke16
def _idris_prim_95__95_poke16(op0, op1, op2, op3):
  while True:
    return _idris_error("unimplemented external: prim__poke16")

# prim__poke32
def _idris_prim_95__95_poke32(op0, op1, op2, op3):
  while True:
    return _idris_error("unimplemented external: prim__poke32")

# prim__poke64
def _idris_prim_95__95_poke64(op0, op1, op2, op3):
  while True:
    return _idris_error("unimplemented external: prim__poke64")

# prim__poke8
def _idris_prim_95__95_poke8(op0, op1, op2, op3):
  while True:
    return _idris_error("unimplemented external: prim__poke8")

# prim__pokeDouble
def _idris_prim_95__95_pokeDouble(op0, op1, op2, op3):
  while True:
    return _idris_error("unimplemented external: prim__pokeDouble")

# prim__pokePtr
def _idris_prim_95__95_pokePtr(op0, op1, op2, op3):
  while True:
    return _idris_error("unimplemented external: prim__pokePtr")

# prim__pokeSingle
def _idris_prim_95__95_pokeSingle(op0, op1, op2, op3):
  while True:
    return _idris_error("unimplemented external: prim__pokeSingle")

# prim__ptrOffset
def _idris_prim_95__95_ptrOffset(op0, op1):
  while True:
    return _idris_error("unimplemented external: prim__ptrOffset")

# prim__readFile
def _idris_prim_95__95_readFile(op0, op1):
  while True:
    return _idris_error("unimplemented external: prim__readFile")

# prim__registerPtr
def _idris_prim_95__95_registerPtr(op0, op1):
  while True:
    return _idris_error("unimplemented external: prim__registerPtr")

# prim__sizeofPtr
def _idris_prim_95__95_sizeofPtr():
  while True:
    return _idris_error("unimplemented external: prim__sizeofPtr")

# prim__sltBigInt
def _idris_prim_95__95_sltBigInt(op0, op1):
  while True:
    return (op0 < op1)

# prim__stderr
def _idris_prim_95__95_stderr():
  while True:
    return _idris_error("unimplemented external: prim__stderr")

# prim__stdin
def _idris_prim_95__95_stdin():
  while True:
    return _idris_error("unimplemented external: prim__stdin")

# prim__stdout
def _idris_prim_95__95_stdout():
  while True:
    return _idris_error("unimplemented external: prim__stdout")

# prim__strHead
def _idris_prim_95__95_strHead(op0):
  while True:
    return op0[0]

# prim__toStrBigInt
def _idris_prim_95__95_toStrBigInt(op0):
  while True:
    return str(op0)

# prim__toStrInt
def _idris_prim_95__95_toStrInt(op0):
  while True:
    return str(op0)

# prim__vm
def _idris_prim_95__95_vm(op0):
  while True:
    return _idris_error("unimplemented external: prim__vm")

# prim__writeFile
def _idris_prim_95__95_writeFile(op0, op1, op2):
  while True:
    return _idris_error("unimplemented external: prim__writeFile")

# prim__writeString
def _idris_prim_95__95_writeString(op0, op1):
  while True:
    return sys.stdout.write(op1)

# prim__zextInt_BigInt
def _idris_prim_95__95_zextInt_95_BigInt(op0):
  while True:
    return op0

# prim_io_bind
def _idris_prim_95_io_95_bind(e0, e1, e2, e3):
  while True:
    return APPLY0(e3, e2)

# prim_lenString
def _idris_prim_95_lenString(op0):
  while True:
    return len(op0)

# prim_write
def _idris_prim_95_write(e0, e1, _idris_w):
  while True:
    return sys.stdout.write(e1)

# Prelude.Interactive.putStr'
def _idris_Prelude_46_Interactive_46_putStr_39_(e0, e1):
  while True:
    return (65739, None, None, None, (65745, None, e1), (65704,))  # {U_io_bind1}, {U_prim_write1}, {U_Prelude.Interactive.{putStr'0}1}

# Python.Exceptions.raise
def _idris_Python_46_Exceptions_46_raise(e0, e1):
  while True:
    return _idris_PE_95__60__36__62__95_cc6adb39(None, None, (65721, None), (65709, e1))  # {U_Python.IO.unRaw1}, {U_Python.Exceptions.{raise0}1}

# run__IO
def _idris_run_95__95_IO(e0, e1):
  while True:
    return APPLY0(e1, None)

# Python.Exceptions.showException
def _idris_Python_46_Exceptions_46_showException(e0):
  while True:
    return _idris_unsafePerformIO(None, None, (65710, e0))  # {U_Python.Exceptions.{showException0}1}

# Prelude.Show.showParens
def _idris_Prelude_46_Show_46_showParens(e0, e1):
  while True:
    if not e0:  # Prelude.Bool.False
      return e1
    else:  # Prelude.Bool.True
      return (u'(' + (e1 + u')'))
    return _idris_error("unreachable due to case in tail position")

# Prelude.Strings.strM
def _idris_Prelude_46_Strings_46_strM(e0):
  while True:
    aux3 = (e0 == u'')
    if aux3 == 0:
      aux4 = False
    else:
      aux4 = True
    aux2 = aux4
    if not aux2:  # Prelude.Bool.False
      aux5 = True
    else:  # Prelude.Bool.True
      aux5 = False
    aux1 = _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Bool_58__33_decEq_58_0(
      aux5, True
    )
    if aux1[0] == 1:  # Prelude.Basics.No
      return (0,)  # Prelude.Strings.StrNil
    else:  # Prelude.Basics.Yes
      return (1, e0[0])  # Prelude.Strings.StrCons
    return _idris_error("unreachable due to case in tail position")

# Python.Functions.strip
def _idris_Python_46_Functions_46_strip(e0, e1, e2):
  while True:
    if e1[0] == 1:  # Python.Telescope.Bind
      in0, in1 = e1[1:]
      if in0[0] == 2:  # Python.Telescope.Default
        in2 = in0[1]
        assert e2[0] == 0  # Builtins.MkDPair
        in3, in4 = e2[1:]
        if in3 is not None:  # Prelude.Maybe.Just
          in5 = in3
          aux1 = in5
        else:  # Prelude.Maybe.Nothing
          aux1 = _idris_Python_46_Functions_46__123_strip0_125_(in2)
        return _idris_Python_46_Functions_46_strip(None, APPLY0(in1, aux1), in4).cons(in3)
        return _idris_error("unreachable due to case in tail position")
      elif in0[0] == 1:  # Python.Telescope.Forall
        assert e2[0] == 0  # Builtins.MkDPair
        in6, in7 = e2[1:]
        e0, e1, e2, = None, APPLY0(in1, in6), in7,
        continue
        return _idris_error("unreachable due to tail call")
        return _idris_error("unreachable due to case in tail position")
      else:  # Python.Telescope.Pi
        assert e2[0] == 0  # Builtins.MkDPair
        in8, in9 = e2[1:]
        return _idris_Python_46_Functions_46_strip(None, APPLY0(in1, in8), in9).cons(in8)
        return _idris_error("unreachable due to case in tail position")
      return _idris_error("unreachable due to case in tail position")
    else:  # Python.Telescope.Return
      return ConsList()
    return _idris_error("unreachable due to case in tail position")

# Python.Exceptions.try
def _idris_Python_46_Exceptions_46_try(e0, e1):
  while True:
    return (65739, None, None, None, (65715, e1), (65716,))  # {U_io_bind1}, {U_Python.Exceptions.{try4}1}, {U_Python.Exceptions.{try5}1}

# Python.IO.unRaw
def _idris_Python_46_IO_46_unRaw(e0, e1):
  while True:
    return e1

# unsafePerformIO
def _idris_unsafePerformIO(e0, e1, e2):
  while True:
    return APPLY0(unsafePerformIO1(e0, e1, e2), APPLY0(e2, None))

# unsafePerformPrimIO
def _idris_unsafePerformPrimIO():
  while True:
    return None

# Python.Lib.Threading.wait
def _idris_Python_46_Lib_46_Threading_46_wait(e0, e1):
  while True:
    return _idris_Python_46_Functions_46__36__46_(
      None,
      None,
      (1, (0,), (65729,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Threading.{wait0}1}
      _idris_Python_46_Fields_46__47__46_(None, None, e1, u'get', None),
      None,
      (0, 1, Unit)  # Builtins.MkDPair
    )

# world
def _idris_world(e0):
  while True:
    return e0

# Prelude.Bool.||
def _idris_Prelude_46_Bool_46__124__124_(e0, e1):
  while True:
    if not e0:  # Prelude.Bool.False
      return EVAL0(e1)
    else:  # Prelude.Bool.True
      return True
    return _idris_error("unreachable due to case in tail position")

# Python.Functions.{$.0}
def _idris_Python_46_Functions_46__123__36__46_0_125_(e3, e2, e5, in0):
  while True:
    return _idris_call(e3, _idris_Python_46_Functions_46_strip(None, e2, e5))

# Python.Functions.{$:0}
def _idris_Python_46_Functions_46__123__36__58_0_125_(e2, e5, in0):
  while True:
    return _idris_Python_46_Functions_46__36__46_(None, None, e2, in0, None, e5)

# Python.Fields.{/.0}
def _idris_Python_46_Fields_46__123__47__46_0_125_(e2, e3, in0):
  while True:
    return getattr(e2, e3)

# Python.Fields.{/:0}
def _idris_Python_46_Fields_46__123__47__58_0_125_(e3, in0):
  while True:
    return _idris_Python_46_Fields_46__47__46_(None, None, in0, e3, None)

# {APPLY0}
def APPLY0(fn0, arg0):
  while True:
    if fn0[0] < 65706:
      if fn0[0] < 65681:
        if fn0[0] < 65668:
          if fn0[0] < 65662:
            if fn0[0] < 65659:
              if fn0[0] == 65656:  # {U_Main.{main0}1}
                return _idris_Main_46__123_main0_125_(arg0)
              elif fn0[0] == 65657:  # {U_Main.{main10}1}
                return _idris_Main_46__123_main10_125_(arg0)
              else:  # {U_Main.{main11}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main11_125_(P_c0, arg0)
            else:
              if fn0[0] == 65659:  # {U_Main.{main12}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main12_125_(P_c0, arg0)
              elif fn0[0] == 65660:  # {U_Main.{main13}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main13_125_(P_c0, arg0)
              else:  # {U_Main.{main14}1}
                return _idris_Main_46__123_main14_125_(arg0)
          else:
            if fn0[0] < 65665:
              if fn0[0] == 65662:  # {U_Main.{main15}1}
                return _idris_Main_46__123_main15_125_(arg0)
              elif fn0[0] == 65663:  # {U_Main.{main16}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main16_125_(P_c0, arg0)
              else:  # {U_Main.{main17}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main17_125_(P_c0, arg0)
            else:
              if fn0[0] == 65665:  # {U_Main.{main18}1}
                P_c0, P_c1 = fn0[1:]
                return _idris_Main_46__123_main18_125_(P_c0, P_c1, arg0)
              elif fn0[0] == 65666:  # {U_Main.{main19}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main19_125_(P_c0, arg0)
              else:  # {U_Main.{main1}1}
                return _idris_Main_46__123_main1_125_(arg0)
        else:
          if fn0[0] < 65674:
            if fn0[0] < 65671:
              if fn0[0] == 65668:  # {U_Main.{main20}1}
                return _idris_Main_46__123_main20_125_(arg0)
              elif fn0[0] == 65669:  # {U_Main.{main21}1}
                return _idris_Main_46__123_main21_125_(arg0)
              else:  # {U_Main.{main22}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main22_125_(P_c0, arg0)
            else:
              if fn0[0] == 65671:  # {U_Main.{main23}1}
                return _idris_Main_46__123_main23_125_(arg0)
              elif fn0[0] == 65672:  # {U_Main.{main24}1}
                return _idris_Main_46__123_main24_125_(arg0)
              else:  # {U_Main.{main25}1}
                return _idris_Main_46__123_main25_125_(arg0)
          else:
            if fn0[0] < 65677:
              if fn0[0] == 65674:  # {U_Main.{main26}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main26_125_(P_c0, arg0)
              elif fn0[0] == 65675:  # {U_Main.{main27}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main27_125_(P_c0, arg0)
              else:  # {U_Main.{main28}1}
                return _idris_Main_46__123_main28_125_(arg0)
            else:
              if fn0[0] < 65679:
                if fn0[0] == 65677:  # {U_Main.{main29}1}
                  return _idris_Main_46__123_main29_125_(arg0)
                else:  # {U_Main.{main2}1}
                  return _idris_Main_46__123_main2_125_(arg0)
              else:
                if fn0[0] == 65679:  # {U_Main.{main30}1}
                  return _idris_Main_46__123_main30_125_(arg0)
                else:  # {U_Main.{main31}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main31_125_(P_c0, arg0)
      else:
        if fn0[0] < 65693:
          if fn0[0] < 65687:
            if fn0[0] < 65684:
              if fn0[0] == 65681:  # {U_Main.{main32}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main32_125_(P_c0, arg0)
              elif fn0[0] == 65682:  # {U_Main.{main33}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main33_125_(P_c0, arg0)
              else:  # {U_Main.{main34}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main34_125_(P_c0, arg0)
            else:
              if fn0[0] == 65684:  # {U_Main.{main35}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main35_125_(P_c0, arg0)
              elif fn0[0] == 65685:  # {U_Main.{main36}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main36_125_(P_c0, arg0)
              else:  # {U_Main.{main37}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main37_125_(P_c0, arg0)
          else:
            if fn0[0] < 65690:
              if fn0[0] == 65687:  # {U_Main.{main38}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main38_125_(P_c0, arg0)
              elif fn0[0] == 65688:  # {U_Main.{main39}1}
                P_c0, P_c1 = fn0[1:]
                return _idris_Main_46__123_main39_125_(P_c0, P_c1, arg0)
              else:  # {U_Main.{main3}1}
                return _idris_Main_46__123_main3_125_(arg0)
            else:
              if fn0[0] == 65690:  # {U_Main.{main40}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main40_125_(P_c0, arg0)
              elif fn0[0] == 65691:  # {U_Main.{main41}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main41_125_(P_c0, arg0)
              else:  # {U_Main.{main42}1}
                P_c0, P_c1 = fn0[1:]
                return _idris_Main_46__123_main42_125_(P_c0, P_c1, arg0)
        else:
          if fn0[0] < 65699:
            if fn0[0] < 65696:
              if fn0[0] == 65693:  # {U_Main.{main43}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main43_125_(P_c0, arg0)
              elif fn0[0] == 65694:  # {U_Main.{main44}1}
                return _idris_Main_46__123_main44_125_(arg0)
              else:  # {U_Main.{main45}1}
                return _idris_Main_46__123_main45_125_(arg0)
            else:
              if fn0[0] == 65696:  # {U_Main.{main4}1}
                P_c0, P_c1 = fn0[1:]
                return _idris_Main_46__123_main4_125_(P_c0, P_c1, arg0)
              elif fn0[0] == 65697:  # {U_Main.{main5}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main5_125_(P_c0, arg0)
              else:  # {U_Main.{main6}1}
                return _idris_Main_46__123_main6_125_(arg0)
          else:
            if fn0[0] < 65702:
              if fn0[0] == 65699:  # {U_Main.{main7}1}
                return _idris_Main_46__123_main7_125_(arg0)
              elif fn0[0] == 65700:  # {U_Main.{main8}1}
                return _idris_Main_46__123_main8_125_(arg0)
              else:  # {U_Main.{main9}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main9_125_(P_c0, arg0)
            else:
              if fn0[0] < 65704:
                if fn0[0] == 65702:  # {U_Prelude.Basics..1}
                  P_c0, P_c1, P_c2, P_c3, P_c4 = fn0[1:]
                  return _idris_Prelude_46_Basics_46__46_(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
                else:  # {U_Prelude.Functor.{Prelude.Monad.@Prelude.Functor.Functor$IO' ffi:!map:0_lam0}1}
                  P_c0 = fn0[1]
                  return _idris_Prelude_46_Functor_46__123_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0_95_lam0_125_(
                    P_c0, arg0
                  )
              else:
                if fn0[0] == 65704:  # {U_Prelude.Interactive.{putStr'0}1}
                  return _idris_Prelude_46_Interactive_46__123_putStr_39_0_125_(arg0)
                else:  # {U_Prelude.List.reverse, reverse'1}
                  P_c0, P_c1 = fn0[1:]
                  return _idris_Prelude_46_List_46_reverse_58_reverse_39__58_0(P_c0, P_c1, arg0)
    else:
      if fn0[0] < 65731:
        if fn0[0] < 65718:
          if fn0[0] < 65712:
            if fn0[0] < 65709:
              if fn0[0] == 65706:  # {U_Prelude.Show.{primNumShow0}1}
                return _idris_Prelude_46_Show_46__123_primNumShow0_125_(arg0)
              elif fn0[0] == 65707:  # {U_Prelude.Strings.{length0}1}
                return _idris_Prelude_46_Strings_46__123_length0_125_(arg0)
              else:  # {U_Python.Exceptions.{catch0}1}
                P_c0 = fn0[1]
                return _idris_Python_46_Exceptions_46__123_catch0_125_(P_c0, arg0)
            else:
              if fn0[0] == 65709:  # {U_Python.Exceptions.{raise0}1}
                P_c0 = fn0[1]
                return _idris_Python_46_Exceptions_46__123_raise0_125_(P_c0, arg0)
              elif fn0[0] == 65710:  # {U_Python.Exceptions.{showException0}1}
                P_c0 = fn0[1]
                return _idris_Python_46_Exceptions_46__123_showException0_125_(P_c0, arg0)
              else:  # {U_Python.Exceptions.{try0}1}
                return _idris_Python_46_Exceptions_46__123_try0_125_(arg0)
          else:
            if fn0[0] < 65715:
              if fn0[0] == 65712:  # {U_Python.Exceptions.{try1}1}
                return _idris_Python_46_Exceptions_46__123_try1_125_(arg0)
              elif fn0[0] == 65713:  # {U_Python.Exceptions.{try2}1}
                return _idris_Python_46_Exceptions_46__123_try2_125_(arg0)
              else:  # {U_Python.Exceptions.{try3}1}
                return _idris_Python_46_Exceptions_46__123_try3_125_(arg0)
            else:
              if fn0[0] == 65715:  # {U_Python.Exceptions.{try4}1}
                P_c0 = fn0[1]
                return _idris_Python_46_Exceptions_46__123_try4_125_(P_c0, arg0)
              elif fn0[0] == 65716:  # {U_Python.Exceptions.{try5}1}
                return _idris_Python_46_Exceptions_46__123_try5_125_(arg0)
              else:  # {U_Python.Fields.{/.0}1}
                P_c0, P_c1 = fn0[1:]
                return _idris_Python_46_Fields_46__123__47__46_0_125_(P_c0, P_c1, arg0)
        else:
          if fn0[0] < 65724:
            if fn0[0] < 65721:
              if fn0[0] == 65718:  # {U_Python.Fields.{/:0}1}
                P_c0 = fn0[1]
                return _idris_Python_46_Fields_46__123__47__58_0_125_(P_c0, arg0)
              elif fn0[0] == 65719:  # {U_Python.Functions.{$.0}1}
                P_c0, P_c1, P_c2 = fn0[1:]
                return _idris_Python_46_Functions_46__123__36__46_0_125_(P_c0, P_c1, P_c2, arg0)
              else:  # {U_Python.Functions.{$:0}1}
                P_c0, P_c1 = fn0[1:]
                return _idris_Python_46_Functions_46__123__36__58_0_125_(P_c0, P_c1, arg0)
            else:
              if fn0[0] == 65721:  # {U_Python.IO.unRaw1}
                P_c0 = fn0[1]
                return _idris_Python_46_IO_46_unRaw(P_c0, arg0)
              elif fn0[0] == 65722:  # {U_Python.Lib.Threading.{forkPIO0}1}
                return _idris_Python_46_Lib_46_Threading_46__123_forkPIO0_125_(arg0)
              else:  # {U_Python.Lib.Threading.{forkPIO1}1}
                return _idris_Python_46_Lib_46_Threading_46__123_forkPIO1_125_(arg0)
          else:
            if fn0[0] < 65727:
              if fn0[0] == 65724:  # {U_Python.Lib.Threading.{forkPIO2}1}
                return _idris_Python_46_Lib_46_Threading_46__123_forkPIO2_125_(arg0)
              elif fn0[0] == 65725:  # {U_Python.Lib.Threading.{forkPIO3}1}
                return _idris_Python_46_Lib_46_Threading_46__123_forkPIO3_125_(arg0)
              else:  # {U_Python.Lib.Threading.{forkPIO4}1}
                P_c0 = fn0[1]
                return _idris_Python_46_Lib_46_Threading_46__123_forkPIO4_125_(P_c0, arg0)
            else:
              if fn0[0] < 65729:
                if fn0[0] == 65727:  # {U_Python.Lib.Threading.{forkPIO5}1}
                  P_c0 = fn0[1]
                  return _idris_Python_46_Lib_46_Threading_46__123_forkPIO5_125_(P_c0, arg0)
                else:  # {U_Python.Lib.Threading.{forkPIO6}1}
                  P_c0 = fn0[1]
                  return _idris_Python_46_Lib_46_Threading_46__123_forkPIO6_125_(P_c0, arg0)
              else:
                if fn0[0] == 65729:  # {U_Python.Lib.Threading.{wait0}1}
                  return _idris_Python_46_Lib_46_Threading_46__123_wait0_125_(arg0)
                else:  # {U_Python.Prim.{Python.Prim.iterate:iter:0_____Python__Prim__idr_84_17_case_lam0}1}
                  P_c0, P_c1 = fn0[1:]
                  return _idris_Python_46_Prim_46__123_Python_46_Prim_46_iterate_58_iter_58_0_95__95__95__95__95_Python_95__95_Prim_95__95_idr_95_84_95_17_95_case_95_lam0_125_(
                    P_c0, P_c1, arg0
                  )
      else:
        if fn0[0] < 65744:
          if fn0[0] < 65737:
            if fn0[0] < 65734:
              if fn0[0] == 65731:  # {U_Python.Prim.{collect0}1}
                P_c0 = fn0[1]
                return _idris_Python_46_Prim_46__123_collect0_125_(P_c0, arg0)
              elif fn0[0] == 65732:  # {U_Python.Prim.{collect1}1}
                return _idris_Python_46_Prim_46__123_collect1_125_(arg0)
              else:  # {U_Python.Prim.{foreach0}1}
                P_c0, P_c1, P_c2 = fn0[1:]
                return _idris_Python_46_Prim_46__123_foreach0_125_(P_c0, P_c1, P_c2, arg0)
            else:
              if fn0[0] == 65734:  # {U_Python.Prim.{foreach1}1}
                P_c0, P_c1, P_c2 = fn0[1:]
                return _idris_Python_46_Prim_46__123_foreach1_125_(P_c0, P_c1, P_c2, arg0)
              elif fn0[0] == 65735:  # {U_Python.Prim.{iterate0}1}
                P_c0, P_c1 = fn0[1:]
                return _idris_Python_46_Prim_46__123_iterate0_125_(P_c0, P_c1, arg0)
              else:  # {U_Python.Prim.{next0}1}
                return _idris_Python_46_Prim_46__123_next0_125_(arg0)
          else:
            if fn0[0] < 65740:
              if fn0[0] == 65737:  # {U_Python.importModule1}
                P_c0, P_c1 = fn0[1:]
                return _idris_Python_46_importModule(P_c0, P_c1, arg0)
              elif fn0[0] == 65738:  # {U_Python.{marshalPIO0}1}
                P_c0 = fn0[1]
                return _idris_Python_46__123_marshalPIO0_125_(P_c0, arg0)
              else:  # {U_io_bind1}
                P_c0, P_c1, P_c2, P_c3, P_c4 = fn0[1:]
                return _idris_io_95_bind(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
            else:
              if fn0[0] < 65742:
                if fn0[0] == 65740:  # {U_io_pure1}
                  P_c0, P_c1, P_c2 = fn0[1:]
                  return _idris_io_95_pure(P_c0, P_c1, P_c2, arg0)
                else:  # {U_prim__toStrBigInt1}
                  return _idris_prim_95__95_toStrBigInt(arg0)
              else:
                if fn0[0] == 65742:  # {U_prim__toStrInt1}
                  return _idris_prim_95__95_toStrInt(arg0)
                else:  # {U_prim__zextInt_BigInt1}
                  return _idris_prim_95__95_zextInt_95_BigInt(arg0)
        else:
          if fn0[0] < 65750:
            if fn0[0] < 65747:
              if fn0[0] == 65744:  # {U_prim_lenString1}
                return _idris_prim_95_lenString(arg0)
              elif fn0[0] == 65745:  # {U_prim_write1}
                P_c0, P_c1 = fn0[1:]
                return _idris_prim_95_write(P_c0, P_c1, arg0)
              else:  # {U_{Python.Lib.Threading.forkPIO:worker:0_lam0}1}
                return _idris__123_Python_46_Lib_46_Threading_46_forkPIO_58_worker_58_0_95_lam0_125_(
                  arg0
                )
            else:
              if fn0[0] == 65747:  # {U_{Python.Lib.Threading.forkPIO:worker:0_lam1}1}
                P_c0 = fn0[1]
                return _idris__123_Python_46_Lib_46_Threading_46_forkPIO_58_worker_58_0_95_lam1_125_(
                  P_c0, arg0
                )
              elif fn0[0] == 65748:  # {U_{Python.Prim.iterate:iter:0_lam0}1}
                P_c0 = fn0[1]
                return _idris__123_Python_46_Prim_46_iterate_58_iter_58_0_95_lam0_125_(P_c0, arg0)
              else:  # {U_{Python.Prim.iterate:iter:0_lam1}1}
                return _idris__123_Python_46_Prim_46_iterate_58_iter_58_0_95_lam1_125_(arg0)
          else:
            if fn0[0] < 65753:
              if fn0[0] == 65750:  # {U_{Python.Prim.iterate:iter:0_lam2}1}
                return _idris__123_Python_46_Prim_46_iterate_58_iter_58_0_95_lam2_125_(arg0)
              elif fn0[0] == 65751:  # {U_{Python.Prim.iterate:iter:0_lam3}1}
                return _idris__123_Python_46_Prim_46_iterate_58_iter_58_0_95_lam3_125_(arg0)
              else:  # {U_{Python.Prim.iterate:iter:0_lam4}1}
                P_c0, P_c1 = fn0[1:]
                return _idris__123_Python_46_Prim_46_iterate_58_iter_58_0_95_lam4_125_(
                  P_c0, P_c1, arg0
                )
            else:
              if fn0[0] < 65755:
                if fn0[0] == 65753:  # {U_{Python.Prim.iterate:iter:0_lam5}1}
                  P_c0, P_c1, P_c2 = fn0[1:]
                  return _idris__123_Python_46_Prim_46_iterate_58_iter_58_0_95_lam5_125_(
                    P_c0, P_c1, P_c2, arg0
                  )
                else:  # {U_{io_bind1}1}
                  P_c0, P_c1, P_c2, P_c3, P_c4, P_c5 = fn0[1:]
                  return io_bind1(P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, arg0)
              else:
                assert fn0[0] == 65755  # {U_{unsafePerformIO0}1}
                return unsafePerformIO0(arg0)
    return _idris_error("unreachable due to case in tail position")

# {APPLY20}
def _idris__123_APPLY20_125_(fn0, _idris__123_arg00_125_, _idris__123_arg10_125_):
  while True:
    return None

# {EVAL0}
def EVAL0(arg0):
  while True:
    return arg0

# Prelude.Functor.{Prelude.Monad.@Prelude.Functor.Functor$IO' ffi:!map:0_lam0}
def _idris_Prelude_46_Functor_46__123_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0_95_lam0_125_(
  e3, in0
):
  while True:
    return (65740, None, None, APPLY0(e3, in0))  # {U_io_pure1}

# Prelude.Interfaces.{Prelude.Show.@Prelude.Interfaces.Ord$Prec:!>=:0_lam0}
def _idris_Prelude_46_Interfaces_46__123_Prelude_46_Show_46__64_Prelude_46_Interfaces_46_Ord_36_Prec_58__33__62__61__58_0_95_lam0_125_(
  e0, e1
):
  while True:
    return _idris_Prelude_46_Interfaces_46_Prelude_46_Show_46__64_Prelude_46_Interfaces_46_Eq_36_Prec_58__33__61__61__58_0(
      e0, e1
    )

# {Python.Lib.Threading.forkPIO:worker:0_lam0}
def _idris__123_Python_46_Lib_46_Threading_46_forkPIO_58_worker_58_0_95_lam0_125_(
  in1
):
  while True:
    return (0,)  # Python.Telescope.Return

# Python.Prim.{Python.Prim.iterate:iter:0_____Python__Prim__idr_84_17_case_lam0}
def _idris_Python_46_Prim_46__123_Python_46_Prim_46_iterate_58_iter_58_0_95__95__95__95__95_Python_95__95_Prim_95__95_idr_95_84_95_17_95_case_95_lam0_125_(
  e9, e10, in1
):
  while True:
    return _idris_Python_46_Prim_46_iterate_58_iter_58_0(
      None, None, None, None, None, None, None, None, e9, in1, e10
    )

# {Python.Prim.iterate:iter:0_lam0}
def _idris__123_Python_46_Prim_46_iterate_58_iter_58_0_95_lam0_125_(in3, in4):
  while True:
    return (65739, None, None, None, in3, in4)  # {U_io_bind1}

# Python.Exceptions.{catch0}
def _idris_Python_46_Exceptions_46__123_catch0_125_(e2, in0):
  while True:
    if in0[0] == 1:  # Python.Exceptions.Except
      in1, in2 = in0[1:]
      return APPLY0(APPLY0(e2, in1), in2)
    else:  # Python.Exceptions.OK
      in3 = in0[1]
      return (65740, None, None, in3)  # {U_io_pure1}
    return _idris_error("unreachable due to case in tail position")

# Python.Prim.{collect0}
def _idris_Python_46_Prim_46__123_collect0_125_(in0, in1):
  while True:
    return (65740, None, None, in0.cons(in1))  # {U_io_pure1}

# Python.Prim.{foreach0}
def _idris_Python_46_Prim_46__123_foreach0_125_(e3, e4, e5, in1):
  while True:
    return _idris_foreach(e3, e4, e5)

# Python.Lib.Threading.{forkPIO0}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO0_125_(in1):
  while True:
    return (0,)  # Python.Telescope.Return

# {io_bind0}
def io_bind0(e0, e1, e2, e3, e4, _idris_w, in0):
  while True:
    return APPLY0(e4, in0)

# Python.Prim.{iterate0}
def _idris_Python_46_Prim_46__123_iterate0_125_(e4, e5, in0):
  while True:
    return _idris_Python_46_Prim_46_iterate_58_iter_58_0(
      None, None, None, None, None, None, None, None, in0, e4, e5
    )

# Prelude.Strings.{length0}
def _idris_Prelude_46_Strings_46__123_length0_125_(in0):
  while True:
    return in0

# Main.{main0}
def _idris_Main_46__123_main0_125_(in2):
  while True:
    return (0,)  # Python.Telescope.Return

# Python.{marshalPIO0}
def _idris_Python_46__123_marshalPIO0_125_(e1, in0):
  while True:
    return _idris_marshal_PIO(e1)

# Python.Prim.{next0}
def _idris_Python_46_Prim_46__123_next0_125_(in0):
  while True:
    if in0[0] == 1:  # Python.Exceptions.Except
      in1, in2 = in0[1:]
      if in1[0] == 0:  # Python.Exceptions.StopIteration
        return (65740, None, None, None)  # {U_io_pure1}
      else:
        return _idris_Python_46_Exceptions_46_raise(None, in2)
      return _idris_error("unreachable due to case in tail position")
    else:  # Python.Exceptions.OK
      in3 = in0[1]
      return (65740, None, None, in3)  # {U_io_pure1}
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.{primNumShow0}
def _idris_Prelude_46_Show_46__123_primNumShow0_125_(in1):
  while True:
    aux1 = (in1 == u'-')
    if aux1 == 0:
      return False
    else:
      return True
    return _idris_error("unreachable due to case in tail position")

# Prelude.Interactive.{putStr'0}
def _idris_Prelude_46_Interactive_46__123_putStr_39_0_125_(in0):
  while True:
    return (65740, None, None, Unit)  # {U_io_pure1}

# Python.Exceptions.{raise0}
def _idris_Python_46_Exceptions_46__123_raise0_125_(e1, in0):
  while True:
    return _idris_raise(e1)

# {runMain0}
def runMain0():
  while True:
    return EVAL0(APPLY0(_idris_Main_46_main(), None))

# Python.Exceptions.{showException0}
def _idris_Python_46_Exceptions_46__123_showException0_125_(e0, in0):
  while True:
    return str(e0)

# Python.Functions.{strip0}
def _idris_Python_46_Functions_46__123_strip0_125_(in2):
  while True:
    return in2

# Python.Exceptions.{try0}
def _idris_Python_46_Exceptions_46__123_try0_125_(in1):
  while True:
    return in1

# {unsafePerformIO0}
def unsafePerformIO0(in0):
  while True:
    return in0

# Python.Lib.Threading.{wait0}
def _idris_Python_46_Lib_46_Threading_46__123_wait0_125_(in0):
  while True:
    return (0,)  # Python.Telescope.Return

# {Python.Lib.Threading.forkPIO:worker:0_lam1}
def _idris__123_Python_46_Lib_46_Threading_46_forkPIO_58_worker_58_0_95_lam1_125_(
  e2, in0
):
  while True:
    return _idris_Python_46_Functions_46__36__46_(
      None,
      None,
      (1, (0,), (65746,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_{Python.Lib.Threading.forkPIO:worker:0_lam0}1}
      _idris_Python_46_Fields_46__47__46_(None, None, e2, u'put', None),
      None,
      (0, in0, Unit)  # Builtins.MkDPair
    )

# {Python.Prim.iterate:iter:0_lam1}
def _idris__123_Python_46_Prim_46_iterate_58_iter_58_0_95_lam1_125_(in3):
  while True:
    return (65748, in3)  # {U_{Python.Prim.iterate:iter:0_lam0}1}

# Python.Prim.{collect1}
def _idris_Python_46_Prim_46__123_collect1_125_(in0):
  while True:
    return (65731, in0)  # {U_Python.Prim.{collect0}1}

# Python.Prim.{foreach1}
def _idris_Python_46_Prim_46__123_foreach1_125_(e3, e4, e5, in0):
  while True:
    return _idris_PE_95__60__36__62__95_cc6adb39(
      None,
      None,
      (65721, None),  # {U_Python.IO.unRaw1}
      (65733, e3, e4, e5)  # {U_Python.Prim.{foreach0}1}
    )

# Python.Lib.Threading.{forkPIO1}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO1_125_(in0):
  while True:
    return (1, (2, 0), (65722,))  # Python.Telescope.Bind, Python.Telescope.Default, {U_Python.Lib.Threading.{forkPIO0}1}

# {io_bind1}
def io_bind1(e0, e1, e2, e3, e4, _idris_w, in0):
  while True:
    return APPLY0(io_bind0(e0, e1, e2, e3, e4, _idris_w, in0), _idris_w)

# Main.{main1}
def _idris_Main_46__123_main1_125_(in6):
  while True:
    return (0,)  # Python.Telescope.Return

# Prelude.Show.{primNumShow1}
def _idris_Prelude_46_Show_46__123_primNumShow1_125_(e0, e1, e2, e3, in0, in2):
  while True:
    return (65706,)  # {U_Prelude.Show.{primNumShow0}1}

# Python.Exceptions.{try1}
def _idris_Python_46_Exceptions_46__123_try1_125_(in2):
  while True:
    return (0, in2)  # Prelude.Either.Left

# {unsafePerformIO1}
def unsafePerformIO1(e0, e1, e2):
  while True:
    return (65755,)  # {U_{unsafePerformIO0}1}

# {Python.Prim.iterate:iter:0_lam2}
def _idris__123_Python_46_Prim_46_iterate_58_iter_58_0_95_lam2_125_(in2):
  while True:
    return (65749,)  # {U_{Python.Prim.iterate:iter:0_lam1}1}

# Python.Lib.Threading.{forkPIO2}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO2_125_(in4):
  while True:
    return (0,)  # Python.Telescope.Return

# {io_bind2}
def io_bind2(e0, e1, e2, e3, e4, _idris_w):
  while True:
    return (65754, e0, e1, e2, e3, e4, _idris_w)  # {U_{io_bind1}1}

# Main.{main2}
def _idris_Main_46__123_main2_125_(in5):
  while True:
    return (1, (0,), (65667,))  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main1}1}

# Prelude.Show.{primNumShow2}
def _idris_Prelude_46_Show_46__123_primNumShow2_125_(in0, e0, e1, e2, e3):
  while True:
    aux1 = _idris_Prelude_46_Strings_46_strM(in0)
    if aux1[0] == 1:  # Prelude.Strings.StrCons
      in2 = aux1[1]
      return APPLY0(
        _idris_Prelude_46_Show_46__123_primNumShow1_125_(e0, e1, e2, e3, in0, in2),
        in2
      )
    else:  # Prelude.Strings.StrNil
      return False
    return _idris_error("unreachable due to case in tail position")

# Python.Exceptions.{try2}
def _idris_Python_46_Exceptions_46__123_try2_125_(in3):
  while True:
    return in3

# {Python.Prim.iterate:iter:0_lam3}
def _idris__123_Python_46_Prim_46_iterate_58_iter_58_0_95_lam3_125_(in1):
  while True:
    return (65750,)  # {U_{Python.Prim.iterate:iter:0_lam2}1}

# Python.Lib.Threading.{forkPIO3}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO3_125_(in3):
  while True:
    return (1, (0,), (65724,))  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Threading.{forkPIO2}1}

# Main.{main3}
def _idris_Main_46__123_main3_125_(in8):
  while True:
    return (0,)  # Python.Telescope.Return

# Python.Exceptions.{try3}
def _idris_Python_46_Exceptions_46__123_try3_125_(in4):
  while True:
    return (1, in4)  # Prelude.Either.Right

# {Python.Prim.iterate:iter:0_lam4}
def _idris__123_Python_46_Prim_46_iterate_58_iter_58_0_95_lam4_125_(e8, e10, in6):
  while True:
    return _idris_Python_46_Prim_46_iterate_58_iter_58_0(
      None, None, None, None, None, None, None, None, e8, in6, e10
    )

# Python.Lib.Threading.{forkPIO4}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO4_125_(in2, in6):
  while True:
    return (65740, None, None, in2)  # {U_io_pure1}

# Main.{main4}
def _idris_Main_46__123_main4_125_(in15, in16, in17):
  while True:
    return _idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
      None, None, in15, in16, in17
    )

# Python.Exceptions.{try4}
def _idris_Python_46_Exceptions_46__123_try4_125_(e1, in0):
  while True:
    return _idris_try(
      e1,
      (65702, None, None, None, (65711,), (65712,)),  # {U_Prelude.Basics..1}, {U_Python.Exceptions.{try0}1}, {U_Python.Exceptions.{try1}1}
      (
        65702,  # {U_Prelude.Basics..1}
        None,
        None,
        None,
        (65702, None, None, None, (65713,), (65714,)),  # {U_Prelude.Basics..1}, {U_Python.Exceptions.{try2}1}, {U_Python.Exceptions.{try3}1}
        (65721, None)  # {U_Python.IO.unRaw1}
      )
    )

# {Python.Prim.iterate:iter:0_lam5}
def _idris__123_Python_46_Prim_46_iterate_58_iter_58_0_95_lam5_125_(
  e10, e9, e8, in0
):
  while True:
    if in0 is not None:  # Prelude.Maybe.Just
      in5 = in0
      return APPLY0(
        APPLY0(
          _idris_Prelude_46_Monad_46__62__62__61_(None, (65751,), None, None),  # {U_{Python.Prim.iterate:iter:0_lam3}1}
          APPLY0(APPLY0(e10, e9), in5)
        ),
        (65752, e8, e10)  # {U_{Python.Prim.iterate:iter:0_lam4}1}
      )
    else:  # Prelude.Maybe.Nothing
      return (65740, None, None, e9)  # {U_io_pure1}
    return _idris_error("unreachable due to case in tail position")

# Python.Lib.Threading.{forkPIO5}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO5_125_(in2, in5):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Functions_46__36__46_(
        None,
        None,
        (0,),  # Python.Telescope.Return
        _idris_Python_46_Fields_46__47__46_(None, None, in5, u'start', None),
        None,
        Unit
      ),
      (65726, in2)  # {U_Python.Lib.Threading.{forkPIO4}1}
    )

# Main.{main5}
def _idris_Main_46__123_main5_125_(in15, in16):
  while True:
    return (65696, in15, in16)  # {U_Main.{main4}1}

# Python.Exceptions.{try5}
def _idris_Python_46_Exceptions_46__123_try5_125_(in5):
  while True:
    if in5[0] == 0:  # Prelude.Either.Left
      in6 = in5[1]
      return (
        65740,  # {U_io_pure1}
        None,
        None,
        (
          1,  # Python.Exceptions.Except
          _idris_Python_46_Exceptions_46_fromString(
            _idris_Python_46_Fields_46__47__46_(
              None,
              None,
              _idris_Python_46_Fields_46__47__46_(None, None, in6, u'__class__', None),
              u'__name__',
              None
            )
          ),
          in6
        )
      )
    else:  # Prelude.Either.Right
      in7 = in5[1]
      return (65740, None, None, (0, in7))  # {U_io_pure1}, Python.Exceptions.OK
    return _idris_error("unreachable due to case in tail position")

# Python.Lib.Threading.{forkPIO6}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO6_125_(e1, in2):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Functions_46__36__58_(
        None,
        None,
        (1, (0,), (65725,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Threading.{forkPIO3}1}
        _idris_Python_46_Fields_46__47__58_(
          None,
          None,
          (65737, None, u'threading'),  # {U_Python.importModule1}
          u'Thread',
          None
        ),
        None,
        (
          0,  # Builtins.MkDPair
          None,
          (
            0,  # Builtins.MkDPair
            _idris_Python_46_marshalPIO(
              None,
              _idris_Python_46_Lib_46_Threading_46_forkPIO_58_worker_58_0(None, e1, in2)
            ),
            Unit
          )
        )
      ),
      (65727, in2)  # {U_Python.Lib.Threading.{forkPIO5}1}
    )

# Main.{main6}
def _idris_Main_46__123_main6_125_(in15):
  while True:
    return (65697, in15)  # {U_Main.{main5}1}

# Main.{main7}
def _idris_Main_46__123_main7_125_(in14):
  while True:
    return (65698,)  # {U_Main.{main6}1}

# Main.{main8}
def _idris_Main_46__123_main8_125_(in13):
  while True:
    return (65699,)  # {U_Main.{main7}1}

# Main.{main9}
def _idris_Main_46__123_main9_125_(in18, in19):
  while True:
    return (in18 + in19)

# Main.{main10}
def _idris_Main_46__123_main10_125_(in18):
  while True:
    return (65701, in18)  # {U_Main.{main9}1}

# Main.{main11}
def _idris_Main_46__123_main11_125_(in11, in21):
  while True:
    return (65740, None, None, (in11 + 1))  # {U_io_pure1}

# Main.{main12}
def _idris_Main_46__123_main12_125_(in11, in20):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        ((_idris_Prelude_46_Show_46_primNumShow(None, (65742,), (0,), (in11 + 1)) + (u'. ' + in20)) + u'\u000a')  # {U_prim__toStrInt1}, Prelude.Show.Open
      ),
      (65658, in11)  # {U_Main.{main11}1}
    )

# Main.{main13}
def _idris_Main_46__123_main13_125_(in11, in12):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_PE_95__60__36__62__95_cc6adb39(
        None,
        None,
        _idris_Prelude_46_Foldable_46_concat(None, None, (65700,), (0, (65657,), u'')),  # {U_Main.{main8}1}, constructor of Prelude.Algebra.Monoid, {U_Main.{main10}1}
        _idris_Python_46_Prim_46_collect(
          None,
          None,
          _idris_Python_46_Fields_46__47__46_(None, None, in12, u'strings', None),
          None
        )
      ),
      (65659, in11)  # {U_Main.{main12}1}
    )

# Main.{main14}
def _idris_Main_46__123_main14_125_(in11):
  while True:
    return (65660, in11)  # {U_Main.{main13}1}

# Main.{main15}
def _idris_Main_46__123_main15_125_(in28):
  while True:
    return (0,)  # Python.Telescope.Return

# Main.{main16}
def _idris_Main_46__123_main16_125_(in29, in30):
  while True:
    return (65740, None, None, APPLY0(_idris_Prelude_46_Strings_46_length(), in29))  # {U_io_pure1}

# Main.{main17}
def _idris_Main_46__123_main17_125_(in26, in29):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        ((u'thread ' + (in26 + u' done')) + u'\u000a')
      ),
      (65663, in29)  # {U_Main.{main16}1}
    )

# Main.{main18}
def _idris_Main_46__123_main18_125_(in1, in26, in27):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Fields_46__47__58_(
        None,
        None,
        _idris_Python_46_Functions_46__36__46_(
          None,
          None,
          (1, (0,), (65662,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main15}1}
          _idris_Python_46_Fields_46__47__46_(None, None, in1, u'get', None),
          None,
          (0, u'http://idris-lang.org', Unit)  # Builtins.MkDPair
        ),
        u'text',
        None
      ),
      (65664, in26)  # {U_Main.{main17}1}
    )

# Main.{main19}
def _idris_Main_46__123_main19_125_(in1, in26):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        ((u'thread ' + (in26 + u' starting')) + u'\u000a')
      ),
      (65665, in1, in26)  # {U_Main.{main18}1}
    )

# Main.{main20}
def _idris_Main_46__123_main20_125_(in40):
  while True:
    return (0,)  # Python.Telescope.Return

# Main.{main21}
def _idris_Main_46__123_main21_125_(in41):
  while True:
    return _idris_Prelude_46_Interactive_46_putStr_39_(
      None,
      u'Something\'s wrong, your root\'s homedir is writable!\u000a'
    )

# Main.{main22}
def _idris_Main_46__123_main22_125_(in42, in43):
  while True:
    if in42[0] == 11:  # Python.Exceptions.OSError
      return _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        ((u'  -> (1) everything\'s fine: ' + _idris_Python_46_Exceptions_46_showException(in43)) + u'\u000a')
      )
    else:
      return _idris_Python_46_Exceptions_46_raise(None, in43)
    return _idris_error("unreachable due to case in tail position")

# Main.{main23}
def _idris_Main_46__123_main23_125_(in42):
  while True:
    return (65670, in42)  # {U_Main.{main22}1}

# Main.{main24}
def _idris_Main_46__123_main24_125_(in45):
  while True:
    return (0,)  # Python.Telescope.Return

# Main.{main25}
def _idris_Main_46__123_main25_125_(in46):
  while True:
    if in46[0] == 1:  # Python.Exceptions.Except
      in47, in48 = in46[1:]
      if in47[0] == 11:  # Python.Exceptions.OSError
        return _idris_Prelude_46_Interactive_46_putStr_39_(
          None,
          ((u'  -> (2) everything\'s fine: ' + _idris_Python_46_Exceptions_46_showException(in48)) + u'\u000a')
        )
      else:
        return _idris_Python_46_Exceptions_46_raise(None, in48)
      return _idris_error("unreachable due to case in tail position")
    else:  # Python.Exceptions.OK
      in49 = in46[1]
      return _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        u'Your root could probably use some security lessons!\u000a'
      )
    return _idris_error("unreachable due to case in tail position")

# Main.{main26}
def _idris_Main_46__123_main26_125_(in38, in44):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Exceptions_46_try(
        None,
        _idris_Python_46_Functions_46__36__46_(
          None,
          None,
          (1, (0,), (65672,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main24}1}
          _idris_Python_46_Fields_46__47__46_(None, None, in38, u'mkdir', None),
          None,
          (0, u'/root/hello', Unit)  # Builtins.MkDPair
        )
      ),
      (65673,)  # {U_Main.{main25}1}
    )

# Main.{main27}
def _idris_Main_46__123_main27_125_(in38, in39):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Exceptions_46_catch(
        None,
        _idris_Python_46_Exceptions_46_try(
          None,
          (
            65739,  # {U_io_bind1}
            None,
            None,
            None,
            _idris_Python_46_Functions_46__36__46_(
              None,
              None,
              (1, (0,), (65668,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main20}1}
              _idris_Python_46_Fields_46__47__46_(None, None, in38, u'mkdir', None),
              None,
              (0, u'/root/hello', Unit)  # Builtins.MkDPair
            ),
            (65669,)  # {U_Main.{main21}1}
          )
        ),
        (65671,)  # {U_Main.{main23}1}
      ),
      (65674, in38)  # {U_Main.{main26}1}
    )

# Main.{main28}
def _idris_Main_46__123_main28_125_(in38):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        u'And now, let\'s fail!\u000a'
      ),
      (65675, in38)  # {U_Main.{main27}1}
    )

# Main.{main29}
def _idris_Main_46__123_main29_125_(in37):
  while True:
    return (65739, None, None, None, (65737, None, u'os'), (65676,))  # {U_io_bind1}, {U_Python.importModule1}, {U_Main.{main28}1}

# Main.{main30}
def _idris_Main_46__123_main30_125_(in36):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(None, u'\u000a'),
      (65677,)  # {U_Main.{main29}1}
    )

# Main.{main31}
def _idris_Main_46__123_main31_125_(in34, in35):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        ((u'thread B says ' + _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_Nat_58__33_show_58_0(
          in34
        )) + u'\u000a')
      ),
      (65679,)  # {U_Main.{main30}1}
    )

# Main.{main32}
def _idris_Main_46__123_main32_125_(in33, in34):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        ((u'thread A says ' + _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_Nat_58__33_show_58_0(
          in33
        )) + u'\u000a')
      ),
      (65680, in34)  # {U_Main.{main31}1}
    )

# Main.{main33}
def _idris_Main_46__123_main33_125_(in32, in33):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Lib_46_Threading_46_wait(None, in32),
      (65681, in33)  # {U_Main.{main32}1}
    )

# Main.{main34}
def _idris_Main_46__123_main34_125_(in31, in32):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Lib_46_Threading_46_wait(None, in31),
      (65682, in32)  # {U_Main.{main33}1}
    )

# Main.{main35}
def _idris_Main_46__123_main35_125_(in25, in31):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Lib_46_Threading_46_forkPIO(None, APPLY0(in25, u'B')),
      (65683, in31)  # {U_Main.{main34}1}
    )

# Main.{main36}
def _idris_Main_46__123_main36_125_(in1, in24):
  while True:
    in25 = (65666, in1)  # {U_Main.{main19}1}
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Lib_46_Threading_46_forkPIO(None, APPLY0(in25, u'A')),
      (65684, in25)  # {U_Main.{main35}1}
    )

# Main.{main37}
def _idris_Main_46__123_main37_125_(in1, in23):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(None, u'\u000a'),
      (65685, in1)  # {U_Main.{main36}1}
    )

# Main.{main38}
def _idris_Main_46__123_main38_125_(in1, in22):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        ((u'Total number of features: ' + _idris_Prelude_46_Show_46_primNumShow(None, (65742,), (0,), in22)) + u'\u000a')  # {U_prim__toStrInt1}, Prelude.Show.Open
      ),
      (65686, in1)  # {U_Main.{main37}1}
    )

# Main.{main39}
def _idris_Main_46__123_main39_125_(in9, in1, in10):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Prim_46_iterate(None, None, None, in9, 0, (65661,), None),  # {U_Main.{main14}1}
      (65687, in1)  # {U_Main.{main38}1}
    )

# Main.{main40}
def _idris_Main_46__123_main40_125_(in1, in9):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        u'Idris has got the following exciting features:\u000a'
      ),
      (65688, in9, in1)  # {U_Main.{main39}1}
    )

# Main.{main41}
def _idris_Main_46__123_main41_125_(in1, in7):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Functions_46__36__46_(
        None,
        None,
        (1, (0,), (65689,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main3}1}
        _idris_Python_46_Fields_46__47__46_(None, None, in7, u'select', None),
        None,
        (0, u'div.entry-content li', Unit)  # Builtins.MkDPair
      ),
      (65690, in1)  # {U_Main.{main40}1}
    )

# Main.{main42}
def _idris_Main_46__123_main42_125_(in3, in1, in4):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Functions_46__36__46_(
        None,
        None,
        (1, (0,), (65678,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main2}1}
        _idris_Python_46_Fields_46__47__46_(None, None, in4, u'BeautifulSoup', None),
        None,
        (0, in3, (0, u'html.parser', Unit))  # Builtins.MkDPair, Builtins.MkDPair
      ),
      (65691, in1)  # {U_Main.{main41}1}
    )

# Main.{main43}
def _idris_Main_46__123_main43_125_(in1, in3):
  while True:
    return (65739, None, None, None, (65737, None, u'bs4'), (65692, in3, in1))  # {U_io_bind1}, {U_Python.importModule1}, {U_Main.{main42}1}

# Main.{main44}
def _idris_Main_46__123_main44_125_(in1):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Fields_46__47__58_(
        None,
        None,
        _idris_Python_46_Functions_46__36__46_(
          None,
          None,
          (1, (0,), (65656,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main0}1}
          _idris_Python_46_Fields_46__47__46_(None, None, in1, u'get', None),
          None,
          (0, u'http://idris-lang.org', Unit)  # Builtins.MkDPair
        ),
        u'text',
        None
      ),
      (65693, in1)  # {U_Main.{main43}1}
    )

# Main.{main45}
def _idris_Main_46__123_main45_125_(in0):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Functions_46__36__46_(
        None,
        None,
        (0,),  # Python.Telescope.Return
        _idris_Python_46_Fields_46__47__46_(None, None, in0, u'Session', None),
        None,
        Unit
      ),
      (65694,)  # {U_Main.{main44}1}
    )

# Main.exports, greet
def _idris_Main_46_exports_58_greet_58_0(e0):
  while True:
    return _idris_Prelude_46_Interactive_46_putStr_39_(
      None,
      ((u'Hello ' + (e0 + u'!')) + u'\u000a')
    )

# Python.Lib.Threading.forkPIO, worker
def _idris_Python_46_Lib_46_Threading_46_forkPIO_58_worker_58_0(e0, e1, e2):
  while True:
    return (65739, None, None, None, e1, (65747, e2))  # {U_io_bind1}, {U_{Python.Lib.Threading.forkPIO:worker:0_lam1}1}

# Python.Prim.iterate, iter
def _idris_Python_46_Prim_46_iterate_58_iter_58_0(
  e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10
):
  while True:
    return (
      65739,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Prim_46_next(None, e8),
      (65753, e10, e9, e8)  # {U_{Python.Prim.iterate:iter:0_lam5}1}
    )

# Prelude.List.reverse, reverse'
def _idris_Prelude_46_List_46_reverse_58_reverse_39__58_0(e0, e1, e2):
  while True:
    if e2:  # Prelude.List.::
      in0, in1 = e2.head, e2.tail
      e0, e1, e2, = None, e1.cons(in0), in1,
      continue
      return _idris_error("unreachable due to tail call")
    else:  # Prelude.List.Nil
      return e1
    return _idris_error("unreachable due to case in tail position")

# Decidable.Equality.Decidable.Equality.Char implementation of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Char_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Decidable.Equality.Decidable.Equality.Int implementation of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Int_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Decidable.Equality.Decidable.Equality.Integer implementation of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Integer_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Decidable.Equality.Decidable.Equality.ManagedPtr implementation of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_ManagedPtr_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Decidable.Equality.Decidable.Equality.Ptr implementation of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Ptr_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Decidable.Equality.Decidable.Equality.String implementation of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_String_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Decidable.Equality.Decidable.Equality.Bool implementation of Decidable.Equality.DecEq, method decEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Bool_58__33_decEq_58_0(
  e0, e1
):
  while True:
    if not e1:  # Prelude.Bool.False
      if not e0:  # Prelude.Bool.False
        return (0,)  # Prelude.Basics.Yes
      else:  # Prelude.Bool.True
        return (1,)  # Prelude.Basics.No
      return _idris_error("unreachable due to case in tail position")
    else:  # Prelude.Bool.True
      if not e0:  # Prelude.Bool.False
        return (1,)  # Prelude.Basics.No
      else:  # Prelude.Bool.True
        return (0,)  # Prelude.Basics.Yes
      return _idris_error("unreachable due to case in tail position")
    return _idris_error("unreachable due to case in tail position")

# Prelude.Interfaces.Prelude.Nat.Nat implementation of Prelude.Interfaces.Eq, method ==
def _idris_Prelude_46_Interfaces_46_Prelude_46_Nat_46__64_Prelude_46_Interfaces_46_Eq_36_Nat_58__33__61__61__58_0(
  e0, e1
):
  while True:
    if e1 == 0:
      if e0 == 0:
        return True
      else:
        return False
      return _idris_error("unreachable due to case in tail position")
    elif True:
      in0 = (e1 - 1)
      if e0 == 0:
        return False
      else:
        in1 = (e0 - 1)
        e0, e1, = in1, in0,
        continue
        return _idris_error("unreachable due to tail call")
      return _idris_error("unreachable due to case in tail position")
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# Prelude.Interfaces.Prelude.Show.Prec implementation of Prelude.Interfaces.Eq, method ==
def _idris_Prelude_46_Interfaces_46_Prelude_46_Show_46__64_Prelude_46_Interfaces_46_Eq_36_Prec_58__33__61__61__58_0(
  e0, e1
):
  while True:
    if e1[0] == 4:  # Prelude.Show.User
      in0 = e1[1]
      if e0[0] == 4:  # Prelude.Show.User
        in1 = e0[1]
        return _idris_Prelude_46_Interfaces_46_Prelude_46_Nat_46__64_Prelude_46_Interfaces_46_Eq_36_Nat_58__33__61__61__58_0(
          in1, in0
        )
      else:
        aux1 = (_idris_Prelude_46_Show_46_precCon(e0) == _idris_Prelude_46_Show_46_precCon(e1))
        if aux1 == 0:
          return False
        else:
          return True
        return _idris_error("unreachable due to case in tail position")
      return _idris_error("unreachable due to case in tail position")
    else:
      aux2 = (_idris_Prelude_46_Show_46_precCon(e0) == _idris_Prelude_46_Show_46_precCon(e1))
      if aux2 == 0:
        return False
      else:
        return True
      return _idris_error("unreachable due to case in tail position")
    return _idris_error("unreachable due to case in tail position")

# Prelude.Foldable.Prelude.List.List implementation of Prelude.Foldable.Foldable, method foldr
def _idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
  e0, e1, e2, e3, e4
):
  while True:
    if e4:  # Prelude.List.::
      in0, in1 = e4.head, e4.tail
      return APPLY0(
        APPLY0(e2, in0),
        _idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
          None, None, e2, e3, in1
        )
      )
    else:  # Prelude.List.Nil
      return e3
    return _idris_error("unreachable due to case in tail position")

# Prelude.Functor.Prelude.Monad.IO' ffi implementation of Prelude.Functor.Functor, method map
def _idris_Prelude_46_Functor_46_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
  e0, e1, e2, e3, e4
):
  while True:
    return (65739, None, None, None, e4, (65703, e3))  # {U_io_bind1}, {U_Prelude.Functor.{Prelude.Monad.@Prelude.Functor.Functor$IO' ffi:!map:0_lam0}1}

# Prelude.Interfaces.Prelude.Interfaces.Integer implementation of Prelude.Interfaces.Ord, method compare
def _idris_Prelude_46_Interfaces_46_Prelude_46_Interfaces_46__64_Prelude_46_Interfaces_46_Ord_36_Integer_58__33_compare_58_0(
  e0, e1
):
  while True:
    aux2 = (e0 == e1)
    if aux2 == 0:
      aux3 = False
    else:
      aux3 = True
    aux1 = aux3
    if not aux1:  # Prelude.Bool.False
      aux5 = (e0 < e1)
      if aux5 == 0:
        aux6 = False
      else:
        aux6 = True
      aux4 = aux6
      if not aux4:  # Prelude.Bool.False
        return (2,)  # Prelude.Interfaces.GT
      else:  # Prelude.Bool.True
        return (0,)  # Prelude.Interfaces.LT
      return _idris_error("unreachable due to case in tail position")
    else:  # Prelude.Bool.True
      return (1,)  # Prelude.Interfaces.EQ
    return _idris_error("unreachable due to case in tail position")

# Prelude.Interfaces.Prelude.Nat.Nat implementation of Prelude.Interfaces.Ord, method compare
def _idris_Prelude_46_Interfaces_46_Prelude_46_Nat_46__64_Prelude_46_Interfaces_46_Ord_36_Nat_58__33_compare_58_0(
  e0, e1
):
  while True:
    if e1 == 0:
      if e0 == 0:
        return (1,)  # Prelude.Interfaces.EQ
      else:
        in0 = (e0 - 1)
        return (2,)  # Prelude.Interfaces.GT
      return _idris_error("unreachable due to case in tail position")
    else:
      in1 = (e1 - 1)
      if e0 == 0:
        return (0,)  # Prelude.Interfaces.LT
      else:
        in2 = (e0 - 1)
        e0, e1, = in2, in1,
        continue
        return _idris_error("unreachable due to tail call")
      return _idris_error("unreachable due to case in tail position")
    return _idris_error("unreachable due to case in tail position")

# Prelude.Interfaces.Prelude.Show.Prec implementation of Prelude.Interfaces.Ord, method >=
def _idris_Prelude_46_Interfaces_46_Prelude_46_Show_46__64_Prelude_46_Interfaces_46_Ord_36_Prec_58__33__62__61__58_0(
  e0, e1
):
  while True:
    aux2 = _idris_Prelude_46_Interfaces_46_Prelude_46_Show_46__64_Prelude_46_Interfaces_46_Ord_36_Prec_58__33_compare_58_0(
      e0, e1
    )
    if aux2[0] == 2:  # Prelude.Interfaces.GT
      aux3 = True
    else:
      aux3 = False
    aux1 = aux3
    if not aux1:  # Prelude.Bool.False
      return _idris_Prelude_46_Interfaces_46__123_Prelude_46_Show_46__64_Prelude_46_Interfaces_46_Ord_36_Prec_58__33__62__61__58_0_95_lam0_125_(
        e0, e1
      )
    else:  # Prelude.Bool.True
      return True
    return _idris_error("unreachable due to case in tail position")

# Prelude.Interfaces.Prelude.Show.Prec implementation of Prelude.Interfaces.Ord, method compare
def _idris_Prelude_46_Interfaces_46_Prelude_46_Show_46__64_Prelude_46_Interfaces_46_Ord_36_Prec_58__33_compare_58_0(
  e0, e1
):
  while True:
    if e1[0] == 4:  # Prelude.Show.User
      in0 = e1[1]
      if e0[0] == 4:  # Prelude.Show.User
        in1 = e0[1]
        return _idris_Prelude_46_Interfaces_46_Prelude_46_Nat_46__64_Prelude_46_Interfaces_46_Ord_36_Nat_58__33_compare_58_0(
          in1, in0
        )
      else:
        return _idris_Prelude_46_Interfaces_46_Prelude_46_Interfaces_46__64_Prelude_46_Interfaces_46_Ord_36_Integer_58__33_compare_58_0(
          _idris_Prelude_46_Show_46_precCon(e0),
          _idris_Prelude_46_Show_46_precCon(e1)
        )
      return _idris_error("unreachable due to case in tail position")
    else:
      return _idris_Prelude_46_Interfaces_46_Prelude_46_Interfaces_46__64_Prelude_46_Interfaces_46_Ord_36_Integer_58__33_compare_58_0(
        _idris_Prelude_46_Show_46_precCon(e0),
        _idris_Prelude_46_Show_46_precCon(e1)
      )
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.Prelude.Show.Nat implementation of Prelude.Show.Show, method show
def _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_Nat_58__33_show_58_0(
  e0
):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (65741,), (0,), e0)  # {U_prim__toStrBigInt1}, Prelude.Show.Open

# with block in Prelude.Strings.strM
def _idris__95_Prelude_46_Strings_46_strM_95_with_95_22(e0, e1):
  while True:
    if e1[0] == 1:  # Prelude.Basics.No
      return (0,)  # Prelude.Strings.StrNil
    else:  # Prelude.Basics.Yes
      return (1, e0[0])  # Prelude.Strings.StrCons
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Interfaces.Prelude.Show.Prec implementation of Prelude.Interfaces.Ord, method >
def _idris__95_Prelude_46_Interfaces_46_Prelude_46_Show_46__64_Prelude_46_Interfaces_46_Ord_36_Prec_58__33__62__58_0_95_with_95_27(
  e0, e1, e2
):
  while True:
    if e0[0] == 2:  # Prelude.Interfaces.GT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Show.firstCharIs
def _idris__95_Prelude_46_Show_46_firstCharIs_95_with_95_44(e0, e1, e2):
  while True:
    if e2[0] == 1:  # Prelude.Strings.StrCons
      in0 = e2[1]
      return APPLY0(e0, in0)
    else:  # Prelude.Strings.StrNil
      return False
    return _idris_error("unreachable due to case in tail position")

# constructor of Prelude.Algebra.Monoid#Semigroup ty
def _idris_Prelude_46_Algebra_46_Monoid_95_ictor_35__34_Semigroup_32_ty_34_(e0, e1):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Algebra.Monoid
    in0, in1 = e1[1:]
    return in0
    return _idris_error("unreachable due to case in tail position")

# Python.Exceptions.case block in fromString at ./Python/Exceptions.idr:56:21
def _idris_Python_46_Exceptions_46_fromString_95__95__95__95__95_Python_95__95_Exceptions_95__95_idr_95_56_95_21_95_case(
  e0, e1
):
  while True:
    return {
      u'ArithmeticError': (3,),  # Python.Exceptions.ArithmeticError
      u'AssertionError': (7,),  # Python.Exceptions.AssertionError
      u'AttributeError': (8,),  # Python.Exceptions.AttributeError
      u'BufferError': (2,),  # Python.Exceptions.BufferError
      u'EOFError': (14,),  # Python.Exceptions.EOFError
      u'EnvironmentError': (9,),  # Python.Exceptions.EnvironmentError
      u'FloatingPointError': (4,),  # Python.Exceptions.FloatingPointError
      u'IOError': (10,),  # Python.Exceptions.IOError
      u'ImportError': (15,),  # Python.Exceptions.ImportError
      u'IndentationError': (26,),  # Python.Exceptions.IndentationError
      u'IndexError': (17,),  # Python.Exceptions.IndexError
      u'KeyError': (18,),  # Python.Exceptions.KeyError
      u'LookupError': (16,),  # Python.Exceptions.LookupError
      u'MemoryError': (19,),  # Python.Exceptions.MemoryError
      u'NameError': (20,),  # Python.Exceptions.NameError
      u'NotImplementedError': (24,),  # Python.Exceptions.NotImplementedError
      u'OSError': (11,),  # Python.Exceptions.OSError
      u'OverflowError': (5,),  # Python.Exceptions.OverflowError
      u'ReferenceError': (22,),  # Python.Exceptions.ReferenceError
      u'RuntimeError': (23,),  # Python.Exceptions.RuntimeError
      u'StandardError': (1,),  # Python.Exceptions.StandardError
      u'StopIteration': (0,),  # Python.Exceptions.StopIteration
      u'SyntaxError': (25,),  # Python.Exceptions.SyntaxError
      u'SystemError': (28,),  # Python.Exceptions.SystemError
      u'TabError': (27,),  # Python.Exceptions.TabError
      u'TypeError': (29,),  # Python.Exceptions.TypeError
      u'UnboundLocalError': (21,),  # Python.Exceptions.UnboundLocalError
      u'UnicodeDecodeError': (32,),  # Python.Exceptions.UnicodeDecodeError
      u'UnicodeEncodeError': (33,),  # Python.Exceptions.UnicodeEncodeError
      u'UnicodeError': (31,),  # Python.Exceptions.UnicodeError
      u'UnicodeTranslateError': (34,),  # Python.Exceptions.UnicodeTranslateError
      u'VMSError': (13,),  # Python.Exceptions.VMSError
      u'ValueError': (30,),  # Python.Exceptions.ValueError
      u'WindowsError': (12,),  # Python.Exceptions.WindowsError
      u'ZeroDivisionError': (6,)  # Python.Exceptions.ZeroDivisionError
    }.get(e0, (35,))  # Python.Exceptions.Other

# Python.Exceptions.case block in try at ./Python/Exceptions.idr:106:16
def _idris_Python_46_Exceptions_46_try_95__95__95__95__95_Python_95__95_Exceptions_95__95_idr_95_106_95_16_95_case(
  e0, e1, e2, e3, e4
):
  while True:
    if e3[0] == 0:  # Prelude.Either.Left
      in0 = e3[1]
      return (
        65740,  # {U_io_pure1}
        None,
        None,
        (
          1,  # Python.Exceptions.Except
          _idris_Python_46_Exceptions_46_fromString(
            _idris_Python_46_Fields_46__47__46_(
              None,
              None,
              _idris_Python_46_Fields_46__47__46_(None, None, in0, u'__class__', None),
              u'__name__',
              None
            )
          ),
          in0
        )
      )
    else:  # Prelude.Either.Right
      in1 = e3[1]
      return (65740, None, None, (0, in1))  # {U_io_pure1}, Python.Exceptions.OK
    return _idris_error("unreachable due to case in tail position")

# Python.Exceptions.case block in case block in try at ./Python/Exceptions.idr:106:16 at ./Python/Exceptions.idr:118:10
def _idris_Python_46_Exceptions_46_try_95__95__95__95__95_Python_95__95_Exceptions_95__95_idr_95_106_95_16_95_case_95__95__95__95__95_Python_95__95_Exceptions_95__95_idr_95_118_95_10_95_case(
  e0, e1, e2, e3, e4, e5
):
  while True:
    if e2[0] == 0:  # Prelude.Either.Left
      in0 = e2[1]
      return (
        65740,  # {U_io_pure1}
        None,
        None,
        (
          1,  # Python.Exceptions.Except
          _idris_Python_46_Exceptions_46_fromString(
            _idris_Python_46_Fields_46__47__46_(
              None,
              None,
              _idris_Python_46_Fields_46__47__46_(None, None, in0, u'__class__', None),
              u'__name__',
              None
            )
          ),
          in0
        )
      )
    else:  # Prelude.Either.Right
      in1 = e2[1]
      return (65740, None, None, (0, in1))  # {U_io_pure1}, Python.Exceptions.OK
    return _idris_error("unreachable due to case in tail position")

# Python.Exceptions.case block in catch at ./Python/Exceptions.idr:130:16
def _idris_Python_46_Exceptions_46_catch_95__95__95__95__95_Python_95__95_Exceptions_95__95_idr_95_130_95_16_95_case(
  e0, e1, e2, e3, e4, e5
):
  while True:
    if e4[0] == 1:  # Python.Exceptions.Except
      in0, in1 = e4[1:]
      return APPLY0(APPLY0(e2, in0), in1)
    else:  # Python.Exceptions.OK
      in2 = e4[1]
      return (65740, None, None, in2)  # {U_io_pure1}
    return _idris_error("unreachable due to case in tail position")

# Python.Prim.case block in next at ./Python/Prim.idr:61:11
def _idris_Python_46_Prim_46_next_95__95__95__95__95_Python_95__95_Prim_95__95_idr_95_61_95_11_95_case(
  e0, e1, e2, e3, e4
):
  while True:
    if e3[0] == 1:  # Python.Exceptions.Except
      in0, in1 = e3[1:]
      if in0[0] == 0:  # Python.Exceptions.StopIteration
        return (65740, None, None, None)  # {U_io_pure1}
      else:
        return _idris_Python_46_Exceptions_46_raise(None, in1)
      return _idris_error("unreachable due to case in tail position")
    else:  # Python.Exceptions.OK
      in2 = e3[1]
      return (65740, None, None, in2)  # {U_io_pure1}
    return _idris_error("unreachable due to case in tail position")

# Python.Prim.case block in Python.Prim.iterate, iter at ./Python/Prim.idr:84:17
def _idris_Python_46_Prim_46_Python_46_Prim_46_iterate_58_iter_58_0_95__95__95__95__95_Python_95__95_Prim_95__95_idr_95_84_95_17_95_case(
  e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13
):
  while True:
    if e12 is not None:  # Prelude.Maybe.Just
      in0 = e12
      return APPLY0(
        APPLY0(
          _idris_Prelude_46_Monad_46__62__62__61_(None, e11, None, None),
          APPLY0(APPLY0(e10, e7), in0)
        ),
        (65730, e9, e10)  # {U_Python.Prim.{Python.Prim.iterate:iter:0_____Python__Prim__idr_84_17_case_lam0}1}
      )
    else:  # Prelude.Maybe.Nothing
      return (65740, None, None, e7)  # {U_io_pure1}
    return _idris_error("unreachable due to case in tail position")

# Main.case block in main at ./examples/example.idr:84:32
def _idris_Main_46_main_95__95__95__95__95_examples_95__95_example_95__95_idr_95_84_95_32_95_case(
  e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14,
  e15, e16, e17, e18, e19, e20, e21, e22, e23, e24, e25, e26, e27,
  e28, e29, e30, e31, e32, e33, e34, e35, e36, e37, e38, e39, e40,
  e41, e42, e43, e44
):
  while True:
    if e42[0] == 11:  # Python.Exceptions.OSError
      return _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        ((u'  -> (1) everything\'s fine: ' + _idris_Python_46_Exceptions_46_showException(e43)) + u'\u000a')
      )
    else:
      return _idris_Python_46_Exceptions_46_raise(None, e43)
    return _idris_error("unreachable due to case in tail position")

# Main.case block in main at ./examples/example.idr:90:13
def _idris_Main_46_main_95__95__95__95__95_examples_95__95_example_95__95_idr_95_90_95_13_95_case(
  e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14,
  e15, e16, e17, e18, e19, e20, e21, e22, e23, e24, e25, e26, e27,
  e28, e29, e30, e31, e32, e33, e34, e35, e36, e37, e38, e39, e40,
  e41, e42, e43, e44, e45
):
  while True:
    if e44[0] == 1:  # Python.Exceptions.Except
      in0, in1 = e44[1:]
      if in0[0] == 11:  # Python.Exceptions.OSError
        return _idris_Prelude_46_Interactive_46_putStr_39_(
          None,
          ((u'  -> (2) everything\'s fine: ' + _idris_Python_46_Exceptions_46_showException(in1)) + u'\u000a')
        )
      else:
        return _idris_Python_46_Exceptions_46_raise(None, in1)
      return _idris_error("unreachable due to case in tail position")
    else:  # Python.Exceptions.OK
      in2 = e44[1]
      return _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        u'Your root could probably use some security lessons!\u000a'
      )
    return _idris_error("unreachable due to case in tail position")

# case block in io_bind at IO.idr:107:34
def _idris_io_95_bind_95_IO_95__95_idr_95_107_95_34_95_case(
  e0, e1, e2, e3, e4, e5, e6, e7
):
  while True:
    return APPLY0(e7, e5)

# case block in Void at (casefun Void)
def _idris_Void_95__95__95_casefun_95__95_Void_95__95__95_case():
  while True:
    return None

# <<Void eliminator>>
def _idris_Void_95_elim():
  while True:
    return None

# export: Main.exports, greet
def greet(arg1):
  APPLY0(_idris_Main_46_exports_58_greet_58_0(arg1), World)

if __name__ == '__main__':
  runMain0()
