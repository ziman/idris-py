#!/usr/bin/env python

import sys
import importlib

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
    return _idris_Python_46_Functions_46_call(None, None, e2, e3, None, e5)

# Python.Functions.$:
def _idris_Python_46_Functions_46__36__58_(e0, e1, e2, e3):
  while True:
    return (65801, None, None, None, e2, (65769, e1, e3))  # {U_io_bind1}, {U_Python.Functions.{$:0}1}

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
    return _idris_unsafePerformIO(None, None, (65767, e2, e3))  # {U_Python.Fields.{/.0}1}

# Python.Fields./:
def _idris_Python_46_Fields_46__47__58_(e0, e1, e2, e3, e4):
  while True:
    return _idris_Prelude_46_Functor_46_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65768, e3),  # {U_Python.Fields.{/:0}1}
      e2
    )

# Prelude.Algebra.<+>
def _idris_Prelude_46_Algebra_46__60__43__62_(e0, e1):
  while True:
    return e1

# Prelude.Classes.==
def _idris_Prelude_46_Classes_46__61__61_(e0, e1):
  while True:
    return e1

# Prelude.Classes.>
def _idris_Prelude_46_Classes_46__62_(e0, e1):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Classes.Ord
    in0, in1 = e1[1:]
    return in1
    return _idris_error("unreachable due to case in tail position")

# Prelude.Monad.>>=
def _idris_Prelude_46_Monad_46__62__62__61_(e0, e1, e2, e3):
  while True:
    assert e3[0] == 0  # constructor of Prelude.Monad.Monad
    in0, in1 = e3[1:]
    return APPLY0(APPLY0(in1, e1), e2)
    return _idris_error("unreachable due to case in tail position")

# @@constructor of Prelude.Algebra.Monoid#Semigroup a
def _idris__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a(
  e0, e1
):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Algebra.Monoid
    in0, in1 = e1[1:]
    return in0
    return _idris_error("unreachable due to case in tail position")

# @@constructor of Prelude.Monad.Monad#Applicative m
def _idris__64__64_constructor_32_of_32_Prelude_46_Monad_46_Monad_35_Applicative_32_m(
  e0, e1
):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Monad.Monad
    in0, in1 = e1[1:]
    return in0
    return _idris_error("unreachable due to case in tail position")

# Force
def _idris_Force(e0, e1, e2):
  while True:
    in0 = EVAL0(e2)
    return in0

# Python.Functions.call
def _idris_Python_46_Functions_46_call(e0, e1, e2, e3, e4, e5):
  while True:
    return _idris_Prelude_46_Functor_46_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65771, None),  # {U_Python.IO.unRaw1}
      (65770, e3, e2, e5)  # {U_Python.Functions.{call0}1}
    )

# call__IO
def _idris_call_95__95_IO(e0, e1, e2):
  while True:
    return APPLY0(e2, None)

# Python.Exceptions.catch
def _idris_Python_46_Exceptions_46_catch(e0, e1, e2):
  while True:
    return (65801, None, None, None, e1, (65746, e2))  # {U_io_bind1}, {U_Python.Exceptions.{catch6}1}

# Python.Prim.collect
def _idris_Python_46_Prim_46_collect(e0, e1, e2, e3):
  while True:
    return _idris_Prelude_46_Functor_46_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65728, None, ConsList()),  # {U_Prelude.List.reverse, reverse'1}
      _idris_Python_46_Prim_46_foreach(
        None,
        None,
        None,
        e2,
        ConsList(),
        (65782,),  # {U_Python.Prim.{collect1}1}
        None
      )
    )

# Prelude.Classes.compare
def _idris_Prelude_46_Classes_46_compare(e0, e1):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Classes.Ord
    in0, in1 = e1[1:]
    return in0
    return _idris_error("unreachable due to case in tail position")

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
      APPLY0(_idris_Prelude_46_Foldable_46_foldr(None, None, None, e2), aux1),
      aux2
    )

# Prelude.Foldable.foldr
def _idris_Prelude_46_Foldable_46_foldr(e0, e1, e2, e3):
  while True:
    return APPLY0(APPLY0(e3, e1), e2)

# Python.Prim.foreach
def _idris_Python_46_Prim_46_foreach(e0, e1, e2, e3, e4, e5, e6):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Functions_46_call(
        None,
        None,
        (0,),  # Python.Telescope.Return
        _idris_Python_46_Fields_46__47__46_(None, None, e3, "__iter__", None),
        None,
        Unit
      ),
      (65784, e3, e4, e5)  # {U_Python.Prim.{foreach1}1}
    )

# Python.Lib.Threading.forkPIO
def _idris_Python_46_Lib_46_Threading_46_forkPIO(e0, e1):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Functions_46__36__58_(
        None,
        (1, (1,), (65773,)),  # Python.Telescope.Bind, Python.Telescope.Forall, {U_Python.Lib.Threading.{forkPIO1}1}
        _idris_Python_46_Fields_46__47__58_(
          None,
          None,
          (65799, None, "Queue"),  # {U_Python.importModule1}
          "Queue",
          None
        ),
        (0, (0,), (0, 1, Unit))  # Builtins.MkSigma, Data.Erased.Erase, Builtins.MkSigma
      ),
      (65778, e1)  # {U_Python.Lib.Threading.{forkPIO6}1}
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

# Prelude.Classes.intToBool
def _idris_Prelude_46_Classes_46_intToBool(e0):
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

# io_return
def _idris_io_95_return(e0, e1, e2, _idris_w):
  while True:
    return e2

# Python.Prim.iterate
def _idris_Python_46_Prim_46_iterate(e0, e1, e2, e3, e4, e5, e6):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Functions_46_call(
        None,
        None,
        (0,),  # Python.Telescope.Return
        _idris_Python_46_Fields_46__47__46_(None, None, e3, "__iter__", None),
        None,
        Unit
      ),
      (65785, e4, e5)  # {U_Python.Prim.{iterate0}1}
    )

# Prelude.Strings.length
def _idris_Prelude_46_Strings_46_length():
  while True:
    return (
      65723,  # {U_Prelude.Basics..1}
      None,
      None,
      None,
      (65723, None, None, None, (65739,), (65805,)),  # {U_Prelude.Basics..1}, {U_Prelude.Strings.{length0}1}, {U_prim__zextInt_BigInt1}
      (65806,)  # {U_prim_lenString1}
    )

# Main.main
def _idris_Main_46_main():
  while True:
    return (65801, None, None, None, (65799, None, "requests"), (65718,))  # {U_io_bind1}, {U_Python.importModule1}, {U_Main.{main69}1}

# Python.marshalPIO
def _idris_Python_46_marshalPIO(e0, e1):
  while True:
    return _idris_unsafePerformIO(None, None, (65800, e1))  # {U_Python.{marshalPIO0}1}

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
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Exceptions_46_try(
        None,
        _idris_Python_46_Functions_46_call(
          None,
          None,
          (0,),  # Python.Telescope.Return
          _idris_Python_46_Fields_46__47__46_(None, None, e1, "next", None),
          None,
          Unit
        )
      ),
      (65789,)  # {U_Python.Prim.{next12}1}
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
    aux2 = _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33__62__61__58_0(
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
      return ("(" + (in0 + ")"))
    return _idris_error("unreachable due to case in tail position")

# prim__addInt
def _idris_prim_95__95_addInt(op0, op1):
  while True:
    return (op0 + op1)

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

# prim__readFile
def _idris_prim_95__95_readFile(op0, op1):
  while True:
    return _idris_error("unimplemented external: prim__readFile")

# prim__registerPtr
def _idris_prim_95__95_registerPtr(op0, op1):
  while True:
    return _idris_error("unimplemented external: prim__registerPtr")

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
def _idris_prim_95__95_vm():
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

# Prelude.Applicative.pure
def _idris_Prelude_46_Applicative_46_pure(e0, e1, e2):
  while True:
    return APPLY0(e2, e1)

# Prelude.Interactive.putStr'
def _idris_Prelude_46_Interactive_46_putStr_39_(e0, e1):
  while True:
    return (65801, None, None, None, (65725, e1), (65726,))  # {U_io_bind1}, {U_Prelude.Interactive.{putStr'0}1}, {U_Prelude.Interactive.{putStr'1}1}

# Python.Exceptions.raise
def _idris_Python_46_Exceptions_46_raise(e0, e1):
  while True:
    return _idris_Prelude_46_Functor_46_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65771, None),  # {U_Python.IO.unRaw1}
      (65747, e1)  # {U_Python.Exceptions.{raise0}1}
    )

# really_believe_me
def _idris_really_95_believe_95_me(e0, e1, e2):
  while True:
    return e2

# run__IO
def _idris_run_95__95_IO(e0, e1):
  while True:
    return APPLY0(e1, None)

# Python.Exceptions.showException
def _idris_Python_46_Exceptions_46_showException(e0):
  while True:
    return _idris_unsafePerformIO(None, None, (65748, e0))  # {U_Python.Exceptions.{showException0}1}

# Prelude.Show.showParens
def _idris_Prelude_46_Show_46_showParens(e0, e1):
  while True:
    if not e0:  # Prelude.Bool.False
      return e1
    else:  # Prelude.Bool.True
      return ("(" + (e1 + ")"))
    return _idris_error("unreachable due to case in tail position")

# Prelude.Strings.strM
def _idris_Prelude_46_Strings_46_strM(e0):
  while True:
    aux3 = (e0 == "")
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
      return _idris_really_95_believe_95_me(None, None, (0,))  # Prelude.Strings.StrNil
    else:  # Prelude.Basics.Yes
      return _idris_really_95_believe_95_me(None, None, (1, e0[0]))  # Prelude.Strings.StrCons
    return _idris_error("unreachable due to case in tail position")

# Python.Telescope.strip
def _idris_Python_46_Telescope_46_strip(e0, e1, e2):
  while True:
    if e1[0] == 1:  # Python.Telescope.Bind
      in0, in1 = e1[1:]
      if in0[0] == 2:  # Python.Telescope.Default
        in2 = in0[1]
        assert e2[0] == 0  # Builtins.MkSigma
        in3, in4 = e2[1:]
        if in3 is not None:  # Prelude.Maybe.Just
          in5 = in3
          aux1 = in5
        else:  # Prelude.Maybe.Nothing
          aux1 = _idris_Python_46_Telescope_46__123_strip0_125_(in2)
        return _idris_Python_46_Telescope_46_strip(None, APPLY0(in1, aux1), in4).cons(in3)
        return _idris_error("unreachable due to case in tail position")
      elif in0[0] == 1:  # Python.Telescope.Forall
        assert e2[0] == 0  # Builtins.MkSigma
        in6, in7 = e2[1:]
        return _idris_Python_46_Telescope_46_strip(None, APPLY0(in1, in6), in7)
        return _idris_error("unreachable due to case in tail position")
      else:  # Python.Telescope.Pi
        assert e2[0] == 0  # Builtins.MkSigma
        in8, in9 = e2[1:]
        return _idris_Python_46_Telescope_46_strip(None, APPLY0(in1, in8), in9).cons(in8)
        return _idris_error("unreachable due to case in tail position")
      return _idris_error("unreachable due to case in tail position")
    else:  # Python.Telescope.Return
      return ConsList()
    return _idris_error("unreachable due to case in tail position")

# Python.Exceptions.try
def _idris_Python_46_Exceptions_46_try(e0, e1):
  while True:
    return (65801, None, None, None, (65761, e1), (65757,))  # {U_io_bind1}, {U_Python.Exceptions.{try4}1}, {U_Python.Exceptions.{try17}1}

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
    return _idris_Python_46_Functions_46_call(
      None,
      None,
      (1, (0,), (65779,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Threading.{wait0}1}
      _idris_Python_46_Fields_46__47__46_(None, None, e1, "get", None),
      None,
      (0, 1, Unit)  # Builtins.MkSigma
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

# Python.Functions.{$:0}
def _idris_Python_46_Functions_46__123__36__58_0_125_(e1, e3, in0):
  while True:
    return _idris_Python_46_Functions_46_call(None, None, e1, in0, None, e3)

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
    if fn0[0] < 65740:
      if fn0[0] < 65694:
        if fn0[0] < 65671:
          if fn0[0] < 65660:
            if fn0[0] < 65654:
              if fn0[0] < 65651:
                if fn0[0] == 65649:  # {U_Main.{case block in main at example.idr:59:44_lam0}1}
                  return _idris_Main_46__123_case_32_block_32_in_32_main_32_at_32_example_46_idr_58_59_58_44_95_lam0_125_(
                    arg0
                  )
                else:  # {U_Main.{case block in main at example.idr:59:44_lam1}1}
                  P_c0, P_c1 = fn0[1:]
                  return _idris_Main_46__123_case_32_block_32_in_32_main_32_at_32_example_46_idr_58_59_58_44_95_lam1_125_(
                    P_c0, P_c1, arg0
                  )
              else:
                if fn0[0] == 65651:  # {U_Main.{case block in main at example.idr:59:44_lam2}1}
                  P_c0, P_c1 = fn0[1:]
                  return _idris_Main_46__123_case_32_block_32_in_32_main_32_at_32_example_46_idr_58_59_58_44_95_lam2_125_(
                    P_c0, P_c1, arg0
                  )
                elif fn0[0] == 65652:  # {U_Main.{case block in main at example.idr:59:44_lam3}1}
                  P_c0, P_c1, P_c2 = fn0[1:]
                  return _idris_Main_46__123_case_32_block_32_in_32_main_32_at_32_example_46_idr_58_59_58_44_95_lam3_125_(
                    P_c0, P_c1, P_c2, arg0
                  )
                else:  # {U_Main.{main0}1}
                  return _idris_Main_46__123_main0_125_(arg0)
            else:
              if fn0[0] < 65657:
                if fn0[0] == 65654:  # {U_Main.{main10}1}
                  return _idris_Main_46__123_main10_125_(arg0)
                elif fn0[0] == 65655:  # {U_Main.{main11}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main11_125_(P_c0, arg0)
                else:  # {U_Main.{main12}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main12_125_(P_c0, arg0)
              else:
                if fn0[0] == 65657:  # {U_Main.{main13}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main13_125_(P_c0, arg0)
                elif fn0[0] == 65658:  # {U_Main.{main14}1}
                  return _idris_Main_46__123_main14_125_(arg0)
                else:  # {U_Main.{main15}1}
                  return _idris_Main_46__123_main15_125_(arg0)
          else:
            if fn0[0] < 65665:
              if fn0[0] < 65662:
                if fn0[0] == 65660:  # {U_Main.{main16}1}
                  return _idris_Main_46__123_main16_125_(arg0)
                else:  # {U_Main.{main17}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main17_125_(P_c0, arg0)
              else:
                if fn0[0] == 65662:  # {U_Main.{main18}1}
                  return _idris_Main_46__123_main18_125_(arg0)
                elif fn0[0] == 65663:  # {U_Main.{main19}1}
                  return _idris_Main_46__123_main19_125_(arg0)
                else:  # {U_Main.{main1}1}
                  return _idris_Main_46__123_main1_125_(arg0)
            else:
              if fn0[0] < 65668:
                if fn0[0] == 65665:  # {U_Main.{main20}1}
                  return _idris_Main_46__123_main20_125_(arg0)
                elif fn0[0] == 65666:  # {U_Main.{main21}1}
                  return _idris_Main_46__123_main21_125_(arg0)
                else:  # {U_Main.{main22}1}
                  return _idris_Main_46__123_main22_125_(arg0)
              else:
                if fn0[0] == 65668:  # {U_Main.{main23}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main23_125_(P_c0, arg0)
                elif fn0[0] == 65669:  # {U_Main.{main24}1}
                  return _idris_Main_46__123_main24_125_(arg0)
                else:  # {U_Main.{main25}1}
                  return _idris_Main_46__123_main25_125_(arg0)
        else:
          if fn0[0] < 65682:
            if fn0[0] < 65676:
              if fn0[0] < 65673:
                if fn0[0] == 65671:  # {U_Main.{main26}1}
                  return _idris_Main_46__123_main26_125_(arg0)
                else:  # {U_Main.{main27}1}
                  return _idris_Main_46__123_main27_125_(arg0)
              else:
                if fn0[0] == 65673:  # {U_Main.{main28}1}
                  return _idris_Main_46__123_main28_125_(arg0)
                elif fn0[0] == 65674:  # {U_Main.{main29}1}
                  return _idris_Main_46__123_main29_125_(arg0)
                else:  # {U_Main.{main2}1}
                  return _idris_Main_46__123_main2_125_(arg0)
            else:
              if fn0[0] < 65679:
                if fn0[0] == 65676:  # {U_Main.{main30}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main30_125_(P_c0, arg0)
                elif fn0[0] == 65677:  # {U_Main.{main31}1}
                  return _idris_Main_46__123_main31_125_(arg0)
                else:  # {U_Main.{main32}1}
                  return _idris_Main_46__123_main32_125_(arg0)
              else:
                if fn0[0] == 65679:  # {U_Main.{main33}1}
                  return _idris_Main_46__123_main33_125_(arg0)
                elif fn0[0] == 65680:  # {U_Main.{main34}1}
                  return _idris_Main_46__123_main34_125_(arg0)
                else:  # {U_Main.{main35}1}
                  return _idris_Main_46__123_main35_125_(arg0)
          else:
            if fn0[0] < 65688:
              if fn0[0] < 65685:
                if fn0[0] == 65682:  # {U_Main.{main36}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main36_125_(P_c0, arg0)
                elif fn0[0] == 65683:  # {U_Main.{main37}1}
                  return _idris_Main_46__123_main37_125_(arg0)
                else:  # {U_Main.{main38}1}
                  return _idris_Main_46__123_main38_125_(arg0)
              else:
                if fn0[0] == 65685:  # {U_Main.{main39}1}
                  return _idris_Main_46__123_main39_125_(arg0)
                elif fn0[0] == 65686:  # {U_Main.{main3}1}
                  return _idris_Main_46__123_main3_125_(arg0)
                else:  # {U_Main.{main40}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main40_125_(P_c0, arg0)
            else:
              if fn0[0] < 65691:
                if fn0[0] == 65688:  # {U_Main.{main41}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main41_125_(P_c0, arg0)
                elif fn0[0] == 65689:  # {U_Main.{main42}1}
                  P_c0, P_c1 = fn0[1:]
                  return _idris_Main_46__123_main42_125_(P_c0, P_c1, arg0)
                else:  # {U_Main.{main43}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main43_125_(P_c0, arg0)
              else:
                if fn0[0] == 65691:  # {U_Main.{main44}1}
                  return _idris_Main_46__123_main44_125_(arg0)
                elif fn0[0] == 65692:  # {U_Main.{main45}1}
                  return _idris_Main_46__123_main45_125_(arg0)
                else:  # {U_Main.{main46}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main46_125_(P_c0, arg0)
      else:
        if fn0[0] < 65717:
          if fn0[0] < 65705:
            if fn0[0] < 65699:
              if fn0[0] < 65696:
                if fn0[0] == 65694:  # {U_Main.{main47}1}
                  return _idris_Main_46__123_main47_125_(arg0)
                else:  # {U_Main.{main48}1}
                  return _idris_Main_46__123_main48_125_(arg0)
              else:
                if fn0[0] == 65696:  # {U_Main.{main49}1}
                  return _idris_Main_46__123_main49_125_(arg0)
                elif fn0[0] == 65697:  # {U_Main.{main4}1}
                  P_c0, P_c1 = fn0[1:]
                  return _idris_Main_46__123_main4_125_(P_c0, P_c1, arg0)
                else:  # {U_Main.{main50}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main50_125_(P_c0, arg0)
            else:
              if fn0[0] < 65702:
                if fn0[0] == 65699:  # {U_Main.{main51}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main51_125_(P_c0, arg0)
                elif fn0[0] == 65700:  # {U_Main.{main52}1}
                  return _idris_Main_46__123_main52_125_(arg0)
                else:  # {U_Main.{main53}1}
                  return _idris_Main_46__123_main53_125_(arg0)
              else:
                if fn0[0] == 65702:  # {U_Main.{main54}1}
                  return _idris_Main_46__123_main54_125_(arg0)
                elif fn0[0] == 65703:  # {U_Main.{main55}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main55_125_(P_c0, arg0)
                else:  # {U_Main.{main56}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main56_125_(P_c0, arg0)
          else:
            if fn0[0] < 65711:
              if fn0[0] < 65708:
                if fn0[0] == 65705:  # {U_Main.{main57}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main57_125_(P_c0, arg0)
                elif fn0[0] == 65706:  # {U_Main.{main58}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main58_125_(P_c0, arg0)
                else:  # {U_Main.{main59}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main59_125_(P_c0, arg0)
              else:
                if fn0[0] == 65708:  # {U_Main.{main5}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main5_125_(P_c0, arg0)
                elif fn0[0] == 65709:  # {U_Main.{main60}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main60_125_(P_c0, arg0)
                else:  # {U_Main.{main61}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main61_125_(P_c0, arg0)
            else:
              if fn0[0] < 65714:
                if fn0[0] == 65711:  # {U_Main.{main62}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main62_125_(P_c0, arg0)
                elif fn0[0] == 65712:  # {U_Main.{main63}1}
                  P_c0, P_c1 = fn0[1:]
                  return _idris_Main_46__123_main63_125_(P_c0, P_c1, arg0)
                else:  # {U_Main.{main64}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main64_125_(P_c0, arg0)
              else:
                if fn0[0] == 65714:  # {U_Main.{main65}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main65_125_(P_c0, arg0)
                elif fn0[0] == 65715:  # {U_Main.{main66}1}
                  P_c0, P_c1 = fn0[1:]
                  return _idris_Main_46__123_main66_125_(P_c0, P_c1, arg0)
                else:  # {U_Main.{main67}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main67_125_(P_c0, arg0)
        else:
          if fn0[0] < 65728:
            if fn0[0] < 65722:
              if fn0[0] < 65719:
                if fn0[0] == 65717:  # {U_Main.{main68}1}
                  return _idris_Main_46__123_main68_125_(arg0)
                else:  # {U_Main.{main69}1}
                  return _idris_Main_46__123_main69_125_(arg0)
              else:
                if fn0[0] == 65719:  # {U_Main.{main6}1}
                  return _idris_Main_46__123_main6_125_(arg0)
                elif fn0[0] == 65720:  # {U_Main.{main7}1}
                  return _idris_Main_46__123_main7_125_(arg0)
                else:  # {U_Main.{main8}1}
                  return _idris_Main_46__123_main8_125_(arg0)
            else:
              if fn0[0] < 65725:
                if fn0[0] == 65722:  # {U_Main.{main9}1}
                  P_c0 = fn0[1]
                  return _idris_Main_46__123_main9_125_(P_c0, arg0)
                elif fn0[0] == 65723:  # {U_Prelude.Basics..1}
                  P_c0, P_c1, P_c2, P_c3, P_c4 = fn0[1:]
                  return _idris_Prelude_46_Basics_46__46_(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
                else:  # {U_Prelude.Functor.{Prelude.Monad.IO' ffi instance of Prelude.Functor.Functor, method map_lam0}1}
                  P_c0 = fn0[1]
                  return _idris_Prelude_46_Functor_46__123_Prelude_46_Monad_46_IO_39__32_ffi_32_instance_32_of_32_Prelude_46_Functor_46_Functor_44__32_method_32_map_95_lam0_125_(
                    P_c0, arg0
                  )
              else:
                if fn0[0] == 65725:  # {U_Prelude.Interactive.{putStr'0}1}
                  P_c0 = fn0[1]
                  return _idris_Prelude_46_Interactive_46__123_putStr_39_0_125_(P_c0, arg0)
                elif fn0[0] == 65726:  # {U_Prelude.Interactive.{putStr'1}1}
                  return _idris_Prelude_46_Interactive_46__123_putStr_39_1_125_(arg0)
                else:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable1}
                  P_c0, P_c1, P_c2, P_c3 = fn0[1:]
                  return _idris_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List(
                    P_c0, P_c1, P_c2, P_c3, arg0
                  )
          else:
            if fn0[0] < 65734:
              if fn0[0] < 65731:
                if fn0[0] == 65728:  # {U_Prelude.List.reverse, reverse'1}
                  P_c0, P_c1 = fn0[1:]
                  return _idris_Prelude_46_List_46_reverse_58_reverse_39__58_0(P_c0, P_c1, arg0)
                elif fn0[0] == 65729:  # {U_Prelude.Nat.Nat instance of Prelude.Classes.Eq1}
                  P_c0 = fn0[1]
                  return _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Eq_36_Nat(P_c0, arg0)
                else:  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam0}1}
                  P_c0 = fn0[1]
                  return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
                    P_c0, arg0
                  )
              else:
                if fn0[0] == 65731:  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam1}1}
                  return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
                    arg0
                  )
                elif fn0[0] == 65732:  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam2}1}
                  P_c0 = fn0[1]
                  return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
                    P_c0, arg0
                  )
                else:  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam3}1}
                  return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
                    arg0
                  )
            else:
              if fn0[0] < 65737:
                if fn0[0] == 65734:  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam0}1}
                  P_c0 = fn0[1]
                  return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
                    P_c0, arg0
                  )
                elif fn0[0] == 65735:  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam1}1}
                  return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
                    arg0
                  )
                else:  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam2}1}
                  P_c0 = fn0[1]
                  return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
                    P_c0, arg0
                  )
              else:
                if fn0[0] == 65737:  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam3}1}
                  return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
                    arg0
                  )
                elif fn0[0] == 65738:  # {U_Prelude.Show.{primNumShow0}1}
                  return _idris_Prelude_46_Show_46__123_primNumShow0_125_(arg0)
                else:  # {U_Prelude.Strings.{length0}1}
                  return _idris_Prelude_46_Strings_46__123_length0_125_(arg0)
    else:
      if fn0[0] < 65785:
        if fn0[0] < 65762:
          if fn0[0] < 65751:
            if fn0[0] < 65745:
              if fn0[0] < 65742:
                if fn0[0] == 65740:  # {U_Python.Exceptions.{catch0}1}
                  return _idris_Python_46_Exceptions_46__123_catch0_125_(arg0)
                else:  # {U_Python.Exceptions.{catch1}1}
                  return _idris_Python_46_Exceptions_46__123_catch1_125_(arg0)
              else:
                if fn0[0] == 65742:  # {U_Python.Exceptions.{catch2}1}
                  P_c0 = fn0[1]
                  return _idris_Python_46_Exceptions_46__123_catch2_125_(P_c0, arg0)
                elif fn0[0] == 65743:  # {U_Python.Exceptions.{catch3}1}
                  return _idris_Python_46_Exceptions_46__123_catch3_125_(arg0)
                else:  # {U_Python.Exceptions.{catch4}1}
                  return _idris_Python_46_Exceptions_46__123_catch4_125_(arg0)
            else:
              if fn0[0] < 65748:
                if fn0[0] == 65745:  # {U_Python.Exceptions.{catch5}1}
                  return _idris_Python_46_Exceptions_46__123_catch5_125_(arg0)
                elif fn0[0] == 65746:  # {U_Python.Exceptions.{catch6}1}
                  P_c0 = fn0[1]
                  return _idris_Python_46_Exceptions_46__123_catch6_125_(P_c0, arg0)
                else:  # {U_Python.Exceptions.{raise0}1}
                  P_c0 = fn0[1]
                  return _idris_Python_46_Exceptions_46__123_raise0_125_(P_c0, arg0)
              else:
                if fn0[0] == 65748:  # {U_Python.Exceptions.{showException0}1}
                  P_c0 = fn0[1]
                  return _idris_Python_46_Exceptions_46__123_showException0_125_(P_c0, arg0)
                elif fn0[0] == 65749:  # {U_Python.Exceptions.{try0}1}
                  return _idris_Python_46_Exceptions_46__123_try0_125_(arg0)
                else:  # {U_Python.Exceptions.{try10}1}
                  return _idris_Python_46_Exceptions_46__123_try10_125_(arg0)
          else:
            if fn0[0] < 65756:
              if fn0[0] < 65753:
                if fn0[0] == 65751:  # {U_Python.Exceptions.{try11}1}
                  return _idris_Python_46_Exceptions_46__123_try11_125_(arg0)
                else:  # {U_Python.Exceptions.{try12}1}
                  return _idris_Python_46_Exceptions_46__123_try12_125_(arg0)
              else:
                if fn0[0] == 65753:  # {U_Python.Exceptions.{try13}1}
                  P_c0 = fn0[1]
                  return _idris_Python_46_Exceptions_46__123_try13_125_(P_c0, arg0)
                elif fn0[0] == 65754:  # {U_Python.Exceptions.{try14}1}
                  return _idris_Python_46_Exceptions_46__123_try14_125_(arg0)
                else:  # {U_Python.Exceptions.{try15}1}
                  return _idris_Python_46_Exceptions_46__123_try15_125_(arg0)
            else:
              if fn0[0] < 65759:
                if fn0[0] == 65756:  # {U_Python.Exceptions.{try16}1}
                  return _idris_Python_46_Exceptions_46__123_try16_125_(arg0)
                elif fn0[0] == 65757:  # {U_Python.Exceptions.{try17}1}
                  return _idris_Python_46_Exceptions_46__123_try17_125_(arg0)
                else:  # {U_Python.Exceptions.{try1}1}
                  return _idris_Python_46_Exceptions_46__123_try1_125_(arg0)
              else:
                if fn0[0] == 65759:  # {U_Python.Exceptions.{try2}1}
                  return _idris_Python_46_Exceptions_46__123_try2_125_(arg0)
                elif fn0[0] == 65760:  # {U_Python.Exceptions.{try3}1}
                  return _idris_Python_46_Exceptions_46__123_try3_125_(arg0)
                else:  # {U_Python.Exceptions.{try4}1}
                  P_c0 = fn0[1]
                  return _idris_Python_46_Exceptions_46__123_try4_125_(P_c0, arg0)
        else:
          if fn0[0] < 65773:
            if fn0[0] < 65767:
              if fn0[0] < 65764:
                if fn0[0] == 65762:  # {U_Python.Exceptions.{try5}1}
                  return _idris_Python_46_Exceptions_46__123_try5_125_(arg0)
                else:  # {U_Python.Exceptions.{try6}1}
                  return _idris_Python_46_Exceptions_46__123_try6_125_(arg0)
              else:
                if fn0[0] == 65764:  # {U_Python.Exceptions.{try7}1}
                  P_c0 = fn0[1]
                  return _idris_Python_46_Exceptions_46__123_try7_125_(P_c0, arg0)
                elif fn0[0] == 65765:  # {U_Python.Exceptions.{try8}1}
                  return _idris_Python_46_Exceptions_46__123_try8_125_(arg0)
                else:  # {U_Python.Exceptions.{try9}1}
                  return _idris_Python_46_Exceptions_46__123_try9_125_(arg0)
            else:
              if fn0[0] < 65770:
                if fn0[0] == 65767:  # {U_Python.Fields.{/.0}1}
                  P_c0, P_c1 = fn0[1:]
                  return _idris_Python_46_Fields_46__123__47__46_0_125_(P_c0, P_c1, arg0)
                elif fn0[0] == 65768:  # {U_Python.Fields.{/:0}1}
                  P_c0 = fn0[1]
                  return _idris_Python_46_Fields_46__123__47__58_0_125_(P_c0, arg0)
                else:  # {U_Python.Functions.{$:0}1}
                  P_c0, P_c1 = fn0[1:]
                  return _idris_Python_46_Functions_46__123__36__58_0_125_(P_c0, P_c1, arg0)
              else:
                if fn0[0] == 65770:  # {U_Python.Functions.{call0}1}
                  P_c0, P_c1, P_c2 = fn0[1:]
                  return _idris_Python_46_Functions_46__123_call0_125_(P_c0, P_c1, P_c2, arg0)
                elif fn0[0] == 65771:  # {U_Python.IO.unRaw1}
                  P_c0 = fn0[1]
                  return _idris_Python_46_IO_46_unRaw(P_c0, arg0)
                else:  # {U_Python.Lib.Threading.{forkPIO0}1}
                  return _idris_Python_46_Lib_46_Threading_46__123_forkPIO0_125_(arg0)
          else:
            if fn0[0] < 65779:
              if fn0[0] < 65776:
                if fn0[0] == 65773:  # {U_Python.Lib.Threading.{forkPIO1}1}
                  return _idris_Python_46_Lib_46_Threading_46__123_forkPIO1_125_(arg0)
                elif fn0[0] == 65774:  # {U_Python.Lib.Threading.{forkPIO2}1}
                  return _idris_Python_46_Lib_46_Threading_46__123_forkPIO2_125_(arg0)
                else:  # {U_Python.Lib.Threading.{forkPIO3}1}
                  return _idris_Python_46_Lib_46_Threading_46__123_forkPIO3_125_(arg0)
              else:
                if fn0[0] == 65776:  # {U_Python.Lib.Threading.{forkPIO4}1}
                  P_c0 = fn0[1]
                  return _idris_Python_46_Lib_46_Threading_46__123_forkPIO4_125_(P_c0, arg0)
                elif fn0[0] == 65777:  # {U_Python.Lib.Threading.{forkPIO5}1}
                  P_c0 = fn0[1]
                  return _idris_Python_46_Lib_46_Threading_46__123_forkPIO5_125_(P_c0, arg0)
                else:  # {U_Python.Lib.Threading.{forkPIO6}1}
                  P_c0 = fn0[1]
                  return _idris_Python_46_Lib_46_Threading_46__123_forkPIO6_125_(P_c0, arg0)
            else:
              if fn0[0] < 65782:
                if fn0[0] == 65779:  # {U_Python.Lib.Threading.{wait0}1}
                  return _idris_Python_46_Lib_46_Threading_46__123_wait0_125_(arg0)
                elif fn0[0] == 65780:  # {U_Python.Prim.{case block in Python.Prim.iterate, iter at ./Python/Prim.idr:84:17_lam0}1}
                  P_c0, P_c1 = fn0[1:]
                  return _idris_Python_46_Prim_46__123_case_32_block_32_in_32_Python_46_Prim_46_iterate_44__32_iter_32_at_32__46__47_Python_47_Prim_46_idr_58_84_58_17_95_lam0_125_(
                    P_c0, P_c1, arg0
                  )
                else:  # {U_Python.Prim.{collect0}1}
                  P_c0 = fn0[1]
                  return _idris_Python_46_Prim_46__123_collect0_125_(P_c0, arg0)
              else:
                if fn0[0] == 65782:  # {U_Python.Prim.{collect1}1}
                  return _idris_Python_46_Prim_46__123_collect1_125_(arg0)
                elif fn0[0] == 65783:  # {U_Python.Prim.{foreach0}1}
                  P_c0, P_c1, P_c2 = fn0[1:]
                  return _idris_Python_46_Prim_46__123_foreach0_125_(P_c0, P_c1, P_c2, arg0)
                else:  # {U_Python.Prim.{foreach1}1}
                  P_c0, P_c1, P_c2 = fn0[1:]
                  return _idris_Python_46_Prim_46__123_foreach1_125_(P_c0, P_c1, P_c2, arg0)
      else:
        if fn0[0] < 65808:
          if fn0[0] < 65796:
            if fn0[0] < 65790:
              if fn0[0] < 65787:
                if fn0[0] == 65785:  # {U_Python.Prim.{iterate0}1}
                  P_c0, P_c1 = fn0[1:]
                  return _idris_Python_46_Prim_46__123_iterate0_125_(P_c0, P_c1, arg0)
                else:  # {U_Python.Prim.{next0}1}
                  return _idris_Python_46_Prim_46__123_next0_125_(arg0)
              else:
                if fn0[0] == 65787:  # {U_Python.Prim.{next10}1}
                  return _idris_Python_46_Prim_46__123_next10_125_(arg0)
                elif fn0[0] == 65788:  # {U_Python.Prim.{next11}1}
                  return _idris_Python_46_Prim_46__123_next11_125_(arg0)
                else:  # {U_Python.Prim.{next12}1}
                  return _idris_Python_46_Prim_46__123_next12_125_(arg0)
            else:
              if fn0[0] < 65793:
                if fn0[0] == 65790:  # {U_Python.Prim.{next1}1}
                  return _idris_Python_46_Prim_46__123_next1_125_(arg0)
                elif fn0[0] == 65791:  # {U_Python.Prim.{next2}1}
                  P_c0 = fn0[1]
                  return _idris_Python_46_Prim_46__123_next2_125_(P_c0, arg0)
                else:  # {U_Python.Prim.{next3}1}
                  return _idris_Python_46_Prim_46__123_next3_125_(arg0)
              else:
                if fn0[0] == 65793:  # {U_Python.Prim.{next4}1}
                  return _idris_Python_46_Prim_46__123_next4_125_(arg0)
                elif fn0[0] == 65794:  # {U_Python.Prim.{next5}1}
                  return _idris_Python_46_Prim_46__123_next5_125_(arg0)
                else:  # {U_Python.Prim.{next6}1}
                  return _idris_Python_46_Prim_46__123_next6_125_(arg0)
          else:
            if fn0[0] < 65802:
              if fn0[0] < 65799:
                if fn0[0] == 65796:  # {U_Python.Prim.{next7}1}
                  return _idris_Python_46_Prim_46__123_next7_125_(arg0)
                elif fn0[0] == 65797:  # {U_Python.Prim.{next8}1}
                  P_c0 = fn0[1]
                  return _idris_Python_46_Prim_46__123_next8_125_(P_c0, arg0)
                else:  # {U_Python.Prim.{next9}1}
                  return _idris_Python_46_Prim_46__123_next9_125_(arg0)
              else:
                if fn0[0] == 65799:  # {U_Python.importModule1}
                  P_c0, P_c1 = fn0[1:]
                  return _idris_Python_46_importModule(P_c0, P_c1, arg0)
                elif fn0[0] == 65800:  # {U_Python.{marshalPIO0}1}
                  P_c0 = fn0[1]
                  return _idris_Python_46__123_marshalPIO0_125_(P_c0, arg0)
                else:  # {U_io_bind1}
                  P_c0, P_c1, P_c2, P_c3, P_c4 = fn0[1:]
                  return _idris_io_95_bind(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
            else:
              if fn0[0] < 65805:
                if fn0[0] == 65802:  # {U_io_return1}
                  P_c0, P_c1, P_c2 = fn0[1:]
                  return _idris_io_95_return(P_c0, P_c1, P_c2, arg0)
                elif fn0[0] == 65803:  # {U_prim__toStrBigInt1}
                  return _idris_prim_95__95_toStrBigInt(arg0)
                else:  # {U_prim__toStrInt1}
                  return _idris_prim_95__95_toStrInt(arg0)
              else:
                if fn0[0] == 65805:  # {U_prim__zextInt_BigInt1}
                  return _idris_prim_95__95_zextInt_95_BigInt(arg0)
                elif fn0[0] == 65806:  # {U_prim_lenString1}
                  return _idris_prim_95_lenString(arg0)
                else:  # {U_{Python.Lib.Threading.forkPIO, worker_lam0}1}
                  return _idris__123_Python_46_Lib_46_Threading_46_forkPIO_44__32_worker_95_lam0_125_(
                    arg0
                  )
        else:
          if fn0[0] < 65819:
            if fn0[0] < 65813:
              if fn0[0] < 65810:
                if fn0[0] == 65808:  # {U_{Python.Lib.Threading.forkPIO, worker_lam1}1}
                  P_c0 = fn0[1]
                  return _idris__123_Python_46_Lib_46_Threading_46_forkPIO_44__32_worker_95_lam1_125_(
                    P_c0, arg0
                  )
                else:  # {U_{Python.Prim.iterate, iter_lam0}1}
                  return _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam0_125_(arg0)
              else:
                if fn0[0] == 65810:  # {U_{Python.Prim.iterate, iter_lam10}1}
                  return _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam10_125_(arg0)
                elif fn0[0] == 65811:  # {U_{Python.Prim.iterate, iter_lam11}1}
                  return _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam11_125_(arg0)
                else:  # {U_{Python.Prim.iterate, iter_lam12}1}
                  return _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam12_125_(arg0)
            else:
              if fn0[0] < 65816:
                if fn0[0] == 65813:  # {U_{Python.Prim.iterate, iter_lam13}1}
                  P_c0, P_c1, P_c2 = fn0[1:]
                  return _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam13_125_(
                    P_c0, P_c1, P_c2, arg0
                  )
                elif fn0[0] == 65814:  # {U_{Python.Prim.iterate, iter_lam1}1}
                  return _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam1_125_(arg0)
                else:  # {U_{Python.Prim.iterate, iter_lam2}1}
                  P_c0 = fn0[1]
                  return _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam2_125_(P_c0, arg0)
              else:
                if fn0[0] == 65816:  # {U_{Python.Prim.iterate, iter_lam3}1}
                  return _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam3_125_(arg0)
                elif fn0[0] == 65817:  # {U_{Python.Prim.iterate, iter_lam4}1}
                  return _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam4_125_(arg0)
                else:  # {U_{Python.Prim.iterate, iter_lam5}1}
                  return _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam5_125_(arg0)
          else:
            if fn0[0] < 65825:
              if fn0[0] < 65822:
                if fn0[0] == 65819:  # {U_{Python.Prim.iterate, iter_lam6}1}
                  P_c0, P_c1 = fn0[1:]
                  return _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam6_125_(P_c0, P_c1, arg0)
                elif fn0[0] == 65820:  # {U_{Python.Prim.iterate, iter_lam7}1}
                  return _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam7_125_(arg0)
                else:  # {U_{Python.Prim.iterate, iter_lam8}1}
                  return _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam8_125_(arg0)
              else:
                if fn0[0] == 65822:  # {U_{Python.Prim.iterate, iter_lam9}1}
                  P_c0 = fn0[1]
                  return _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam9_125_(P_c0, arg0)
                elif fn0[0] == 65823:  # {U_{io_bind1}1}
                  P_c0, P_c1, P_c2, P_c3, P_c4, P_c5 = fn0[1:]
                  return io_bind1(P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, arg0)
                else:  # {U_{unsafePerformIO0}1}
                  return unsafePerformIO0(arg0)
            else:
              if fn0[0] < 65828:
                if fn0[0] == 65825:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable2}
                  P_c0, P_c1, P_c2 = fn0[1:]
                  return (65727, P_c0, P_c1, P_c2, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable1}
                elif fn0[0] == 65826:  # {U_Prelude.Nat.Nat instance of Prelude.Classes.Eq2}
                  return (65729, arg0)  # {U_Prelude.Nat.Nat instance of Prelude.Classes.Eq1}
                else:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable3}
                  P_c0, P_c1 = fn0[1:]
                  return (65825, P_c0, P_c1, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable2}
              else:
                if fn0[0] == 65828:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable4}
                  P_c0 = fn0[1]
                  return (65827, P_c0, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable3}
                else:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable5}
                  return (65828, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable4}
    return _idris_error("unreachable due to case in tail position")

# {EVAL0}
def EVAL0(arg0):
  while True:
    return arg0

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam0}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
  in0, in1
):
  while True:
    return _idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat_58__33_compare_58_0(
      in0, in1
    )

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam0}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
  in0, in1
):
  while True:
    return _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33_compare_58_0(
      in0, in1
    )

# Prelude.Functor.{Prelude.Monad.IO' ffi instance of Prelude.Functor.Functor, method map_lam0}
def _idris_Prelude_46_Functor_46__123_Prelude_46_Monad_46_IO_39__32_ffi_32_instance_32_of_32_Prelude_46_Functor_46_Functor_44__32_method_32_map_95_lam0_125_(
  e3, in0
):
  while True:
    return (65802, None, None, APPLY0(e3, in0))  # {U_io_return1}

# Prelude.Classes.{Prelude.Show.Prec instance of Prelude.Classes.Ord, method >=_lam0}
def _idris_Prelude_46_Classes_46__123_Prelude_46_Show_46_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__62__61__95_lam0_125_(
  e0, e1
):
  while True:
    return _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Eq_36_Prec_58__33__61__61__58_0(
      e0, e1
    )

# {Python.Lib.Threading.forkPIO, worker_lam0}
def _idris__123_Python_46_Lib_46_Threading_46_forkPIO_44__32_worker_95_lam0_125_(
  in1
):
  while True:
    return (0,)  # Python.Telescope.Return

# {Python.Prim.iterate, iter_lam0}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam0_125_(in2):
  while True:
    return (65802, None, None, in2)  # {U_io_return1}

# Python.Functions.{call0}
def _idris_Python_46_Functions_46__123_call0_125_(e3, e2, e5, in0):
  while True:
    return _idris_call(e3, _idris_Python_46_Telescope_46_strip(None, e2, e5))

# Python.Prim.{case block in Python.Prim.iterate, iter at ./Python/Prim.idr:84:17_lam0}
def _idris_Python_46_Prim_46__123_case_32_block_32_in_32_Python_46_Prim_46_iterate_44__32_iter_32_at_32__46__47_Python_47_Prim_46_idr_58_84_58_17_95_lam0_125_(
  e8, e10, in1
):
  while True:
    return _idris_Python_46_Prim_46_iterate_58_iter_58_0(
      None, None, None, None, None, None, None, None, e8, in1, e10
    )

# Main.{case block in main at example.idr:59:44_lam0}
def _idris_Main_46__123_case_32_block_32_in_32_main_32_at_32_example_46_idr_58_59_58_44_95_lam0_125_(
  in1
):
  while True:
    return (0,)  # Python.Telescope.Return

# Python.Exceptions.{catch0}
def _idris_Python_46_Exceptions_46__123_catch0_125_(in2):
  while True:
    return (65802, None, None, in2)  # {U_io_return1}

# Python.Prim.{collect0}
def _idris_Python_46_Prim_46__123_collect0_125_(in0, in1):
  while True:
    return (65802, None, None, in0.cons(in1))  # {U_io_return1}

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
def _idris_Python_46_Prim_46__123_next0_125_(in2):
  while True:
    return (65802, None, None, in2)  # {U_io_return1}

# Prelude.Show.{primNumShow0}
def _idris_Prelude_46_Show_46__123_primNumShow0_125_(in1):
  while True:
    aux1 = (in1 == '-')
    if aux1 == 0:
      return False
    else:
      return True
    return _idris_error("unreachable due to case in tail position")

# Prelude.Interactive.{putStr'0}
def _idris_Prelude_46_Interactive_46__123_putStr_39_0_125_(e1, in0):
  while True:
    return sys.stdout.write(e1)

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

# Python.Telescope.{strip0}
def _idris_Python_46_Telescope_46__123_strip0_125_(in2):
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

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam1}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
  in0
):
  while True:
    return (65730, in0)  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam0}1}

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam1}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
  in0
):
  while True:
    return (65734, in0)  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam0}1}

# {Python.Lib.Threading.forkPIO, worker_lam1}
def _idris__123_Python_46_Lib_46_Threading_46_forkPIO_44__32_worker_95_lam1_125_(
  e2, in0
):
  while True:
    return _idris_Python_46_Functions_46_call(
      None,
      None,
      (1, (0,), (65807,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_{Python.Lib.Threading.forkPIO, worker_lam0}1}
      _idris_Python_46_Fields_46__47__46_(None, None, e2, "put", None),
      None,
      (0, in0, Unit)  # Builtins.MkSigma
    )

# {Python.Prim.iterate, iter_lam1}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam1_125_(in1):
  while True:
    return (65809,)  # {U_{Python.Prim.iterate, iter_lam0}1}

# Main.{case block in main at example.idr:59:44_lam1}
def _idris_Main_46__123_case_32_block_32_in_32_main_32_at_32_example_46_idr_58_59_58_44_95_lam1_125_(
  e18, in2, in3
):
  while True:
    assert e18[0] == 0  # constructor of Prelude.Monad.Monad
    in4, in5 = e18[1:]
    aux1 = in4
    return APPLY0(
      _idris_Prelude_46_Applicative_46_pure(None, None, aux1),
      APPLY0(_idris_Prelude_46_Strings_46_length(), in2)
    )

# Python.Exceptions.{catch1}
def _idris_Python_46_Exceptions_46__123_catch1_125_(in1):
  while True:
    return (65740,)  # {U_Python.Exceptions.{catch0}1}

# Python.Prim.{collect1}
def _idris_Python_46_Prim_46__123_collect1_125_(in0):
  while True:
    return (65781, in0)  # {U_Python.Prim.{collect0}1}

# Python.Prim.{foreach1}
def _idris_Python_46_Prim_46__123_foreach1_125_(e3, e4, e5, in0):
  while True:
    return _idris_Prelude_46_Functor_46_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65771, None),  # {U_Python.IO.unRaw1}
      (65783, e3, e4, e5)  # {U_Python.Prim.{foreach0}1}
    )

# Python.Lib.Threading.{forkPIO1}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO1_125_(in0):
  while True:
    return (1, (2, 0), (65772,))  # Python.Telescope.Bind, Python.Telescope.Default, {U_Python.Lib.Threading.{forkPIO0}1}

# {io_bind1}
def io_bind1(e0, e1, e2, e3, e4, _idris_w, in0):
  while True:
    return APPLY0(io_bind0(e0, e1, e2, e3, e4, _idris_w, in0), _idris_w)

# Main.{main1}
def _idris_Main_46__123_main1_125_(in6):
  while True:
    return (0,)  # Python.Telescope.Return

# Python.Prim.{next1}
def _idris_Python_46_Prim_46__123_next1_125_(in1):
  while True:
    return (65786,)  # {U_Python.Prim.{next0}1}

# Prelude.Show.{primNumShow1}
def _idris_Prelude_46_Show_46__123_primNumShow1_125_(e0, e1, e2, e3, in0, in2):
  while True:
    return (65738,)  # {U_Prelude.Show.{primNumShow0}1}

# Prelude.Interactive.{putStr'1}
def _idris_Prelude_46_Interactive_46__123_putStr_39_1_125_(in1):
  while True:
    return (65802, None, None, Unit)  # {U_io_return1}

# Python.Exceptions.{try1}
def _idris_Python_46_Exceptions_46__123_try1_125_(in2):
  while True:
    return (0, in2)  # Prelude.Either.Left

# {unsafePerformIO1}
def unsafePerformIO1(e0, e1, e2):
  while True:
    return (65824,)  # {U_{unsafePerformIO0}1}

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam2}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
  in2, in3
):
  while True:
    aux1 = APPLY0(
      APPLY0(
        _idris_Prelude_46_Classes_46_compare(
          None,
          _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat()
        ),
        in2
      ),
      in3
    )
    if aux1[0] == 2:  # Prelude.Classes.GT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam2}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
  in2, in3
):
  while True:
    aux1 = APPLY0(
      APPLY0(
        _idris_Prelude_46_Classes_46_compare(
          None,
          _idris_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec()
        ),
        in2
      ),
      in3
    )
    if aux1[0] == 2:  # Prelude.Classes.GT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# {Python.Prim.iterate, iter_lam2}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam2_125_(in5, in6):
  while True:
    return (65801, None, None, None, in5, in6)  # {U_io_bind1}

# Main.{case block in main at example.idr:59:44_lam2}
def _idris_Main_46__123_case_32_block_32_in_32_main_32_at_32_example_46_idr_58_59_58_44_95_lam2_125_(
  e18, e20, in2
):
  while True:
    return APPLY0(
      APPLY0(
        _idris_Prelude_46_Monad_46__62__62__61_(None, None, None, e18),
        _idris_Prelude_46_Interactive_46_putStr_39_(
          None,
          (("thread " + (e20 + " done")) + "\n")
        )
      ),
      (65650, e18, in2)  # {U_Main.{case block in main at example.idr:59:44_lam1}1}
    )

# Python.Exceptions.{catch2}
def _idris_Python_46_Exceptions_46__123_catch2_125_(in5, in6):
  while True:
    return (65801, None, None, None, in5, in6)  # {U_io_bind1}

# Python.Lib.Threading.{forkPIO2}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO2_125_(in4):
  while True:
    return (0,)  # Python.Telescope.Return

# {io_bind2}
def io_bind2(e0, e1, e2, e3, e4, _idris_w):
  while True:
    return (65823, e0, e1, e2, e3, e4, _idris_w)  # {U_{io_bind1}1}

# Main.{main2}
def _idris_Main_46__123_main2_125_(in5):
  while True:
    return (1, (0,), (65664,))  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main1}1}

# Python.Prim.{next2}
def _idris_Python_46_Prim_46__123_next2_125_(in5, in6):
  while True:
    return (65801, None, None, None, in5, in6)  # {U_io_bind1}

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

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam3}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
  in2
):
  while True:
    return (65732, in2)  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam2}1}

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam3}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
  in2
):
  while True:
    return (65736, in2)  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam2}1}

# {Python.Prim.iterate, iter_lam3}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam3_125_(in5):
  while True:
    return (65815, in5)  # {U_{Python.Prim.iterate, iter_lam2}1}

# Main.{case block in main at example.idr:59:44_lam3}
def _idris_Main_46__123_case_32_block_32_in_32_main_32_at_32_example_46_idr_58_59_58_44_95_lam3_125_(
  e18, e3, e20, in0
):
  while True:
    return APPLY0(
      APPLY0(
        _idris_Prelude_46_Monad_46__62__62__61_(None, None, None, e18),
        _idris_Python_46_Fields_46__47__58_(
          None,
          None,
          _idris_Python_46_Functions_46_call(
            None,
            None,
            (1, (0,), (65649,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{case block in main at example.idr:59:44_lam0}1}
            _idris_Python_46_Fields_46__47__46_(None, None, e3, "get", None),
            None,
            (0, "http://idris-lang.org", Unit)  # Builtins.MkSigma
          ),
          "text",
          None
        )
      ),
      (65651, e18, e20)  # {U_Main.{case block in main at example.idr:59:44_lam2}1}
    )

# Python.Exceptions.{catch3}
def _idris_Python_46_Exceptions_46__123_catch3_125_(in5):
  while True:
    return (65742, in5)  # {U_Python.Exceptions.{catch2}1}

# Python.Lib.Threading.{forkPIO3}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO3_125_(in3):
  while True:
    return (1, (0,), (65774,))  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Threading.{forkPIO2}1}

# Main.{main3}
def _idris_Main_46__123_main3_125_(in8):
  while True:
    return (0,)  # Python.Telescope.Return

# Python.Prim.{next3}
def _idris_Python_46_Prim_46__123_next3_125_(in5):
  while True:
    return (65791, in5)  # {U_Python.Prim.{next2}1}

# Python.Exceptions.{try3}
def _idris_Python_46_Exceptions_46__123_try3_125_(in4):
  while True:
    return (1, in4)  # Prelude.Either.Right

# {Python.Prim.iterate, iter_lam4}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam4_125_(in4):
  while True:
    return (65816,)  # {U_{Python.Prim.iterate, iter_lam3}1}

# Python.Exceptions.{catch4}
def _idris_Python_46_Exceptions_46__123_catch4_125_(in4):
  while True:
    return (65743,)  # {U_Python.Exceptions.{catch3}1}

# Python.Lib.Threading.{forkPIO4}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO4_125_(in2, in6):
  while True:
    return (65802, None, None, in2)  # {U_io_return1}

# Main.{main4}
def _idris_Main_46__123_main4_125_(in15, in16, in17):
  while True:
    return _idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
      None, None, in15, in16, in17
    )

# Python.Prim.{next4}
def _idris_Python_46_Prim_46__123_next4_125_(in4):
  while True:
    return (65792,)  # {U_Python.Prim.{next3}1}

# Python.Exceptions.{try4}
def _idris_Python_46_Exceptions_46__123_try4_125_(e1, in0):
  while True:
    return _idris_try(
      e1,
      (65723, None, None, None, (65749,), (65758,)),  # {U_Prelude.Basics..1}, {U_Python.Exceptions.{try0}1}, {U_Python.Exceptions.{try1}1}
      (
        65723,  # {U_Prelude.Basics..1}
        None,
        None,
        None,
        (65723, None, None, None, (65759,), (65760,)),  # {U_Prelude.Basics..1}, {U_Python.Exceptions.{try2}1}, {U_Python.Exceptions.{try3}1}
        (65771, None)  # {U_Python.IO.unRaw1}
      )
    )

# {Python.Prim.iterate, iter_lam5}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam5_125_(in3):
  while True:
    return (65817,)  # {U_{Python.Prim.iterate, iter_lam4}1}

# Python.Exceptions.{catch5}
def _idris_Python_46_Exceptions_46__123_catch5_125_(in3):
  while True:
    return (65744,)  # {U_Python.Exceptions.{catch4}1}

# Python.Lib.Threading.{forkPIO5}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO5_125_(in2, in5):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Functions_46_call(
        None,
        None,
        (0,),  # Python.Telescope.Return
        _idris_Python_46_Fields_46__47__46_(None, None, in5, "start", None),
        None,
        Unit
      ),
      (65776, in2)  # {U_Python.Lib.Threading.{forkPIO4}1}
    )

# Main.{main5}
def _idris_Main_46__123_main5_125_(in15, in16):
  while True:
    return (65697, in15, in16)  # {U_Main.{main4}1}

# Python.Prim.{next5}
def _idris_Python_46_Prim_46__123_next5_125_(in3):
  while True:
    return (65793,)  # {U_Python.Prim.{next4}1}

# Python.Exceptions.{try5}
def _idris_Python_46_Exceptions_46__123_try5_125_(in7):
  while True:
    return (65802, None, None, in7)  # {U_io_return1}

# {Python.Prim.iterate, iter_lam6}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam6_125_(e8, e10, in8):
  while True:
    return _idris_Python_46_Prim_46_iterate_58_iter_58_0(
      None, None, None, None, None, None, None, None, e8, in8, e10
    )

# Python.Exceptions.{catch6}
def _idris_Python_46_Exceptions_46__123_catch6_125_(e2, in0):
  while True:
    if in0[0] == 1:  # Python.Exceptions.Except
      in7, in8 = in0[1:]
      return APPLY0(APPLY0(e2, in7), in8)
    else:  # Python.Exceptions.OK
      in9 = in0[1]
      aux1 = (0, (65741,), (65745,))  # constructor of Prelude.Monad.Monad, {U_Python.Exceptions.{catch1}1}, {U_Python.Exceptions.{catch5}1}
      assert aux1[0] == 0  # constructor of Prelude.Monad.Monad
      in10, in11 = aux1[1:]
      aux2 = in10
      return APPLY0(_idris_Prelude_46_Applicative_46_pure(None, None, aux2), in9)
    return _idris_error("unreachable due to case in tail position")

# Python.Lib.Threading.{forkPIO6}
def _idris_Python_46_Lib_46_Threading_46__123_forkPIO6_125_(e1, in2):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Functions_46__36__58_(
        None,
        (1, (0,), (65775,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Threading.{forkPIO3}1}
        _idris_Python_46_Fields_46__47__58_(
          None,
          None,
          (65799, None, "threading"),  # {U_Python.importModule1}
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
      (65777, in2)  # {U_Python.Lib.Threading.{forkPIO5}1}
    )

# Main.{main6}
def _idris_Main_46__123_main6_125_(in15):
  while True:
    return (65708, in15)  # {U_Main.{main5}1}

# Python.Prim.{next6}
def _idris_Python_46_Prim_46__123_next6_125_(in2):
  while True:
    return (65802, None, None, in2)  # {U_io_return1}

# Python.Exceptions.{try6}
def _idris_Python_46_Exceptions_46__123_try6_125_(in6):
  while True:
    return (65762,)  # {U_Python.Exceptions.{try5}1}

# {Python.Prim.iterate, iter_lam7}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam7_125_(in2):
  while True:
    return (65802, None, None, in2)  # {U_io_return1}

# Main.{main7}
def _idris_Main_46__123_main7_125_(in14):
  while True:
    return (65719,)  # {U_Main.{main6}1}

# Python.Prim.{next7}
def _idris_Python_46_Prim_46__123_next7_125_(in1):
  while True:
    return (65795,)  # {U_Python.Prim.{next6}1}

# Python.Exceptions.{try7}
def _idris_Python_46_Exceptions_46__123_try7_125_(in10, in11):
  while True:
    return (65801, None, None, None, in10, in11)  # {U_io_bind1}

# {Python.Prim.iterate, iter_lam8}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam8_125_(in1):
  while True:
    return (65820,)  # {U_{Python.Prim.iterate, iter_lam7}1}

# Main.{main8}
def _idris_Main_46__123_main8_125_(in13):
  while True:
    return (65720,)  # {U_Main.{main7}1}

# Python.Prim.{next8}
def _idris_Python_46_Prim_46__123_next8_125_(in5, in6):
  while True:
    return (65801, None, None, None, in5, in6)  # {U_io_bind1}

# Python.Exceptions.{try8}
def _idris_Python_46_Exceptions_46__123_try8_125_(in10):
  while True:
    return (65764, in10)  # {U_Python.Exceptions.{try7}1}

# {Python.Prim.iterate, iter_lam9}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam9_125_(in5, in6):
  while True:
    return (65801, None, None, None, in5, in6)  # {U_io_bind1}

# Main.{main9}
def _idris_Main_46__123_main9_125_(in18, in19):
  while True:
    return (in18 + in19)

# Python.Prim.{next9}
def _idris_Python_46_Prim_46__123_next9_125_(in5):
  while True:
    return (65797, in5)  # {U_Python.Prim.{next8}1}

# Python.Exceptions.{try9}
def _idris_Python_46_Exceptions_46__123_try9_125_(in9):
  while True:
    return (65765,)  # {U_Python.Exceptions.{try8}1}

# {Python.Prim.iterate, iter_lam10}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam10_125_(in5):
  while True:
    return (65822, in5)  # {U_{Python.Prim.iterate, iter_lam9}1}

# Main.{main10}
def _idris_Main_46__123_main10_125_(in18):
  while True:
    return (65722, in18)  # {U_Main.{main9}1}

# Python.Prim.{next10}
def _idris_Python_46_Prim_46__123_next10_125_(in4):
  while True:
    return (65798,)  # {U_Python.Prim.{next9}1}

# Python.Exceptions.{try10}
def _idris_Python_46_Exceptions_46__123_try10_125_(in8):
  while True:
    return (65766,)  # {U_Python.Exceptions.{try9}1}

# {Python.Prim.iterate, iter_lam11}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam11_125_(in4):
  while True:
    return (65810,)  # {U_{Python.Prim.iterate, iter_lam10}1}

# Main.{main11}
def _idris_Main_46__123_main11_125_(in11, in21):
  while True:
    return (65802, None, None, (in11 + 1))  # {U_io_return1}

# Python.Prim.{next11}
def _idris_Python_46_Prim_46__123_next11_125_(in3):
  while True:
    return (65787,)  # {U_Python.Prim.{next10}1}

# Python.Exceptions.{try11}
def _idris_Python_46_Exceptions_46__123_try11_125_(in7):
  while True:
    return (65802, None, None, in7)  # {U_io_return1}

# {Python.Prim.iterate, iter_lam12}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam12_125_(in3):
  while True:
    return (65811,)  # {U_{Python.Prim.iterate, iter_lam11}1}

# Main.{main12}
def _idris_Main_46__123_main12_125_(in11, in20):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        ((_idris_Prelude_46_Show_46_primNumShow(None, (65804,), (0,), (in11 + 1)) + (". " + in20)) + "\n")  # {U_prim__toStrInt1}, Prelude.Show.Open
      ),
      (65655, in11)  # {U_Main.{main11}1}
    )

# Python.Prim.{next12}
def _idris_Python_46_Prim_46__123_next12_125_(in0):
  while True:
    if in0[0] == 1:  # Python.Exceptions.Except
      in7, in8 = in0[1:]
      if in7[0] == 0:  # Python.Exceptions.StopIteration
        aux1 = (0, (65790,), (65794,))  # constructor of Prelude.Monad.Monad, {U_Python.Prim.{next1}1}, {U_Python.Prim.{next5}1}
        assert aux1[0] == 0  # constructor of Prelude.Monad.Monad
        in9, in10 = aux1[1:]
        aux2 = in9
        return APPLY0(_idris_Prelude_46_Applicative_46_pure(None, None, aux2), None)
      else:
        return _idris_Python_46_Exceptions_46_raise(None, in8)
      return _idris_error("unreachable due to case in tail position")
    else:  # Python.Exceptions.OK
      in11 = in0[1]
      aux3 = (0, (65796,), (65788,))  # constructor of Prelude.Monad.Monad, {U_Python.Prim.{next7}1}, {U_Python.Prim.{next11}1}
      assert aux3[0] == 0  # constructor of Prelude.Monad.Monad
      in12, in13 = aux3[1:]
      aux4 = in12
      return APPLY0(_idris_Prelude_46_Applicative_46_pure(None, None, aux4), in11)
    return _idris_error("unreachable due to case in tail position")

# Python.Exceptions.{try12}
def _idris_Python_46_Exceptions_46__123_try12_125_(in6):
  while True:
    return (65751,)  # {U_Python.Exceptions.{try11}1}

# {Python.Prim.iterate, iter_lam13}
def _idris__123_Python_46_Prim_46_iterate_44__32_iter_95_lam13_125_(
  e10, e9, e8, in0
):
  while True:
    if in0 is not None:  # Prelude.Maybe.Just
      in7 = in0
      return APPLY0(
        APPLY0(
          _idris_Prelude_46_Monad_46__62__62__61_(
            None,
            None,
            None,
            (0, (65814,), (65818,))  # constructor of Prelude.Monad.Monad, {U_{Python.Prim.iterate, iter_lam1}1}, {U_{Python.Prim.iterate, iter_lam5}1}
          ),
          APPLY0(APPLY0(e10, e9), in7)
        ),
        (65819, e8, e10)  # {U_{Python.Prim.iterate, iter_lam6}1}
      )
    else:  # Prelude.Maybe.Nothing
      aux1 = (0, (65821,), (65812,))  # constructor of Prelude.Monad.Monad, {U_{Python.Prim.iterate, iter_lam8}1}, {U_{Python.Prim.iterate, iter_lam12}1}
      assert aux1[0] == 0  # constructor of Prelude.Monad.Monad
      in9, in10 = aux1[1:]
      aux2 = in9
      return APPLY0(_idris_Prelude_46_Applicative_46_pure(None, None, aux2), e9)
    return _idris_error("unreachable due to case in tail position")

# Main.{main13}
def _idris_Main_46__123_main13_125_(in11, in12):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Functor_46_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
        None,
        None,
        None,
        _idris_Prelude_46_Foldable_46_concat(None, None, (65721,), (0, (65654,), "")),  # {U_Main.{main8}1}, constructor of Prelude.Algebra.Monoid, {U_Main.{main10}1}
        _idris_Python_46_Prim_46_collect(
          None,
          None,
          _idris_Python_46_Fields_46__47__46_(None, None, in12, "strings", None),
          None
        )
      ),
      (65656, in11)  # {U_Main.{main12}1}
    )

# Python.Exceptions.{try13}
def _idris_Python_46_Exceptions_46__123_try13_125_(in10, in11):
  while True:
    return (65801, None, None, None, in10, in11)  # {U_io_bind1}

# Main.{main14}
def _idris_Main_46__123_main14_125_(in11):
  while True:
    return (65657, in11)  # {U_Main.{main13}1}

# Python.Exceptions.{try14}
def _idris_Python_46_Exceptions_46__123_try14_125_(in10):
  while True:
    return (65753, in10)  # {U_Python.Exceptions.{try13}1}

# Main.{main15}
def _idris_Main_46__123_main15_125_(in28):
  while True:
    return (65802, None, None, in28)  # {U_io_return1}

# Python.Exceptions.{try15}
def _idris_Python_46_Exceptions_46__123_try15_125_(in9):
  while True:
    return (65754,)  # {U_Python.Exceptions.{try14}1}

# Main.{main16}
def _idris_Main_46__123_main16_125_(in27):
  while True:
    return (65659,)  # {U_Main.{main15}1}

# Python.Exceptions.{try16}
def _idris_Python_46_Exceptions_46__123_try16_125_(in8):
  while True:
    return (65755,)  # {U_Python.Exceptions.{try15}1}

# Main.{main17}
def _idris_Main_46__123_main17_125_(in31, in32):
  while True:
    return (65801, None, None, None, in31, in32)  # {U_io_bind1}

# Python.Exceptions.{try17}
def _idris_Python_46_Exceptions_46__123_try17_125_(in5):
  while True:
    if in5[0] == 0:  # Prelude.Either.Left
      in12 = in5[1]
      aux1 = (0, (65763,), (65750,))  # constructor of Prelude.Monad.Monad, {U_Python.Exceptions.{try6}1}, {U_Python.Exceptions.{try10}1}
      assert aux1[0] == 0  # constructor of Prelude.Monad.Monad
      in13, in14 = aux1[1:]
      aux2 = in13
      aux3 = _idris_Python_46_Fields_46__47__46_(
        None,
        None,
        _idris_Python_46_Fields_46__47__46_(None, None, in12, "__class__", None),
        "__name__",
        None
      )
      return APPLY0(
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
          }.get(aux3, (35,)),  # Python.Exceptions.Other
          in12
        )
      )
    else:  # Prelude.Either.Right
      in15 = in5[1]
      aux4 = (0, (65752,), (65756,))  # constructor of Prelude.Monad.Monad, {U_Python.Exceptions.{try12}1}, {U_Python.Exceptions.{try16}1}
      assert aux4[0] == 0  # constructor of Prelude.Monad.Monad
      in16, in17 = aux4[1:]
      aux5 = in16
      return APPLY0(_idris_Prelude_46_Applicative_46_pure(None, None, aux5), (0, in15))  # Python.Exceptions.OK
    return _idris_error("unreachable due to case in tail position")

# Main.{main18}
def _idris_Main_46__123_main18_125_(in31):
  while True:
    return (65661, in31)  # {U_Main.{main17}1}

# Main.{main19}
def _idris_Main_46__123_main19_125_(in30):
  while True:
    return (65662,)  # {U_Main.{main18}1}

# Main.{main20}
def _idris_Main_46__123_main20_125_(in29):
  while True:
    return (65663,)  # {U_Main.{main19}1}

# Main.{main21}
def _idris_Main_46__123_main21_125_(in28):
  while True:
    return (65802, None, None, in28)  # {U_io_return1}

# Main.{main22}
def _idris_Main_46__123_main22_125_(in27):
  while True:
    return (65666,)  # {U_Main.{main21}1}

# Main.{main23}
def _idris_Main_46__123_main23_125_(in31, in32):
  while True:
    return (65801, None, None, None, in31, in32)  # {U_io_bind1}

# Main.{main24}
def _idris_Main_46__123_main24_125_(in31):
  while True:
    return (65668, in31)  # {U_Main.{main23}1}

# Main.{main25}
def _idris_Main_46__123_main25_125_(in30):
  while True:
    return (65669,)  # {U_Main.{main24}1}

# Main.{main26}
def _idris_Main_46__123_main26_125_(in29):
  while True:
    return (65670,)  # {U_Main.{main25}1}

# Main.{main27}
def _idris_Main_46__123_main27_125_(in34):
  while True:
    return (0,)  # Python.Telescope.Return

# Main.{main28}
def _idris_Main_46__123_main28_125_(in28):
  while True:
    return (65802, None, None, in28)  # {U_io_return1}

# Main.{main29}
def _idris_Main_46__123_main29_125_(in27):
  while True:
    return (65673,)  # {U_Main.{main28}1}

# Main.{main30}
def _idris_Main_46__123_main30_125_(in31, in32):
  while True:
    return (65801, None, None, None, in31, in32)  # {U_io_bind1}

# Main.{main31}
def _idris_Main_46__123_main31_125_(in31):
  while True:
    return (65676, in31)  # {U_Main.{main30}1}

# Main.{main32}
def _idris_Main_46__123_main32_125_(in30):
  while True:
    return (65677,)  # {U_Main.{main31}1}

# Main.{main33}
def _idris_Main_46__123_main33_125_(in29):
  while True:
    return (65678,)  # {U_Main.{main32}1}

# Main.{main34}
def _idris_Main_46__123_main34_125_(in28):
  while True:
    return (65802, None, None, in28)  # {U_io_return1}

# Main.{main35}
def _idris_Main_46__123_main35_125_(in27):
  while True:
    return (65680,)  # {U_Main.{main34}1}

# Main.{main36}
def _idris_Main_46__123_main36_125_(in31, in32):
  while True:
    return (65801, None, None, None, in31, in32)  # {U_io_bind1}

# Main.{main37}
def _idris_Main_46__123_main37_125_(in31):
  while True:
    return (65682, in31)  # {U_Main.{main36}1}

# Main.{main38}
def _idris_Main_46__123_main38_125_(in30):
  while True:
    return (65683,)  # {U_Main.{main37}1}

# Main.{main39}
def _idris_Main_46__123_main39_125_(in29):
  while True:
    return (65684,)  # {U_Main.{main38}1}

# Main.{main40}
def _idris_Main_46__123_main40_125_(in35, in36):
  while True:
    aux1 = (0, (65681,), (65685,))  # constructor of Prelude.Monad.Monad, {U_Main.{main35}1}, {U_Main.{main39}1}
    assert aux1[0] == 0  # constructor of Prelude.Monad.Monad
    in37, in38 = aux1[1:]
    aux2 = in37
    return APPLY0(
      _idris_Prelude_46_Applicative_46_pure(None, None, aux2),
      APPLY0(_idris_Prelude_46_Strings_46_length(), in35)
    )

# Main.{main41}
def _idris_Main_46__123_main41_125_(in26, in35):
  while True:
    return APPLY0(
      APPLY0(
        _idris_Prelude_46_Monad_46__62__62__61_(
          None,
          None,
          None,
          (0, (65674,), (65679,))  # constructor of Prelude.Monad.Monad, {U_Main.{main29}1}, {U_Main.{main33}1}
        ),
        _idris_Prelude_46_Interactive_46_putStr_39_(
          None,
          (("thread " + (in26 + " done")) + "\n")
        )
      ),
      (65687, in35)  # {U_Main.{main40}1}
    )

# Main.{main42}
def _idris_Main_46__123_main42_125_(in1, in26, in33):
  while True:
    return APPLY0(
      APPLY0(
        _idris_Prelude_46_Monad_46__62__62__61_(
          None,
          None,
          None,
          (0, (65667,), (65671,))  # constructor of Prelude.Monad.Monad, {U_Main.{main22}1}, {U_Main.{main26}1}
        ),
        _idris_Python_46_Fields_46__47__58_(
          None,
          None,
          _idris_Python_46_Functions_46_call(
            None,
            None,
            (1, (0,), (65672,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main27}1}
            _idris_Python_46_Fields_46__47__46_(None, None, in1, "get", None),
            None,
            (0, "http://idris-lang.org", Unit)  # Builtins.MkSigma
          ),
          "text",
          None
        )
      ),
      (65688, in26)  # {U_Main.{main41}1}
    )

# Main.{main43}
def _idris_Main_46__123_main43_125_(in1, in26):
  while True:
    return APPLY0(
      APPLY0(
        _idris_Prelude_46_Monad_46__62__62__61_(
          None,
          None,
          None,
          (0, (65660,), (65665,))  # constructor of Prelude.Monad.Monad, {U_Main.{main16}1}, {U_Main.{main20}1}
        ),
        _idris_Prelude_46_Interactive_46_putStr_39_(
          None,
          (("thread " + (in26 + " starting")) + "\n")
        )
      ),
      (65689, in1, in26)  # {U_Main.{main42}1}
    )

# Main.{main44}
def _idris_Main_46__123_main44_125_(in48):
  while True:
    return (0,)  # Python.Telescope.Return

# Main.{main45}
def _idris_Main_46__123_main45_125_(in49):
  while True:
    return _idris_Prelude_46_Interactive_46_putStr_39_(
      None,
      "Something's wrong, your root's homedir is writable!\n"
    )

# Main.{main46}
def _idris_Main_46__123_main46_125_(in50, in51):
  while True:
    if in50[0] == 11:  # Python.Exceptions.OSError
      return _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        (("  -> (1) everything's fine: " + _idris_Python_46_Exceptions_46_showException(in51)) + "\n")
      )
    else:
      return _idris_Python_46_Exceptions_46_raise(None, in51)
    return _idris_error("unreachable due to case in tail position")

# Main.{main47}
def _idris_Main_46__123_main47_125_(in50):
  while True:
    return (65693, in50)  # {U_Main.{main46}1}

# Main.{main48}
def _idris_Main_46__123_main48_125_(in53):
  while True:
    return (0,)  # Python.Telescope.Return

# Main.{main49}
def _idris_Main_46__123_main49_125_(in54):
  while True:
    if in54[0] == 1:  # Python.Exceptions.Except
      in55, in56 = in54[1:]
      if in55[0] == 11:  # Python.Exceptions.OSError
        return _idris_Prelude_46_Interactive_46_putStr_39_(
          None,
          (("  -> (2) everything's fine: " + _idris_Python_46_Exceptions_46_showException(in56)) + "\n")
        )
      else:
        return _idris_Python_46_Exceptions_46_raise(None, in56)
      return _idris_error("unreachable due to case in tail position")
    else:  # Python.Exceptions.OK
      in57 = in54[1]
      return _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        "Your root could probably use some security lessons!\n"
      )
    return _idris_error("unreachable due to case in tail position")

# Main.{main50}
def _idris_Main_46__123_main50_125_(in46, in52):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Exceptions_46_try(
        None,
        _idris_Python_46_Functions_46_call(
          None,
          None,
          (1, (0,), (65695,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main48}1}
          _idris_Python_46_Fields_46__47__46_(None, None, in46, "mkdir", None),
          None,
          (0, "/root/hello", Unit)  # Builtins.MkSigma
        )
      ),
      (65696,)  # {U_Main.{main49}1}
    )

# Main.{main51}
def _idris_Main_46__123_main51_125_(in46, in47):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Exceptions_46_catch(
        None,
        _idris_Python_46_Exceptions_46_try(
          None,
          (
            65801,  # {U_io_bind1}
            None,
            None,
            None,
            _idris_Python_46_Functions_46_call(
              None,
              None,
              (1, (0,), (65691,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main44}1}
              _idris_Python_46_Fields_46__47__46_(None, None, in46, "mkdir", None),
              None,
              (0, "/root/hello", Unit)  # Builtins.MkSigma
            ),
            (65692,)  # {U_Main.{main45}1}
          )
        ),
        (65694,)  # {U_Main.{main47}1}
      ),
      (65698, in46)  # {U_Main.{main50}1}
    )

# Main.{main52}
def _idris_Main_46__123_main52_125_(in46):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(None, "And now, let's fail!\n"),
      (65699, in46)  # {U_Main.{main51}1}
    )

# Main.{main53}
def _idris_Main_46__123_main53_125_(in45):
  while True:
    return (65801, None, None, None, (65799, None, "os"), (65700,))  # {U_io_bind1}, {U_Python.importModule1}, {U_Main.{main52}1}

# Main.{main54}
def _idris_Main_46__123_main54_125_(in44):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(None, "\n"),
      (65701,)  # {U_Main.{main53}1}
    )

# Main.{main55}
def _idris_Main_46__123_main55_125_(in42, in43):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        (("thread B says " + _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_Nat_58__33_show_58_0(
          in42
        )) + "\n")
      ),
      (65702,)  # {U_Main.{main54}1}
    )

# Main.{main56}
def _idris_Main_46__123_main56_125_(in41, in42):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        (("thread A says " + _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_Nat_58__33_show_58_0(
          in41
        )) + "\n")
      ),
      (65703, in42)  # {U_Main.{main55}1}
    )

# Main.{main57}
def _idris_Main_46__123_main57_125_(in40, in41):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Lib_46_Threading_46_wait(None, in40),
      (65704, in41)  # {U_Main.{main56}1}
    )

# Main.{main58}
def _idris_Main_46__123_main58_125_(in39, in40):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Lib_46_Threading_46_wait(None, in39),
      (65705, in40)  # {U_Main.{main57}1}
    )

# Main.{main59}
def _idris_Main_46__123_main59_125_(in25, in39):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Lib_46_Threading_46_forkPIO(None, APPLY0(in25, "B")),
      (65706, in39)  # {U_Main.{main58}1}
    )

# Main.{main60}
def _idris_Main_46__123_main60_125_(in1, in24):
  while True:
    in25 = (65690, in1)  # {U_Main.{main43}1}
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Lib_46_Threading_46_forkPIO(None, APPLY0(in25, "A")),
      (65707, in25)  # {U_Main.{main59}1}
    )

# Main.{main61}
def _idris_Main_46__123_main61_125_(in1, in23):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(None, "\n"),
      (65709, in1)  # {U_Main.{main60}1}
    )

# Main.{main62}
def _idris_Main_46__123_main62_125_(in1, in22):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        (("Total number of features: " + _idris_Prelude_46_Show_46_primNumShow(None, (65804,), (0,), in22)) + "\n")  # {U_prim__toStrInt1}, Prelude.Show.Open
      ),
      (65710, in1)  # {U_Main.{main61}1}
    )

# Main.{main63}
def _idris_Main_46__123_main63_125_(in9, in1, in10):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Prim_46_iterate(None, None, None, in9, 0, (65658,), None),  # {U_Main.{main14}1}
      (65711, in1)  # {U_Main.{main62}1}
    )

# Main.{main64}
def _idris_Main_46__123_main64_125_(in1, in9):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        "Idris has got the following exciting features:\n"
      ),
      (65712, in9, in1)  # {U_Main.{main63}1}
    )

# Main.{main65}
def _idris_Main_46__123_main65_125_(in1, in7):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Functions_46_call(
        None,
        None,
        (1, (0,), (65686,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main3}1}
        _idris_Python_46_Fields_46__47__46_(None, None, in7, "select", None),
        None,
        (0, "div.entry-content li", Unit)  # Builtins.MkSigma
      ),
      (65713, in1)  # {U_Main.{main64}1}
    )

# Main.{main66}
def _idris_Main_46__123_main66_125_(in3, in1, in4):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Functions_46_call(
        None,
        None,
        (1, (0,), (65675,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main2}1}
        _idris_Python_46_Fields_46__47__46_(None, None, in4, "BeautifulSoup", None),
        None,
        (0, in3, (0, "html.parser", Unit))  # Builtins.MkSigma, Builtins.MkSigma
      ),
      (65714, in1)  # {U_Main.{main65}1}
    )

# Main.{main67}
def _idris_Main_46__123_main67_125_(in1, in3):
  while True:
    return (65801, None, None, None, (65799, None, "bs4"), (65715, in3, in1))  # {U_io_bind1}, {U_Python.importModule1}, {U_Main.{main66}1}

# Main.{main68}
def _idris_Main_46__123_main68_125_(in1):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Fields_46__47__58_(
        None,
        None,
        _idris_Python_46_Functions_46_call(
          None,
          None,
          (1, (0,), (65653,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Main.{main0}1}
          _idris_Python_46_Fields_46__47__46_(None, None, in1, "get", None),
          None,
          (0, "http://idris-lang.org", Unit)  # Builtins.MkSigma
        ),
        "text",
        None
      ),
      (65716, in1)  # {U_Main.{main67}1}
    )

# Main.{main69}
def _idris_Main_46__123_main69_125_(in0):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Functions_46_call(
        None,
        None,
        (0,),  # Python.Telescope.Return
        _idris_Python_46_Fields_46__47__46_(None, None, in0, "Session", None),
        None,
        Unit
      ),
      (65717,)  # {U_Main.{main68}1}
    )

# Main.exports, greet
def _idris_Main_46_exports_58_greet_58_0(e0):
  while True:
    return _idris_Prelude_46_Interactive_46_putStr_39_(
      None,
      (("Hello " + (e0 + "!")) + "\n")
    )

# Python.Lib.Threading.forkPIO, worker
def _idris_Python_46_Lib_46_Threading_46_forkPIO_58_worker_58_0(e0, e1, e2):
  while True:
    return (65801, None, None, None, e1, (65808, e2))  # {U_io_bind1}, {U_{Python.Lib.Threading.forkPIO, worker_lam1}1}

# Python.Prim.iterate, iter
def _idris_Python_46_Prim_46_iterate_58_iter_58_0(
  e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10
):
  while True:
    return (
      65801,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46_Prim_46_next(None, e8),
      (65813, e10, e9, e8)  # {U_{Python.Prim.iterate, iter_lam13}1}
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

# Decidable.Equality.Decidable.Equality.Char instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Char_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Decidable.Equality.Decidable.Equality.Int instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Int_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Decidable.Equality.Decidable.Equality.Integer instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Integer_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Decidable.Equality.Decidable.Equality.ManagedPtr instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_ManagedPtr_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Decidable.Equality.Decidable.Equality.Ptr instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Ptr_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Decidable.Equality.Decidable.Equality.String instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_String_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Decidable.Equality.Decidable.Equality.Bool instance of Decidable.Equality.DecEq, method decEq
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

# Prelude.Classes.Prelude.Nat.Nat instance of Prelude.Classes.Eq, method ==
def _idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Eq_36_Nat_58__33__61__61__58_0(
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
        return APPLY0(APPLY0(_idris_Prelude_46_Classes_46__61__61_(None, (65826,)), in1), in0)  # {U_Prelude.Nat.Nat instance of Prelude.Classes.Eq2}
      return _idris_error("unreachable due to case in tail position")
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.Prelude.Show.Prec instance of Prelude.Classes.Eq, method ==
def _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Eq_36_Prec_58__33__61__61__58_0(
  e0, e1
):
  while True:
    if e1[0] == 4:  # Prelude.Show.User
      in0 = e1[1]
      if e0[0] == 4:  # Prelude.Show.User
        in1 = e0[1]
        return _idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Eq_36_Nat_58__33__61__61__58_0(
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

# Prelude.Foldable.Prelude.List.List instance of Prelude.Foldable.Foldable, method foldr
def _idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
  e0, e1, e2, e3, e4
):
  while True:
    if e4:  # Prelude.List.::
      in0, in1 = e4.head, e4.tail
      return APPLY0(
        APPLY0(e2, in0),
        APPLY0(
          APPLY0(
            APPLY0(_idris_Prelude_46_Foldable_46_foldr(None, None, None, (65829,)), e2),  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable5}
            e3
          ),
          in1
        )
      )
    else:  # Prelude.List.Nil
      return e3
    return _idris_error("unreachable due to case in tail position")

# Prelude.Functor.Prelude.Monad.IO' ffi instance of Prelude.Functor.Functor, method map
def _idris_Prelude_46_Functor_46_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
  e0, e1, e2, e3, e4
):
  while True:
    return (65801, None, None, None, e4, (65724, e3))  # {U_io_bind1}, {U_Prelude.Functor.{Prelude.Monad.IO' ffi instance of Prelude.Functor.Functor, method map_lam0}1}

# Prelude.Classes.Prelude.Classes.Integer instance of Prelude.Classes.Ord, method compare
def _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Integer_58__33_compare_58_0(
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
        return (2,)  # Prelude.Classes.GT
      else:  # Prelude.Bool.True
        return (0,)  # Prelude.Classes.LT
      return _idris_error("unreachable due to case in tail position")
    else:  # Prelude.Bool.True
      return (1,)  # Prelude.Classes.EQ
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.Prelude.Nat.Nat instance of Prelude.Classes.Ord, method compare
def _idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat_58__33_compare_58_0(
  e0, e1
):
  while True:
    if e1 == 0:
      if e0 == 0:
        return (1,)  # Prelude.Classes.EQ
      else:
        in0 = (e0 - 1)
        return (2,)  # Prelude.Classes.GT
      return _idris_error("unreachable due to case in tail position")
    else:
      in1 = (e1 - 1)
      if e0 == 0:
        return (0,)  # Prelude.Classes.LT
      else:
        in2 = (e0 - 1)
        return APPLY0(
          APPLY0(
            _idris_Prelude_46_Classes_46_compare(
              None,
              _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat()
            ),
            in2
          ),
          in1
        )
      return _idris_error("unreachable due to case in tail position")
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.Prelude.Show.Prec instance of Prelude.Classes.Ord, method >=
def _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33__62__61__58_0(
  e0, e1
):
  while True:
    aux1 = APPLY0(
      APPLY0(
        _idris_Prelude_46_Classes_46__62_(
          None,
          _idris_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec()
        ),
        e0
      ),
      e1
    )
    if not aux1:  # Prelude.Bool.False
      return _idris_Prelude_46_Classes_46__123_Prelude_46_Show_46_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__62__61__95_lam0_125_(
        e0, e1
      )
    else:  # Prelude.Bool.True
      return True
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.Prelude.Show.Prec instance of Prelude.Classes.Ord, method compare
def _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33_compare_58_0(
  e0, e1
):
  while True:
    if e1[0] == 4:  # Prelude.Show.User
      in0 = e1[1]
      if e0[0] == 4:  # Prelude.Show.User
        in1 = e0[1]
        return _idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat_58__33_compare_58_0(
          in1, in0
        )
      else:
        return _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Integer_58__33_compare_58_0(
          _idris_Prelude_46_Show_46_precCon(e0),
          _idris_Prelude_46_Show_46_precCon(e1)
        )
      return _idris_error("unreachable due to case in tail position")
    else:
      return _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Integer_58__33_compare_58_0(
        _idris_Prelude_46_Show_46_precCon(e0),
        _idris_Prelude_46_Show_46_precCon(e1)
      )
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.Prelude.Show.Nat instance of Prelude.Show.Show, method show
def _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_Nat_58__33_show_58_0(
  e0
):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (65803,), (0,), e0)  # {U_prim__toStrBigInt1}, Prelude.Show.Open

# with block in Prelude.Strings.strM
def _idris__95_Prelude_46_Strings_46_strM_95_with_95_21(e0, e1):
  while True:
    if e1[0] == 1:  # Prelude.Basics.No
      return _idris_really_95_believe_95_me(None, None, (0,))  # Prelude.Strings.StrNil
    else:  # Prelude.Basics.Yes
      return _idris_really_95_believe_95_me(None, None, (1, e0[0]))  # Prelude.Strings.StrCons
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Classes.Prelude.Show.Prec instance of Prelude.Classes.Ord, method >
def _idris__95_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33__62__58_0_95_with_95_27(
  e0, e1, e2
):
  while True:
    if e0[0] == 2:  # Prelude.Classes.GT
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

# with block in Prelude.Classes.Prelude.Nat.Nat instance of Prelude.Classes.Ord, method >
def _idris__95_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat_58__33__62__58_0_95_with_95_84(
  e0, e1, e2
):
  while True:
    if e0[0] == 2:  # Prelude.Classes.GT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# Prelude.Nat.Nat instance of Prelude.Classes.Eq
def _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Eq_36_Nat(meth0, meth1):
  while True:
    return _idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Eq_36_Nat_58__33__61__61__58_0(
      meth0, meth1
    )

# Prelude.List.List instance of Prelude.Foldable.Foldable
def _idris_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List(
  meth0, meth1, meth2, meth3, meth4
):
  while True:
    return _idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
      None, None, meth2, meth3, meth4
    )

# Prelude.Nat.Nat instance of Prelude.Classes.Ord
def _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat():
  while True:
    return (0, (65731,), (65733,))  # constructor of Prelude.Classes.Ord, {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam1}1}, {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam3}1}

# Prelude.Show.Prec instance of Prelude.Classes.Ord
def _idris_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec():
  while True:
    return (0, (65735,), (65737,))  # constructor of Prelude.Classes.Ord, {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam1}1}, {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam3}1}

# Python.Exceptions.case block in fromString at ./Python/Exceptions.idr:56:21
def _idris_Python_46_Exceptions_46_fromString_95__95__95__95__95_Python_95__95_Exceptions_95__95_idr_95_56_95_21_95_case(
  e0, e1
):
  while True:
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

# Python.Exceptions.case block in try at ./Python/Exceptions.idr:106:16
def _idris_Python_46_Exceptions_46_try_95__95__95__95__95_Python_95__95_Exceptions_95__95_idr_95_106_95_16_95_case(
  e0, e1, e2, e3, e4
):
  while True:
    if e3[0] == 0:  # Prelude.Either.Left
      in0 = e3[1]
      assert e2[0] == 0  # constructor of Prelude.Monad.Monad
      in1, in2 = e2[1:]
      aux1 = in1
      aux2 = _idris_Python_46_Fields_46__47__46_(
        None,
        None,
        _idris_Python_46_Fields_46__47__46_(None, None, in0, "__class__", None),
        "__name__",
        None
      )
      return APPLY0(
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
          }.get(aux2, (35,)),  # Python.Exceptions.Other
          in0
        )
      )
    else:  # Prelude.Either.Right
      in3 = e3[1]
      assert e2[0] == 0  # constructor of Prelude.Monad.Monad
      in4, in5 = e2[1:]
      aux3 = in4
      return APPLY0(_idris_Prelude_46_Applicative_46_pure(None, None, aux3), (0, in3))  # Python.Exceptions.OK
    return _idris_error("unreachable due to case in tail position")

# Python.Exceptions.case block in case block in try at ./Python/Exceptions.idr:106:16 at ./Python/Exceptions.idr:118:10
def _idris_Python_46_Exceptions_46_try_95__95__95__95__95_Python_95__95_Exceptions_95__95_idr_95_106_95_16_95_case_95__95__95__95__95_Python_95__95_Exceptions_95__95_idr_95_118_95_10_95_case(
  e0, e1, e2, e3, e4, e5
):
  while True:
    if e2[0] == 0:  # Prelude.Either.Left
      in0 = e2[1]
      assert e4[0] == 0  # constructor of Prelude.Monad.Monad
      in1, in2 = e4[1:]
      aux1 = in1
      aux2 = _idris_Python_46_Fields_46__47__46_(
        None,
        None,
        _idris_Python_46_Fields_46__47__46_(None, None, in0, "__class__", None),
        "__name__",
        None
      )
      return APPLY0(
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
          }.get(aux2, (35,)),  # Python.Exceptions.Other
          in0
        )
      )
    else:  # Prelude.Either.Right
      in3 = e2[1]
      assert e4[0] == 0  # constructor of Prelude.Monad.Monad
      in4, in5 = e4[1:]
      aux3 = in4
      return APPLY0(_idris_Prelude_46_Applicative_46_pure(None, None, aux3), (0, in3))  # Python.Exceptions.OK
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
      assert e3[0] == 0  # constructor of Prelude.Monad.Monad
      in3, in4 = e3[1:]
      aux1 = in3
      return APPLY0(_idris_Prelude_46_Applicative_46_pure(None, None, aux1), in2)
    return _idris_error("unreachable due to case in tail position")

# Python.Prim.case block in next at ./Python/Prim.idr:61:11
def _idris_Python_46_Prim_46_next_95__95__95__95__95_Python_95__95_Prim_95__95_idr_95_61_95_11_95_case(
  e0, e1, e2, e3, e4
):
  while True:
    if e3[0] == 1:  # Python.Exceptions.Except
      in0, in1 = e3[1:]
      if in0[0] == 0:  # Python.Exceptions.StopIteration
        assert e2[0] == 0  # constructor of Prelude.Monad.Monad
        in2, in3 = e2[1:]
        aux1 = in2
        return APPLY0(_idris_Prelude_46_Applicative_46_pure(None, None, aux1), None)
      else:
        return _idris_Python_46_Exceptions_46_raise(None, in1)
      return _idris_error("unreachable due to case in tail position")
    else:  # Python.Exceptions.OK
      in4 = e3[1]
      assert e2[0] == 0  # constructor of Prelude.Monad.Monad
      in5, in6 = e2[1:]
      aux2 = in5
      return APPLY0(_idris_Prelude_46_Applicative_46_pure(None, None, aux2), in4)
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
          _idris_Prelude_46_Monad_46__62__62__61_(None, None, None, e11),
          APPLY0(APPLY0(e10, e9), in0)
        ),
        (65780, e8, e10)  # {U_Python.Prim.{case block in Python.Prim.iterate, iter at ./Python/Prim.idr:84:17_lam0}1}
      )
    else:  # Prelude.Maybe.Nothing
      assert e11[0] == 0  # constructor of Prelude.Monad.Monad
      in2, in3 = e11[1:]
      aux1 = in2
      return APPLY0(_idris_Prelude_46_Applicative_46_pure(None, None, aux1), e9)
    return _idris_error("unreachable due to case in tail position")

# case block in io_bind at IO.idr:109:34
def _idris_io_95_bind_95_IO_95__95_idr_95_109_95_34_95_case(
  e0, e1, e2, e3, e4, e5, e6, e7
):
  while True:
    return APPLY0(e7, e5)

# Main.case block in main at example.idr:59:44
def _idris_Main_46_main_95_example_95__95_idr_95_59_95_44_95_case(
  e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14,
  e15, e16, e17, e18, e19, e20, e21
):
  while True:
    return APPLY0(
      APPLY0(
        _idris_Prelude_46_Monad_46__62__62__61_(None, None, None, e18),
        _idris_Prelude_46_Interactive_46_putStr_39_(
          None,
          (("thread " + (e20 + " starting")) + "\n")
        )
      ),
      (65652, e18, e3, e20)  # {U_Main.{case block in main at example.idr:59:44_lam3}1}
    )

# Main.case block in main at example.idr:84:32
def _idris_Main_46_main_95_example_95__95_idr_95_84_95_32_95_case(
  e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14,
  e15, e16, e17, e18, e19, e20, e21, e22, e23, e24, e25, e26, e27,
  e28, e29, e30, e31, e32, e33, e34, e35, e36, e37, e38, e39, e40,
  e41, e42, e43, e44
):
  while True:
    if e42[0] == 11:  # Python.Exceptions.OSError
      return _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        (("  -> (1) everything's fine: " + _idris_Python_46_Exceptions_46_showException(e43)) + "\n")
      )
    else:
      return _idris_Python_46_Exceptions_46_raise(None, e43)
    return _idris_error("unreachable due to case in tail position")

# Main.case block in main at example.idr:90:13
def _idris_Main_46_main_95_example_95__95_idr_95_90_95_13_95_case(
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
          (("  -> (2) everything's fine: " + _idris_Python_46_Exceptions_46_showException(in1)) + "\n")
        )
      else:
        return _idris_Python_46_Exceptions_46_raise(None, in1)
      return _idris_error("unreachable due to case in tail position")
    else:  # Python.Exceptions.OK
      in2 = e44[1]
      return _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        "Your root could probably use some security lessons!\n"
      )
    return _idris_error("unreachable due to case in tail position")

# case block in Void
def _idris_Void_95_case():
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
