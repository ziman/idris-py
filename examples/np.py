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
      (65680, None),  # {U_Python.IO.unRaw1}
      (65679, e3, e2, e5)  # {U_Python.Functions.{$.0}1}
    )

# Prelude.Basics..
def _idris_Prelude_46_Basics_46__46_(e0, e1, e2, e3, e4, _idris_x):
  while True:
    return APPLY0(e3, APPLY0(e4, _idris_x))

# Python.Fields./.
def _idris_Python_46_Fields_46__47__46_(e0, e1, e2, e3, e4):
  while True:
    return _idris_unsafePerformIO(None, None, (65677, e2, e3))  # {U_Python.Fields.{/.0}1}

# Python.Fields.//.
def _idris_Python_46_Fields_46__47__47__46_(e0, e1, e2, e3, e4, e5):
  while True:
    return _idris_unsafePerformIO(None, None, (65678, e3, e4))  # {U_Python.Fields.{//.0}1}

# Force
def _idris_Force(e0, e1, e2):
  while True:
    return EVAL0(e2)

# PE_<$>_cc6adb39
def _idris_PE_95__60__36__62__95_cc6adb39(e0, e1, e2, e3):
  while True:
    return _idris_PE_95_map_95_1b3102a6(None, None, e2, e3)

# PE_Prelude.Show.List a implementation of Prelude.Show.Show, method showPrec_32c264fb
def _idris_PE_95_Prelude_46_Show_46_List_32_a_32_implementation_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_showPrec_95_32c264fb(
  e0, e1
):
  while True:
    return _idris_PE_95_Prelude_46_Show_46_List_32_a_32_implementation_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_d2fe2214(
      e1
    )

# PE_Prelude.Show.List a implementation of Prelude.Show.Show, method show_d2fe2214
def _idris_PE_95_Prelude_46_Show_46_List_32_a_32_implementation_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_d2fe2214(
  e0
):
  while True:
    return (u'[' + (_idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
      None,
      None,
      (0, (65709,), (65711,)),  # constructor of Prelude.Show.Show, {U_{PE_Prelude.Show.List a implementation of Prelude.Show.Show, method show_d2fe22140}1}, {U_{PE_Prelude.Show.List a implementation of Prelude.Show.Show, method show_d2fe22142}1}
      u'',
      e0
    ) + u']'))

# PE_map_1b3102a6
def _idris_PE_95_map_95_1b3102a6(e0, e1, e2, e3):
  while True:
    return _idris_Prelude_46_Functor_46_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None, None, None, e2, e3
    )

# PE_printLn'_e0c531f5
def _idris_PE_95_printLn_39__95_e0c531f5(e0, e1):
  while True:
    return (65705, None, None, None, (65712, e1), (65713,))  # {U_io_bind1}, {U_{PE_printLn'_e0c531f50}1}, {U_{PE_printLn'_e0c531f51}1}

# PE_show_408b7599
def _idris_PE_95_show_95_408b7599(e0):
  while True:
    if e0 is not None:  # Prelude.Maybe.Just
      in3 = e0
      aux1 = _idris_Prelude_46_Interfaces_46_Prelude_46_Show_46__64_Prelude_46_Interfaces_46_Ord_36_Prec_58__33__62__61__58_0(
        (0,), (6,)  # Prelude.Show.Open, Prelude.Show.App
      )
      if not aux1:  # Prelude.Bool.False
        return (u'Just' + (u' ' + APPLY0(
          APPLY0(_idris_Prelude_46_Show_46_showPrec(None, (0, (65714,), (65716,))), None),  # constructor of Prelude.Show.Show, {U_{PE_show_408b75990}1}, {U_{PE_show_408b75992}1}
          in3
        )))
      else:  # Prelude.Bool.True
        return (u'(' + ((u'Just' + (u' ' + APPLY0(
          APPLY0(_idris_Prelude_46_Show_46_showPrec(None, (0, (65717,), (65719,))), None),  # constructor of Prelude.Show.Show, {U_{PE_show_408b75993}1}, {U_{PE_show_408b75995}1}
          in3
        ))) + u')'))
      return _idris_error("unreachable due to case in tail position")
    else:  # Prelude.Maybe.Nothing
      return u'Nothing'
    return _idris_error("unreachable due to case in tail position")

# Python.Lib.Numpy.Matrix.array
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46_array(e0, e1, e2, e3, e4):
  while True:
    assert e3[0] == 0  # Python.Lib.Numpy.Matrix.MkDType
    in7, in8, in9 = e3[1:]
    aux1 = in7
    return APPLY0(
      _idris_Python_46_Lib_46_Numpy_46_Matrix_46_unsafeNp(None, None, None, None),
      _idris_Python_46_Functions_46__36__46_(
        None,
        None,
        (1, (0,), (65685,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Numpy.Matrix.{array1}1}
        _idris_Python_46_Fields_46__47__47__46_(
          None,
          None,
          None,
          _idris_unsafePerformIO(None, None, (65703, None, u'numpy')),  # {U_Python.importModule1}
          u'array',
          None
        ),
        None,
        (
          0,  # Builtins.MkDPair
          _idris_Python_46_Prim_46_pyList(
            None,
            APPLY0(
              _idris_Prelude_46_List_46_toList(None, None, (65690,)),  # {U_Python.Lib.Numpy.Matrix.{array6}1}
              _idris_Prelude_46_Functor_46_Data_46_Vect_46__64_Prelude_46_Functor_46_Functor_36_Vect_32_n_58__33_map_58_0(
                None,
                None,
                None,
                (65681, None, None, None, None, None, None, None),  # {U_Python.Lib.Numpy.Matrix.array, c1}
                e4
              )
            )
          ),
          (0, aux1, Unit)  # Builtins.MkDPair
        )
      )
    )

# assert_unreachable
def _idris_assert_95_unreachable():
  while True:
    return None

# call__IO
def _idris_call_95__95_IO(e0, e1, e2):
  while True:
    return APPLY0(e2, None)

# Python.Lib.Numpy.Matrix.dot
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46_dot(e0, e1, e2, e3, e4, e5, e6):
  while True:
    return APPLY0(
      _idris_Python_46_Lib_46_Numpy_46_Matrix_46_unsafeNp(None, None, None, None),
      _idris_Python_46_Functions_46__36__46_(
        None,
        None,
        (1, (0,), (65692,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Numpy.Matrix.{dot1}1}
        _idris_Python_46_Fields_46__47__46_(
          None,
          None,
          _idris_unsafePerformIO(None, None, (65703, None, u'numpy')),  # {U_Python.importModule1}
          u'dot',
          None
        ),
        None,
        (0, e5, (0, e6, Unit))  # Builtins.MkDPair, Builtins.MkDPair
      )
    )

# Python.Lib.Numpy.Matrix.DType.dtFromDouble
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46_DType_46_dtFromDouble(e0, e1):
  while True:
    assert e1[0] == 0  # Python.Lib.Numpy.Matrix.MkDType
    in0, in1, in2 = e1[1:]
    return in2
    return _idris_error("unreachable due to case in tail position")

# Python.Lib.Numpy.Matrix.DType.dtFromInteger
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46_DType_46_dtFromInteger(e0, e1):
  while True:
    assert e1[0] == 0  # Python.Lib.Numpy.Matrix.MkDType
    in0, in1, in2 = e1[1:]
    return in1
    return _idris_error("unreachable due to case in tail position")

# Python.Lib.Numpy.Matrix.DType.dtName
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46_DType_46_dtName(e0, e1):
  while True:
    assert e1[0] == 0  # Python.Lib.Numpy.Matrix.MkDType
    in0, in1, in2 = e1[1:]
    return in0
    return _idris_error("unreachable due to case in tail position")

# Main.f
def _idris_Main_46_f(e0):
  while True:
    if e0 == 0:
      return ConsList().cons(False).cons(True)
    else:
      in0 = (e0 - 1)
      e0, = in0,
      continue
      return _idris_error("unreachable due to tail call")
    return _idris_error("unreachable due to case in tail position")

# Python.Lib.Numpy.Matrix.fill
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46_fill(e0, e1, e2, e3, e4):
  while True:
    return APPLY0(
      _idris_Python_46_Lib_46_Numpy_46_Matrix_46_unsafeNp(None, None, None, None),
      _idris_Python_46_Functions_46__36__46_(
        None,
        None,
        (1, (0,), (65694,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Numpy.Matrix.{fill1}1}
        _idris_Python_46_Fields_46__47__46_(
          None,
          None,
          _idris_unsafePerformIO(None, None, (65703, None, u'numpy')),  # {U_Python.importModule1}
          u'tile',
          None
        ),
        None,
        (0, e4, (0, (e1, e0), Unit))  # Builtins.MkDPair, Builtins.MkDPair
      )
    )

# Prelude.Foldable.foldr
def _idris_Prelude_46_Foldable_46_foldr(e0, e1, e2, e3):
  while True:
    return APPLY0(APPLY0(e1, e2), e3)

# Data.Vect.foldrImpl
def _idris_Data_46_Vect_46_foldrImpl(e0, e1, e2, e3, e4, e5, e6):
  while True:
    if e6[0] == 1:  # Data.Vect.::
      in0, in1 = e6[1:]
      e0, e1, e2, e3, e4, e5, e6, = None, None, None, e3, e4, (65671, None, None, None, e5, APPLY0(e3, in0)), in1,  # {U_Prelude.Basics..1}
      continue
      return _idris_error("unreachable due to tail call")
    else:  # Data.Vect.Nil
      return APPLY0(e5, e4)
    return _idris_error("unreachable due to case in tail position")

# Python.Lib.Numpy.Matrix.fromDouble
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46_fromDouble(e0, e1, e2, e3):
  while True:
    assert e1[0] == 0  # Python.Lib.Numpy.Matrix.MkDType
    in0, in1, in2 = e1[1:]
    aux1 = in2
    return (65671, None, None, None, (65682, e2, e3, None, None), aux1)  # {U_Prelude.Basics..1}, {U_Python.Lib.Numpy.Matrix.fill1}

# Python.Lib.Numpy.Matrix.fromInteger
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46_fromInteger(e0, e1, e2, e3):
  while True:
    assert e1[0] == 0  # Python.Lib.Numpy.Matrix.MkDType
    in0, in1, in2 = e1[1:]
    aux1 = in1
    return (65671, None, None, None, (65682, e2, e3, None, None), aux1)  # {U_Prelude.Basics..1}, {U_Python.Lib.Numpy.Matrix.fill1}

# Prelude.Maybe.fromMaybe
def _idris_Prelude_46_Maybe_46_fromMaybe(e0, e1, e2):
  while True:
    if e2 is not None:  # Prelude.Maybe.Just
      in0 = e2
      return in0
    else:  # Prelude.Maybe.Nothing
      return EVAL0(e1)
    return _idris_error("unreachable due to case in tail position")

# Python.getGlobal
def _idris_Python_46_getGlobal(e0, e1):
  while True:
    return _idris_unsafePerformIO(None, None, (65704, e1))  # {U_Python.{getGlobal0}1}

# Prelude.Basics.id
def _idris_Prelude_46_Basics_46_id(e0, e1):
  while True:
    return e1

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

# Main.main
def _idris_Main_46_main():
  while True:
    return (
      65705,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_PE_95_printLn_39__95_e0c531f5(None, _idris_Main_46_f(4)),
      (65669,)  # {U_Main.{main8}1}
    )

# mkForeignPrim
def _idris_mkForeignPrim():
  while True:
    return None

# Python.Lib.Numpy.Matrix.nda
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46_nda():
  while True:
    return _idris_Python_46_Fields_46__47__46_(
      None,
      None,
      _idris_unsafePerformIO(None, None, (65703, None, u'numpy')),  # {U_Python.importModule1}
      u'ndarray',
      None
    )

# Python.Lib.Numpy.Matrix.op
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46_op(e0, e1, e2, e3, e4, e5, e6, e7):
  while True:
    return APPLY0(
      _idris_Python_46_Lib_46_Numpy_46_Matrix_46_unsafeNp(None, None, None, None),
      _idris_Python_46_Functions_46__36__46_(
        None,
        None,
        (1, (0,), (65696,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Numpy.Matrix.{op1}1}
        _idris_Python_46_Fields_46__47__46_(
          None,
          None,
          _idris_Python_46_Lib_46_Numpy_46_Matrix_46_nda(),
          e4,
          None
        ),
        None,
        (0, e6, (0, e7, Unit))  # Builtins.MkDPair, Builtins.MkDPair
      )
    )

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

# prim__eqManagedPtr
def _idris_prim_95__95_eqManagedPtr(op0, op1):
  while True:
    return _idris_error("unimplemented external: prim__eqManagedPtr")

# prim__eqPtr
def _idris_prim_95__95_eqPtr(op0, op1):
  while True:
    return _idris_error("unimplemented external: prim__eqPtr")

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

# prim__toFloatBigInt
def _idris_prim_95__95_toFloatBigInt(op0):
  while True:
    return float(op0)

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

# prim_io_bind
def _idris_prim_95_io_95_bind(e0, e1, e2, e3):
  while True:
    return APPLY0(e3, e2)

# prim_write
def _idris_prim_95_write(e0, e1, _idris_w):
  while True:
    return sys.stdout.write(e1)

# Prelude.Interactive.putStr'
def _idris_Prelude_46_Interactive_46_putStr_39_(e0, e1):
  while True:
    return (65705, None, None, None, (65707, None, e1), (65674,))  # {U_io_bind1}, {U_prim_write1}, {U_Prelude.Interactive.{putStr'0}1}

# Python.Prim.pyList
def _idris_Python_46_Prim_46_pyList(e0, e1):
  while True:
    return _idris_unsafePerformIO(
      None,
      None,
      _idris_Python_46_Functions_46__36__46_(
        None,
        None,
        (1, (1,), (65702,)),  # Python.Telescope.Bind, Python.Telescope.Forall, {U_Python.Prim.{pyList1}1}
        _idris_Python_46_Fields_46__47__46_(
          None,
          None,
          _idris_Python_46_getGlobal(None, u'__builtins__'),
          u'list',
          None
        ),
        None,
        (0, (0,), (0, e1, Unit))  # Builtins.MkDPair, Data.Erased.Erase, Builtins.MkDPair
      )
    )

# Python.Lib.Numpy.Matrix.reshape
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46_reshape(
  e0, e1, e2, e3, e4, e5, e6, e7
):
  while True:
    return APPLY0(
      _idris_Python_46_Lib_46_Numpy_46_Matrix_46_unsafeNp(None, None, None, None),
      _idris_Python_46_Functions_46__36__46_(
        None,
        None,
        (1, (0,), (65698,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Numpy.Matrix.{reshape1}1}
        _idris_Python_46_Fields_46__47__46_(
          None,
          None,
          _idris_unsafePerformIO(None, None, (65703, None, u'numpy')),  # {U_Python.importModule1}
          u'reshape',
          None
        ),
        None,
        (0, e6, (0, (e3, e2), Unit))  # Builtins.MkDPair, Builtins.MkDPair
      )
    )

# run__IO
def _idris_run_95__95_IO(e0, e1):
  while True:
    return APPLY0(e1, None)

# Prelude.Show.show
def _idris_Prelude_46_Show_46_show(e0, e1):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Show.Show
    in0, in1 = e1[1:]
    return in0
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.showParens
def _idris_Prelude_46_Show_46_showParens(e0, e1):
  while True:
    if not e0:  # Prelude.Bool.False
      return e1
    else:  # Prelude.Bool.True
      return (u'(' + (e1 + u')'))
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.showPrec
def _idris_Prelude_46_Show_46_showPrec(e0, e1):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Show.Show
    in0, in1 = e1[1:]
    return in1
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

# Prelude.List.toList
def _idris_Prelude_46_List_46_toList(e0, e1, e2):
  while True:
    return APPLY0(
      APPLY0(_idris_Prelude_46_Foldable_46_foldr(None, e2, None, None), (65676,)),  # {U_Prelude.List.{toList1}1}
      ConsList()
    )

# Python.Lib.Numpy.Matrix.transpose
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46_transpose(e0, e1, e2, e3, e4):
  while True:
    return APPLY0(
      _idris_Python_46_Lib_46_Numpy_46_Matrix_46_unsafeNp(None, None, None, None),
      _idris_Python_46_Functions_46__36__46_(
        None,
        None,
        (1, (0,), (65699,)),  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Numpy.Matrix.{transpose0}1}
        _idris_Python_46_Fields_46__47__46_(
          None,
          None,
          _idris_unsafePerformIO(None, None, (65703, None, u'numpy')),  # {U_Python.importModule1}
          u'transpose',
          None
        ),
        None,
        (0, e4, Unit)  # Builtins.MkDPair
      )
    )

# Python.IO.unRaw
def _idris_Python_46_IO_46_unRaw(e0, e1):
  while True:
    return e1

# Python.Lib.Numpy.Matrix.unsafeNp
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46_unsafeNp(e0, e1, e2, e3):
  while True:
    return (65671, None, None, None, (65700,), (65708, None, None))  # {U_Prelude.Basics..1}, {U_Python.Lib.Numpy.Matrix.{unsafeNp0}1}, {U_unsafePerformIO1}

# unsafePerformIO
def _idris_unsafePerformIO(e0, e1, e2):
  while True:
    return APPLY0(unsafePerformIO1(e0, e1, e2), APPLY0(e2, None))

# unsafePerformPrimIO
def _idris_unsafePerformPrimIO():
  while True:
    return None

# world
def _idris_world(e0):
  while True:
    return e0

# Main.xs
def _idris_Main_46_xs():
  while True:
    return _idris_Python_46_Lib_46_Numpy_46_Matrix_46_array(
      None,
      None,
      None,
      (0, u'float', (65670,), (65672, None)),  # Python.Lib.Numpy.Matrix.MkDType, {U_Main.{xs0}1}, {U_Prelude.Basics.id1}
      (
        1,  # Data.Vect.::
        (1, 1.0, (1, -2.1, (1, 3.3, (1, -0.1, (0,))))),  # Data.Vect.::, Data.Vect.::, Data.Vect.::, Data.Vect.::, Data.Vect.Nil
        (
          1,  # Data.Vect.::
          (1, 3.5, (1, 7.0, (1, 0.0, (1, -5.2, (0,))))),  # Data.Vect.::, Data.Vect.::, Data.Vect.::, Data.Vect.::, Data.Vect.Nil
          (1, (1, 0.5, (1, 7.2, (1, -1.1, (1, 0.0, (0,))))), (0,))  # Data.Vect.::, Data.Vect.::, Data.Vect.::, Data.Vect.::, Data.Vect.::, Data.Vect.Nil, Data.Vect.Nil
        )
      )
    )

# Main.ys
def _idris_Main_46_ys():
  while True:
    return _idris_Python_46_Lib_46_Numpy_46_Matrix_46_reshape(
      None,
      None,
      2,
      6,
      None,
      None,
      _idris_Main_46_xs(),
      None
    )

# Main.zs
def _idris_Main_46_zs():
  while True:
    return _idris_Python_46_Lib_46_Numpy_46_Matrix_46_reshape(
      None,
      None,
      3,
      4,
      None,
      None,
      _idris_Main_46_ys(),
      None
    )

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

# Python.Fields.{/.0}
def _idris_Python_46_Fields_46__123__47__46_0_125_(e2, e3, in0):
  while True:
    return getattr(e2, e3)

# Python.Fields.{//.0}
def _idris_Python_46_Fields_46__123__47__47__46_0_125_(e3, e4, in0):
  while True:
    return getattr(e3, e4)

# {APPLY0}
def APPLY0(fn0, arg0):
  while True:
    if fn0[0] < 65695:
      if fn0[0] < 65678:
        if fn0[0] < 65669:
          if fn0[0] < 65665:
            if fn0[0] < 65663:
              if fn0[0] == 65661:  # {U_Main.{main0}1}
                return _idris_Main_46__123_main0_125_(arg0)
              else:  # {U_Main.{main1}1}
                return _idris_Main_46__123_main1_125_(arg0)
            else:
              if fn0[0] == 65663:  # {U_Main.{main2}1}
                return _idris_Main_46__123_main2_125_(arg0)
              else:  # {U_Main.{main3}1}
                return _idris_Main_46__123_main3_125_(arg0)
          else:
            if fn0[0] < 65667:
              if fn0[0] == 65665:  # {U_Main.{main4}1}
                return _idris_Main_46__123_main4_125_(arg0)
              else:  # {U_Main.{main5}1}
                return _idris_Main_46__123_main5_125_(arg0)
            else:
              if fn0[0] == 65667:  # {U_Main.{main6}1}
                return _idris_Main_46__123_main6_125_(arg0)
              else:  # {U_Main.{main7}1}
                return _idris_Main_46__123_main7_125_(arg0)
        else:
          if fn0[0] < 65673:
            if fn0[0] < 65671:
              if fn0[0] == 65669:  # {U_Main.{main8}1}
                return _idris_Main_46__123_main8_125_(arg0)
              else:  # {U_Main.{xs0}1}
                return _idris_Main_46__123_xs0_125_(arg0)
            else:
              if fn0[0] == 65671:  # {U_Prelude.Basics..1}
                P_c0, P_c1, P_c2, P_c3, P_c4 = fn0[1:]
                return _idris_Prelude_46_Basics_46__46_(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
              else:  # {U_Prelude.Basics.id1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Basics_46_id(P_c0, arg0)
          else:
            if fn0[0] < 65675:
              if fn0[0] == 65673:  # {U_Prelude.Functor.{Prelude.Monad.@Prelude.Functor.Functor$IO' ffi:!map:0_lam0}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Functor_46__123_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0_95_lam0_125_(
                  P_c0, arg0
                )
              else:  # {U_Prelude.Interactive.{putStr'0}1}
                return _idris_Prelude_46_Interactive_46__123_putStr_39_0_125_(arg0)
            else:
              if fn0[0] == 65675:  # {U_Prelude.List.{toList0}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_List_46__123_toList0_125_(P_c0, arg0)
              elif fn0[0] == 65676:  # {U_Prelude.List.{toList1}1}
                return _idris_Prelude_46_List_46__123_toList1_125_(arg0)
              else:  # {U_Python.Fields.{/.0}1}
                P_c0, P_c1 = fn0[1:]
                return _idris_Python_46_Fields_46__123__47__46_0_125_(P_c0, P_c1, arg0)
      else:
        if fn0[0] < 65686:
          if fn0[0] < 65682:
            if fn0[0] < 65680:
              if fn0[0] == 65678:  # {U_Python.Fields.{//.0}1}
                P_c0, P_c1 = fn0[1:]
                return _idris_Python_46_Fields_46__123__47__47__46_0_125_(P_c0, P_c1, arg0)
              else:  # {U_Python.Functions.{$.0}1}
                P_c0, P_c1, P_c2 = fn0[1:]
                return _idris_Python_46_Functions_46__123__36__46_0_125_(P_c0, P_c1, P_c2, arg0)
            else:
              if fn0[0] == 65680:  # {U_Python.IO.unRaw1}
                P_c0 = fn0[1]
                return _idris_Python_46_IO_46_unRaw(P_c0, arg0)
              else:  # {U_Python.Lib.Numpy.Matrix.array, c1}
                P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, P_c6 = fn0[1:]
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46_array_58_c_58_0(
                  P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, P_c6, arg0
                )
          else:
            if fn0[0] < 65684:
              if fn0[0] == 65682:  # {U_Python.Lib.Numpy.Matrix.fill1}
                P_c0, P_c1, P_c2, P_c3 = fn0[1:]
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46_fill(P_c0, P_c1, P_c2, P_c3, arg0)
              else:  # {U_Python.Lib.Numpy.Matrix.op1}
                P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, P_c6 = fn0[1:]
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46_op(
                  P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, P_c6, arg0
                )
            else:
              if fn0[0] == 65684:  # {U_Python.Lib.Numpy.Matrix.{array0}1}
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_array0_125_(arg0)
              else:  # {U_Python.Lib.Numpy.Matrix.{array1}1}
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_array1_125_(arg0)
        else:
          if fn0[0] < 65690:
            if fn0[0] < 65688:
              if fn0[0] == 65686:  # {U_Python.Lib.Numpy.Matrix.{array2}1}
                P_c0, P_c1 = fn0[1:]
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_array2_125_(P_c0, P_c1, arg0)
              else:  # {U_Python.Lib.Numpy.Matrix.{array3}1}
                P_c0 = fn0[1]
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_array3_125_(P_c0, arg0)
            else:
              if fn0[0] == 65688:  # {U_Python.Lib.Numpy.Matrix.{array4}1}
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_array4_125_(arg0)
              else:  # {U_Python.Lib.Numpy.Matrix.{array5}1}
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_array5_125_(arg0)
          else:
            if fn0[0] < 65692:
              if fn0[0] == 65690:  # {U_Python.Lib.Numpy.Matrix.{array6}1}
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_array6_125_(arg0)
              else:  # {U_Python.Lib.Numpy.Matrix.{dot0}1}
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_dot0_125_(arg0)
            else:
              if fn0[0] == 65692:  # {U_Python.Lib.Numpy.Matrix.{dot1}1}
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_dot1_125_(arg0)
              elif fn0[0] == 65693:  # {U_Python.Lib.Numpy.Matrix.{fill0}1}
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_fill0_125_(arg0)
              else:  # {U_Python.Lib.Numpy.Matrix.{fill1}1}
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_fill1_125_(arg0)
    else:
      if fn0[0] < 65712:
        if fn0[0] < 65703:
          if fn0[0] < 65699:
            if fn0[0] < 65697:
              if fn0[0] == 65695:  # {U_Python.Lib.Numpy.Matrix.{op0}1}
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_op0_125_(arg0)
              else:  # {U_Python.Lib.Numpy.Matrix.{op1}1}
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_op1_125_(arg0)
            else:
              if fn0[0] == 65697:  # {U_Python.Lib.Numpy.Matrix.{reshape0}1}
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_reshape0_125_(arg0)
              else:  # {U_Python.Lib.Numpy.Matrix.{reshape1}1}
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_reshape1_125_(arg0)
          else:
            if fn0[0] < 65701:
              if fn0[0] == 65699:  # {U_Python.Lib.Numpy.Matrix.{transpose0}1}
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_transpose0_125_(arg0)
              else:  # {U_Python.Lib.Numpy.Matrix.{unsafeNp0}1}
                return _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_unsafeNp0_125_(arg0)
            else:
              if fn0[0] == 65701:  # {U_Python.Prim.{pyList0}1}
                return _idris_Python_46_Prim_46__123_pyList0_125_(arg0)
              else:  # {U_Python.Prim.{pyList1}1}
                return _idris_Python_46_Prim_46__123_pyList1_125_(arg0)
        else:
          if fn0[0] < 65707:
            if fn0[0] < 65705:
              if fn0[0] == 65703:  # {U_Python.importModule1}
                P_c0, P_c1 = fn0[1:]
                return _idris_Python_46_importModule(P_c0, P_c1, arg0)
              else:  # {U_Python.{getGlobal0}1}
                P_c0 = fn0[1]
                return _idris_Python_46__123_getGlobal0_125_(P_c0, arg0)
            else:
              if fn0[0] == 65705:  # {U_io_bind1}
                P_c0, P_c1, P_c2, P_c3, P_c4 = fn0[1:]
                return _idris_io_95_bind(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
              else:  # {U_io_pure1}
                P_c0, P_c1, P_c2 = fn0[1:]
                return _idris_io_95_pure(P_c0, P_c1, P_c2, arg0)
          else:
            if fn0[0] < 65709:
              if fn0[0] == 65707:  # {U_prim_write1}
                P_c0, P_c1 = fn0[1:]
                return _idris_prim_95_write(P_c0, P_c1, arg0)
              else:  # {U_unsafePerformIO1}
                P_c0, P_c1 = fn0[1:]
                return _idris_unsafePerformIO(P_c0, P_c1, arg0)
            else:
              if fn0[0] == 65709:  # {U_{PE_Prelude.Show.List a implementation of Prelude.Show.Show, method show_d2fe22140}1}
                return _idris__123_PE_95_Prelude_46_Show_46_List_32_a_32_implementation_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_d2fe22140_125_(
                  arg0
                )
              elif fn0[0] == 65710:  # {U_{PE_Prelude.Show.List a implementation of Prelude.Show.Show, method show_d2fe22141}1}
                return _idris__123_PE_95_Prelude_46_Show_46_List_32_a_32_implementation_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_d2fe22141_125_(
                  arg0
                )
              else:  # {U_{PE_Prelude.Show.List a implementation of Prelude.Show.Show, method show_d2fe22142}1}
                return _idris__123_PE_95_Prelude_46_Show_46_List_32_a_32_implementation_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_d2fe22142_125_(
                  arg0
                )
      else:
        if fn0[0] < 65720:
          if fn0[0] < 65716:
            if fn0[0] < 65714:
              if fn0[0] == 65712:  # {U_{PE_printLn'_e0c531f50}1}
                P_c0 = fn0[1]
                return _idris__123_PE_95_printLn_39__95_e0c531f50_125_(P_c0, arg0)
              else:  # {U_{PE_printLn'_e0c531f51}1}
                return _idris__123_PE_95_printLn_39__95_e0c531f51_125_(arg0)
            else:
              if fn0[0] == 65714:  # {U_{PE_show_408b75990}1}
                return _idris__123_PE_95_show_95_408b75990_125_(arg0)
              else:  # {U_{PE_show_408b75991}1}
                return _idris__123_PE_95_show_95_408b75991_125_(arg0)
          else:
            if fn0[0] < 65718:
              if fn0[0] == 65716:  # {U_{PE_show_408b75992}1}
                return _idris__123_PE_95_show_95_408b75992_125_(arg0)
              else:  # {U_{PE_show_408b75993}1}
                return _idris__123_PE_95_show_95_408b75993_125_(arg0)
            else:
              if fn0[0] == 65718:  # {U_{PE_show_408b75994}1}
                return _idris__123_PE_95_show_95_408b75994_125_(arg0)
              else:  # {U_{PE_show_408b75995}1}
                return _idris__123_PE_95_show_95_408b75995_125_(arg0)
        else:
          if fn0[0] < 65724:
            if fn0[0] < 65722:
              if fn0[0] == 65720:  # {U_{Python.Lib.Numpy.Matrix.array:c:0_lam0}1}
                P_c0, P_c1 = fn0[1:]
                return _idris__123_Python_46_Lib_46_Numpy_46_Matrix_46_array_58_c_58_0_95_lam0_125_(
                  P_c0, P_c1, arg0
                )
              else:  # {U_{Python.Lib.Numpy.Matrix.array:c:0_lam1}1}
                P_c0 = fn0[1]
                return _idris__123_Python_46_Lib_46_Numpy_46_Matrix_46_array_58_c_58_0_95_lam1_125_(
                  P_c0, arg0
                )
            else:
              if fn0[0] == 65722:  # {U_{Python.Lib.Numpy.Matrix.array:c:0_lam2}1}
                return _idris__123_Python_46_Lib_46_Numpy_46_Matrix_46_array_58_c_58_0_95_lam2_125_(
                  arg0
                )
              else:  # {U_{Python.Lib.Numpy.Matrix.array:c:0_lam3}1}
                return _idris__123_Python_46_Lib_46_Numpy_46_Matrix_46_array_58_c_58_0_95_lam3_125_(
                  arg0
                )
          else:
            if fn0[0] < 65726:
              if fn0[0] == 65724:  # {U_{Python.Lib.Numpy.Matrix.array:c:0_lam4}1}
                return _idris__123_Python_46_Lib_46_Numpy_46_Matrix_46_array_58_c_58_0_95_lam4_125_(
                  arg0
                )
              else:  # {U_{io_bind1}1}
                P_c0, P_c1, P_c2, P_c3, P_c4, P_c5 = fn0[1:]
                return io_bind1(P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, arg0)
            else:
              if fn0[0] == 65726:  # {U_{unsafePerformIO0}1}
                return unsafePerformIO0(arg0)
              else:  # {U_Python.Lib.Numpy.Matrix.op2}
                P_c0, P_c1, P_c2, P_c3, P_c4, P_c5 = fn0[1:]
                return (65683, P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, arg0)  # {U_Python.Lib.Numpy.Matrix.op1}
    return _idris_error("unreachable due to case in tail position")

# {APPLY20}
def _idris__123_APPLY20_125_(fn0, _idris__123_arg00_125_, _idris__123_arg10_125_):
  while True:
    if fn0[0] == 65727:  # {U_Python.Lib.Numpy.Matrix.op2}
      P_c0, P_c1, P_c2, P_c3, P_c4, P_c5 = fn0[1:]
      return _idris_Python_46_Lib_46_Numpy_46_Matrix_46_op(
        P_c0,
        P_c1,
        P_c2,
        P_c3,
        P_c4,
        P_c5,
        _idris__123_arg00_125_,
        _idris__123_arg10_125_
      )
    else:
      return APPLY0(APPLY0(fn0, _idris__123_arg00_125_), _idris__123_arg10_125_)
    return _idris_error("unreachable due to case in tail position")

# {EVAL0}
def EVAL0(arg0):
  while True:
    return arg0

# {PE_Prelude.Show.List a implementation of Prelude.Show.Show, method show_d2fe22140}
def _idris__123_PE_95_Prelude_46_Show_46_List_32_a_32_implementation_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_d2fe22140_125_(
  in0
):
  while True:
    if not in0:  # Prelude.Bool.False
      return u'False'
    else:  # Prelude.Bool.True
      return u'True'
    return _idris_error("unreachable due to case in tail position")

# {PE_printLn'_e0c531f50}
def _idris__123_PE_95_printLn_39__95_e0c531f50_125_(e1, in0):
  while True:
    return sys.stdout.write((_idris_PE_95_show_95_408b7599(e1) + u'\u000a'))

# {PE_show_408b75990}
def _idris__123_PE_95_show_95_408b75990_125_(in0):
  while True:
    return _idris_PE_95_Prelude_46_Show_46_List_32_a_32_implementation_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_d2fe2214(
      in0
    )

# Prelude.Functor.{Prelude.Monad.@Prelude.Functor.Functor$IO' ffi:!map:0_lam0}
def _idris_Prelude_46_Functor_46__123_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0_95_lam0_125_(
  e3, in0
):
  while True:
    return (65706, None, None, APPLY0(e3, in0))  # {U_io_pure1}

# Prelude.Interfaces.{Prelude.Show.@Prelude.Interfaces.Ord$Prec:!>=:0_lam0}
def _idris_Prelude_46_Interfaces_46__123_Prelude_46_Show_46__64_Prelude_46_Interfaces_46_Ord_36_Prec_58__33__62__61__58_0_95_lam0_125_(
  e0, e1
):
  while True:
    return _idris_Prelude_46_Interfaces_46_Prelude_46_Show_46__64_Prelude_46_Interfaces_46_Eq_36_Prec_58__33__61__61__58_0(
      e0, e1
    )

# {Python.Lib.Numpy.Matrix.array:c:0_lam0}
def _idris__123_Python_46_Lib_46_Numpy_46_Matrix_46_array_58_c_58_0_95_lam0_125_(
  in2, in3, in4
):
  while True:
    return _idris_Data_46_Vect_46_foldrImpl(None, None, None, in2, in3, (65672, None), in4)  # {U_Prelude.Basics.id1}

# Python.Lib.Numpy.Matrix.{array0}
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_array0_125_(in1):
  while True:
    return (0,)  # Python.Telescope.Return

# Python.Lib.Numpy.Matrix.{dot0}
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_dot0_125_(in1):
  while True:
    return (0,)  # Python.Telescope.Return

# Python.Lib.Numpy.Matrix.{fill0}
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_fill0_125_(in1):
  while True:
    return (0,)  # Python.Telescope.Return

# Python.{getGlobal0}
def _idris_Python_46__123_getGlobal0_125_(e1, in0):
  while True:
    return _idris_get_global(e1)

# {io_bind0}
def io_bind0(e0, e1, e2, e3, e4, _idris_w, in0):
  while True:
    return APPLY0(e4, in0)

# Main.{main0}
def _idris_Main_46__123_main0_125_(in7):
  while True:
    return float(in7)

# Python.Lib.Numpy.Matrix.{op0}
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_op0_125_(in1):
  while True:
    return (0,)  # Python.Telescope.Return

# Prelude.Interactive.{putStr'0}
def _idris_Prelude_46_Interactive_46__123_putStr_39_0_125_(in0):
  while True:
    return (65706, None, None, Unit)  # {U_io_pure1}

# Python.Prim.{pyList0}
def _idris_Python_46_Prim_46__123_pyList0_125_(in1):
  while True:
    return (0,)  # Python.Telescope.Return

# Python.Lib.Numpy.Matrix.{reshape0}
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_reshape0_125_(in1):
  while True:
    return (0,)  # Python.Telescope.Return

# {runMain0}
def runMain0():
  while True:
    return EVAL0(APPLY0(_idris_Main_46_main(), None))

# Python.Functions.{strip0}
def _idris_Python_46_Functions_46__123_strip0_125_(in2):
  while True:
    return in2

# Prelude.List.{toList0}
def _idris_Prelude_46_List_46__123_toList0_125_(in0, in1):
  while True:
    return in1.cons(in0)

# Python.Lib.Numpy.Matrix.{transpose0}
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_transpose0_125_(in0):
  while True:
    return (0,)  # Python.Telescope.Return

# Python.Lib.Numpy.Matrix.{unsafeNp0}
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_unsafeNp0_125_(in0):
  while True:
    return in0

# {unsafePerformIO0}
def unsafePerformIO0(in0):
  while True:
    return in0

# Main.{xs0}
def _idris_Main_46__123_xs0_125_(in0):
  while True:
    return float(in0)

# {PE_Prelude.Show.List a implementation of Prelude.Show.Show, method show_d2fe22141}
def _idris__123_PE_95_Prelude_46_Show_46_List_32_a_32_implementation_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_d2fe22141_125_(
  in2
):
  while True:
    if not in2:  # Prelude.Bool.False
      return u'False'
    else:  # Prelude.Bool.True
      return u'True'
    return _idris_error("unreachable due to case in tail position")

# {PE_printLn'_e0c531f51}
def _idris__123_PE_95_printLn_39__95_e0c531f51_125_(in1):
  while True:
    return (65706, None, None, Unit)  # {U_io_pure1}

# {PE_show_408b75991}
def _idris__123_PE_95_show_95_408b75991_125_(in2):
  while True:
    return _idris_PE_95_Prelude_46_Show_46_List_32_a_32_implementation_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_showPrec_95_32c264fb(
      None, in2
    )

# {Python.Lib.Numpy.Matrix.array:c:0_lam1}
def _idris__123_Python_46_Lib_46_Numpy_46_Matrix_46_array_58_c_58_0_95_lam1_125_(
  in2, in3
):
  while True:
    return (65720, in2, in3)  # {U_{Python.Lib.Numpy.Matrix.array:c:0_lam0}1}

# Python.Lib.Numpy.Matrix.{array1}
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_array1_125_(in0):
  while True:
    return (1, (0,), (65684,))  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Numpy.Matrix.{array0}1}

# Python.Lib.Numpy.Matrix.{dot1}
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_dot1_125_(in0):
  while True:
    return (1, (0,), (65691,))  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Numpy.Matrix.{dot0}1}

# Python.Lib.Numpy.Matrix.{fill1}
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_fill1_125_(in0):
  while True:
    return (1, (0,), (65693,))  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Numpy.Matrix.{fill0}1}

# {io_bind1}
def io_bind1(e0, e1, e2, e3, e4, _idris_w, in0):
  while True:
    return APPLY0(io_bind0(e0, e1, e2, e3, e4, _idris_w, in0), _idris_w)

# Main.{main1}
def _idris_Main_46__123_main1_125_(in8):
  while True:
    return float(in8)

# Python.Lib.Numpy.Matrix.{op1}
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_op1_125_(in0):
  while True:
    return (1, (0,), (65695,))  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Numpy.Matrix.{op0}1}

# Python.Prim.{pyList1}
def _idris_Python_46_Prim_46__123_pyList1_125_(in0):
  while True:
    return (1, (0,), (65701,))  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Prim.{pyList0}1}

# Python.Lib.Numpy.Matrix.{reshape1}
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_reshape1_125_(in0):
  while True:
    return (1, (0,), (65697,))  # Python.Telescope.Bind, Python.Telescope.Pi, {U_Python.Lib.Numpy.Matrix.{reshape0}1}

# Prelude.List.{toList1}
def _idris_Prelude_46_List_46__123_toList1_125_(in0):
  while True:
    return (65675, in0)  # {U_Prelude.List.{toList0}1}

# {unsafePerformIO1}
def unsafePerformIO1(e0, e1, e2):
  while True:
    return (65726,)  # {U_{unsafePerformIO0}1}

# {PE_Prelude.Show.List a implementation of Prelude.Show.Show, method show_d2fe22142}
def _idris__123_PE_95_Prelude_46_Show_46_List_32_a_32_implementation_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_d2fe22142_125_(
  in1
):
  while True:
    return (65710,)  # {U_{PE_Prelude.Show.List a implementation of Prelude.Show.Show, method show_d2fe22141}1}

# {PE_show_408b75992}
def _idris__123_PE_95_show_95_408b75992_125_(in1):
  while True:
    return (65715,)  # {U_{PE_show_408b75991}1}

# {Python.Lib.Numpy.Matrix.array:c:0_lam2}
def _idris__123_Python_46_Lib_46_Numpy_46_Matrix_46_array_58_c_58_0_95_lam2_125_(
  in2
):
  while True:
    return (65721, in2)  # {U_{Python.Lib.Numpy.Matrix.array:c:0_lam1}1}

# Python.Lib.Numpy.Matrix.{array2}
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_array2_125_(in4, in5, in6):
  while True:
    return _idris_Data_46_Vect_46_foldrImpl(None, None, None, in4, in5, (65672, None), in6)  # {U_Prelude.Basics.id1}

# {io_bind2}
def io_bind2(e0, e1, e2, e3, e4, _idris_w):
  while True:
    return (65725, e0, e1, e2, e3, e4, _idris_w)  # {U_{io_bind1}1}

# Main.{main2}
def _idris_Main_46__123_main2_125_(in6):
  while True:
    return _idris_Prelude_46_Interactive_46_putStr_39_(
      None,
      (_idris_Prelude_46_Show_46_Python_46_Lib_46_Numpy_46_Matrix_46__64_Prelude_46_Show_46_Show_36_Matrix_32_r_32_c_32_dt_58__33_show_58_0(
        None,
        None,
        None,
        None,
        APPLY0(
          APPLY0(
            _idris_Prelude_46_Interfaces_46_Python_46_Lib_46_Numpy_46_Matrix_46__64_Prelude_46_Interfaces_46_Num_36_Matrix_32_r_32_c_32_dt_58__33__43__58_0(
              None, None, None, None
            ),
            APPLY0(
              APPLY0(
                _idris_Prelude_46_Interfaces_46_Python_46_Lib_46_Numpy_46_Matrix_46__64_Prelude_46_Interfaces_46_Num_36_Matrix_32_r_32_c_32_dt_58__33__42__58_0(
                  None, None, None, None
                ),
                APPLY0(
                  _idris_Python_46_Lib_46_Numpy_46_Matrix_46_fromInteger(
                    None,
                    (0, u'float', (65661,), (65672, None)),  # Python.Lib.Numpy.Matrix.MkDType, {U_Main.{main0}1}, {U_Prelude.Basics.id1}
                    4,
                    4
                  ),
                  2
                )
              ),
              _idris_Python_46_Lib_46_Numpy_46_Matrix_46_transpose(
                None,
                None,
                None,
                None,
                _idris_Python_46_Lib_46_Numpy_46_Matrix_46_dot(
                  None,
                  None,
                  None,
                  None,
                  None,
                  _idris_Main_46_zs(),
                  _idris_Main_46_xs()
                )
              )
            )
          ),
          APPLY0(
            _idris_Python_46_Lib_46_Numpy_46_Matrix_46_fromDouble(
              None,
              (0, u'float', (65662,), (65672, None)),  # Python.Lib.Numpy.Matrix.MkDType, {U_Main.{main1}1}, {U_Prelude.Basics.id1}
              4,
              4
            ),
            0.2
          )
        )
      ) + u'\u000a')
    )

# {PE_show_408b75993}
def _idris__123_PE_95_show_95_408b75993_125_(in0):
  while True:
    return _idris_PE_95_Prelude_46_Show_46_List_32_a_32_implementation_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_d2fe2214(
      in0
    )

# {Python.Lib.Numpy.Matrix.array:c:0_lam3}
def _idris__123_Python_46_Lib_46_Numpy_46_Matrix_46_array_58_c_58_0_95_lam3_125_(
  in1
):
  while True:
    return (65722,)  # {U_{Python.Lib.Numpy.Matrix.array:c:0_lam2}1}

# Python.Lib.Numpy.Matrix.{array3}
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_array3_125_(in4, in5):
  while True:
    return (65686, in4, in5)  # {U_Python.Lib.Numpy.Matrix.{array2}1}

# Main.{main3}
def _idris_Main_46__123_main3_125_(in5):
  while True:
    return (
      65705,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        (_idris_Prelude_46_Show_46_Python_46_Lib_46_Numpy_46_Matrix_46__64_Prelude_46_Show_46_Show_36_Matrix_32_r_32_c_32_dt_58__33_show_58_0(
          None,
          None,
          None,
          None,
          _idris_Python_46_Lib_46_Numpy_46_Matrix_46_dot(
            None,
            None,
            None,
            None,
            None,
            _idris_Python_46_Lib_46_Numpy_46_Matrix_46_transpose(
              None,
              None,
              None,
              None,
              _idris_Main_46_xs()
            ),
            _idris_Main_46_xs()
          )
        ) + u'\u000a')
      ),
      (65663,)  # {U_Main.{main2}1}
    )

# {PE_show_408b75994}
def _idris__123_PE_95_show_95_408b75994_125_(in2):
  while True:
    return _idris_PE_95_Prelude_46_Show_46_List_32_a_32_implementation_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_showPrec_95_32c264fb(
      None, in2
    )

# {Python.Lib.Numpy.Matrix.array:c:0_lam4}
def _idris__123_Python_46_Lib_46_Numpy_46_Matrix_46_array_58_c_58_0_95_lam4_125_(
  in0
):
  while True:
    return (65723,)  # {U_{Python.Lib.Numpy.Matrix.array:c:0_lam3}1}

# Python.Lib.Numpy.Matrix.{array4}
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_array4_125_(in4):
  while True:
    return (65687, in4)  # {U_Python.Lib.Numpy.Matrix.{array3}1}

# Main.{main4}
def _idris_Main_46__123_main4_125_(in4):
  while True:
    return (
      65705,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        (_idris_Prelude_46_Show_46_Python_46_Lib_46_Numpy_46_Matrix_46__64_Prelude_46_Show_46_Show_36_Matrix_32_r_32_c_32_dt_58__33_show_58_0(
          None,
          None,
          None,
          None,
          _idris_Python_46_Lib_46_Numpy_46_Matrix_46_dot(
            None,
            None,
            None,
            None,
            None,
            _idris_Main_46_xs(),
            _idris_Python_46_Lib_46_Numpy_46_Matrix_46_transpose(
              None,
              None,
              None,
              None,
              _idris_Main_46_xs()
            )
          )
        ) + u'\u000a')
      ),
      (65664,)  # {U_Main.{main3}1}
    )

# {PE_show_408b75995}
def _idris__123_PE_95_show_95_408b75995_125_(in1):
  while True:
    return (65718,)  # {U_{PE_show_408b75994}1}

# Python.Lib.Numpy.Matrix.{array5}
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_array5_125_(in3):
  while True:
    return (65688,)  # {U_Python.Lib.Numpy.Matrix.{array4}1}

# Main.{main5}
def _idris_Main_46__123_main5_125_(in3):
  while True:
    return (
      65705,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        (_idris_Prelude_46_Show_46_Python_46_Lib_46_Numpy_46_Matrix_46__64_Prelude_46_Show_46_Show_36_Matrix_32_r_32_c_32_dt_58__33_show_58_0(
          None,
          None,
          None,
          None,
          _idris_Python_46_Lib_46_Numpy_46_Matrix_46_dot(
            None,
            None,
            None,
            None,
            None,
            _idris_Main_46_xs(),
            _idris_Main_46_zs()
          )
        ) + u'\u000a')
      ),
      (65665,)  # {U_Main.{main4}1}
    )

# Python.Lib.Numpy.Matrix.{array6}
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46__123_array6_125_(in2):
  while True:
    return (65689,)  # {U_Python.Lib.Numpy.Matrix.{array5}1}

# Main.{main6}
def _idris_Main_46__123_main6_125_(in2):
  while True:
    return (
      65705,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        (_idris_Prelude_46_Show_46_Python_46_Lib_46_Numpy_46_Matrix_46__64_Prelude_46_Show_46_Show_36_Matrix_32_r_32_c_32_dt_58__33_show_58_0(
          None,
          None,
          None,
          None,
          _idris_Main_46_zs()
        ) + u'\u000a')
      ),
      (65666,)  # {U_Main.{main5}1}
    )

# Main.{main7}
def _idris_Main_46__123_main7_125_(in1):
  while True:
    return (
      65705,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        (_idris_Prelude_46_Show_46_Python_46_Lib_46_Numpy_46_Matrix_46__64_Prelude_46_Show_46_Show_36_Matrix_32_r_32_c_32_dt_58__33_show_58_0(
          None,
          None,
          None,
          None,
          _idris_Main_46_ys()
        ) + u'\u000a')
      ),
      (65667,)  # {U_Main.{main6}1}
    )

# Main.{main8}
def _idris_Main_46__123_main8_125_(in0):
  while True:
    return (
      65705,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr_39_(
        None,
        (_idris_Prelude_46_Show_46_Python_46_Lib_46_Numpy_46_Matrix_46__64_Prelude_46_Show_46_Show_36_Matrix_32_r_32_c_32_dt_58__33_show_58_0(
          None,
          None,
          None,
          None,
          _idris_Main_46_xs()
        ) + u'\u000a')
      ),
      (65668,)  # {U_Main.{main7}1}
    )

# Python.Lib.Numpy.Matrix.array, c
def _idris_Python_46_Lib_46_Numpy_46_Matrix_46_array_58_c_58_0(
  e0, e1, e2, e3, e4, e5, e6, e7
):
  while True:
    return _idris_Python_46_Prim_46_pyList(
      None,
      APPLY0(_idris_Prelude_46_List_46_toList(None, None, (65724,)), e7)  # {U_{Python.Lib.Numpy.Matrix.array:c:0_lam4}1}
    )

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

# Prelude.Show.Prelude.Show.List a implementation of Prelude.Show.Show, method show, show'
def _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
  e0, e1, e2, e3, e4
):
  while True:
    if e4:  # Prelude.List.::
      in0, in1 = e4.head, e4.tail
      if not in1:  # Prelude.List.Nil
        return (e3 + APPLY0(_idris_Prelude_46_Show_46_show(None, e2), in0))
      else:
        e0, e1, e2, e3, e4, = None, None, e2, (e3 + (APPLY0(_idris_Prelude_46_Show_46_show(None, e2), in0) + u', ')), in1,
        continue
        return _idris_error("unreachable due to tail call")
      return _idris_error("unreachable due to case in tail position")
    else:  # Prelude.List.Nil
      return e3
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

# Prelude.Functor.Prelude.Monad.IO' ffi implementation of Prelude.Functor.Functor, method map
def _idris_Prelude_46_Functor_46_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
  e0, e1, e2, e3, e4
):
  while True:
    return (65705, None, None, None, e4, (65673, e3))  # {U_io_bind1}, {U_Prelude.Functor.{Prelude.Monad.@Prelude.Functor.Functor$IO' ffi:!map:0_lam0}1}

# Prelude.Functor.Data.Vect.Vect n implementation of Prelude.Functor.Functor, method map
def _idris_Prelude_46_Functor_46_Data_46_Vect_46__64_Prelude_46_Functor_46_Functor_36_Vect_32_n_58__33_map_58_0(
  e0, e1, e2, e3, e4
):
  while True:
    if e4[0] == 1:  # Data.Vect.::
      in0, in1 = e4[1:]
      return (
        1,  # Data.Vect.::
        APPLY0(e3, in0),
        _idris_Prelude_46_Functor_46_Data_46_Vect_46__64_Prelude_46_Functor_46_Functor_36_Vect_32_n_58__33_map_58_0(
          None, None, None, e3, in1
        )
      )
    else:  # Data.Vect.Nil
      return (0,)  # Data.Vect.Nil
    return _idris_error("unreachable due to case in tail position")

# Prelude.Interfaces.Python.Lib.Numpy.Matrix.Matrix r c dt implementation of Prelude.Interfaces.Num, method *
def _idris_Prelude_46_Interfaces_46_Python_46_Lib_46_Numpy_46_Matrix_46__64_Prelude_46_Interfaces_46_Num_36_Matrix_32_r_32_c_32_dt_58__33__42__58_0(
  e0, e1, e2, e3
):
  while True:
    return (65727, None, None, None, None, u'__mul__', None)  # {U_Python.Lib.Numpy.Matrix.op2}

# Prelude.Interfaces.Python.Lib.Numpy.Matrix.Matrix r c dt implementation of Prelude.Interfaces.Num, method +
def _idris_Prelude_46_Interfaces_46_Python_46_Lib_46_Numpy_46_Matrix_46__64_Prelude_46_Interfaces_46_Num_36_Matrix_32_r_32_c_32_dt_58__33__43__58_0(
  e0, e1, e2, e3
):
  while True:
    return (65727, None, None, None, None, u'__add__', None)  # {U_Python.Lib.Numpy.Matrix.op2}

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

# Prelude.Show.Prelude.Show.Bool implementation of Prelude.Show.Show, method show
def _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_Bool_58__33_show_58_0(
  e0
):
  while True:
    if not e0:  # Prelude.Bool.False
      return u'False'
    else:  # Prelude.Bool.True
      return u'True'
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.Python.Lib.Numpy.Matrix.Matrix r c dt implementation of Prelude.Show.Show, method show
def _idris_Prelude_46_Show_46_Python_46_Lib_46_Numpy_46_Matrix_46__64_Prelude_46_Show_46_Show_36_Matrix_32_r_32_c_32_dt_58__33_show_58_0(
  e0, e1, e2, e3, e4
):
  while True:
    return _idris_unsafePerformIO(
      None,
      None,
      _idris_Python_46_Functions_46__36__46_(
        None,
        None,
        (0,),  # Python.Telescope.Return
        _idris_Python_46_Fields_46__47__46_(None, None, e4, u'__str__', None),
        None,
        Unit
      )
    )

# Prelude.Show.Prelude.Show.Maybe a implementation of Prelude.Show.Show, method showPrec
def _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_Maybe_32_a_58__33_showPrec_58_0(
  e0, e1, e2, e3
):
  while True:
    if e3 is not None:  # Prelude.Maybe.Just
      in0 = e3
      aux1 = _idris_Prelude_46_Interfaces_46_Prelude_46_Show_46__64_Prelude_46_Interfaces_46_Ord_36_Prec_58__33__62__61__58_0(
        e2, (6,)  # Prelude.Show.App
      )
      if not aux1:  # Prelude.Bool.False
        return (u'Just' + (u' ' + APPLY0(APPLY0(_idris_Prelude_46_Show_46_showPrec(None, e1), None), in0)))
      else:  # Prelude.Bool.True
        return (u'(' + ((u'Just' + (u' ' + APPLY0(APPLY0(_idris_Prelude_46_Show_46_showPrec(None, e1), None), in0))) + u')'))
      return _idris_error("unreachable due to case in tail position")
    else:  # Prelude.Maybe.Nothing
      return u'Nothing'
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

if __name__ == '__main__':
  runMain0()
