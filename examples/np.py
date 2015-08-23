#!/usr/bin/env python

import sys
import importlib

Unit = object()
World = object()

class IdrisError(Exception):
  pass

def _idris_error(msg):
  raise IdrisError(msg)

def _idris_call(f, args):
  return f(*list(args))

def _idris_foreach(it, st, f):
  for x in it:
    # Apply st, x, world
    st = APPLY0(APPLY0(APPLY0(f, st), x), World)
  return st

def _idris_try(f, succ, fail):
  try:
    result = APPLY0(f, World)  # apply to world
    return APPLY0(succ, result)
  except Exception as e:
    # provide the exception class name + the exception itself
    return APPLY0(APPLY0(fail, e.__class__.__name__), e)

def _idris_raise(e):
  raise e

def _idris_marshal_IO(action):
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

# Python.$.
def _idris_Python_46__36__46_(e0, e1, e2, e3, e4, e5):
  while True:
    return _idris_Prelude_46_Functor_46_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65656, None),  # {U_Python.IO.unRaw1}
      (65674, e3, e5)  # {U_Python.{$.0}1}
    )

# Prelude.Basics..
def _idris_Prelude_46_Basics_46__46_(e0, e1, e2, e3, e4, _idris_x):
  while True:
    return APPLY0(e3, APPLY0(e4, _idris_x))

# Python./.
def _idris_Python_46__47__46_(e0, e1, e2, e3, e4):
  while True:
    return _idris_unsafePerformIO(None, None, (65675, e2, e3))  # {U_Python.{/.0}1}

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

# Force
def _idris_Force(e0, e1, e2):
  while True:
    in0 = EVAL0(e2)
    return in0

# PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac6
def _idris_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac6(
  e0
):
  while True:
    if e0 is not None:  # Prelude.Maybe.Just
      in9 = e0
      aux1 = _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33__62__61__58_0(
        (0,), (6,)  # Prelude.Show.Open, Prelude.Show.App
      )
      if not aux1:  # Prelude.Bool.False
        return ("Just" + (" " + APPLY0(
          APPLY0(_idris_Prelude_46_Show_46_showPrec(None, (0, (65691,), (65696,))), None),  # constructor of Prelude.Show.Show, {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac63}1}, {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac68}1}
          in9
        )))
      else:  # Prelude.Bool.True
        return ("(" + (("Just" + (" " + APPLY0(
          APPLY0(_idris_Prelude_46_Show_46_showPrec(None, (0, (65683,), (65688,))), None),  # constructor of Prelude.Show.Show, {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac612}1}, {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac617}1}
          in9
        ))) + ")"))
      return _idris_error("unreachable due to case in tail position")
    else:  # Prelude.Maybe.Nothing
      return "Nothing"
    return _idris_error("unreachable due to case in tail position")

# PE_printLn_140bcc5c
def _idris_PE_95_printLn_95_140bcc5c(e0, e1):
  while True:
    return _idris_Prelude_46_Interactive_46_putStr(
      None,
      (_idris_PE_95_show_95_d9020f8e(e1) + "\n")
    )

# PE_show_d9020f8e
def _idris_PE_95_show_95_d9020f8e(e0):
  while True:
    return _idris_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac6(
      e0
    )

# Python.Lib.Numpy.add
def _idris_Python_46_Lib_46_Numpy_46_add(e0, e1, e2, e3):
  while True:
    return (65704, None, None, None, None, "__add__", None)  # {U_Python.Lib.Numpy.binop2}

# Python.Lib.Numpy.array
def _idris_Python_46_Lib_46_Numpy_46_array(e0, e1, e2, e3, e4):
  while True:
    return APPLY0(
      _idris_Python_46_Lib_46_Numpy_46_unsafeNpMtx(None, None, None, None),
      (65663, e4, e3)  # {U_Python.Lib.Numpy.{array2}1}
    )

# believe_me
def _idris_believe_95_me(e0, e1, e2):
  while True:
    return e2

# Python.Lib.Numpy.binop
def _idris_Python_46_Lib_46_Numpy_46_binop(e0, e1, e2, e3, e4, e5, e6, e7):
  while True:
    return APPLY0(
      _idris_Python_46_Lib_46_Numpy_46_unsafeNpMtx(None, None, None, None),
      (65664, e4, e6, e7)  # {U_Python.Lib.Numpy.{binop0}1}
    )

# Python.Builtins.builtins
def _idris_Python_46_Builtins_46_builtins():
  while True:
    return _idris_unsafePerformIO(None, None, (65672, "__builtins__"))  # {U_Python.RTS.getGlobal1}

# call__IO
def _idris_call_95__95_IO(e0, e1, e2):
  while True:
    return APPLY0(e2, None)

# Prelude.Classes.compare
def _idris_Prelude_46_Classes_46_compare(e0, e1):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Classes.Ord
    in0, in1 = e1[1:]
    return in0
    return _idris_error("unreachable due to case in tail position")

# Python.Lib.Numpy.dot
def _idris_Python_46_Lib_46_Numpy_46_dot(e0, e1, e2, e3, e4, e5, e6):
  while True:
    return APPLY0(
      _idris_Python_46_Lib_46_Numpy_46_unsafeNpMtx(None, None, None, None),
      (65665, e5, e6)  # {U_Python.Lib.Numpy.{dot0}1}
    )

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

# Data.VectType.Vect.foldrImpl
def _idris_Data_46_VectType_46_Vect_46_foldrImpl(e0, e1, e2, e3, e4, e5, e6):
  while True:
    if e6[0] == 1:  # Data.VectType.Vect.::
      in0, in1 = e6[1:]
      e0, e1, e2, e3, e4, e5, e6, = None, None, None, e3, e4, (65642, None, None, None, e5, APPLY0(e3, in0)), in1,  # {U_Prelude.Basics..1}
      continue
      return _idris_error("unreachable due to tail call")
    else:  # Data.VectType.Vect.Nil
      return APPLY0(e5, e4)
    return _idris_error("unreachable due to case in tail position")

# Python.Lib.Numpy.fromDyn
def _idris_Python_46_Lib_46_Numpy_46_fromDyn(e0, e1, e2, e3, e4):
  while True:
    return APPLY0(
      _idris_Python_46_Lib_46_Numpy_46_unsafeNpMtx(None, None, None, None),
      (65667, e4, e3, e1, e2)  # {U_Python.Lib.Numpy.{fromDyn1}1}
    )

# Python.Lib.Numpy.fromFloat
def _idris_Python_46_Lib_46_Numpy_46_fromFloat(e0, e1, e2, e3):
  while True:
    return (65642, None, None, None, (65659, None, e1, e2, e3), (65677, None, None))  # {U_Prelude.Basics..1}, {U_Python.Lib.Numpy.fromDyn1}, {U_believe_me1}

# Python.Lib.Numpy.fromInteger
def _idris_Python_46_Lib_46_Numpy_46_fromInteger(e0, e1, e2, e3):
  while True:
    return (65642, None, None, None, (65659, None, e1, e2, e3), (65677, None, None))  # {U_Prelude.Basics..1}, {U_Python.Lib.Numpy.fromDyn1}, {U_believe_me1}

# Python.RTS.getGlobal
def _idris_Python_46_RTS_46_getGlobal(e0, _idris_w):
  while True:
    return _idris_get_global(e0)

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

# Python.RTS.importModule
def _idris_Python_46_RTS_46_importModule(e0, _idris_w):
  while True:
    return importlib.import_module(e0)

# Python.Lib.Numpy.import_
def _idris_Python_46_Lib_46_Numpy_46_import_95_():
  while True:
    return _idris_Prelude_46_Functor_46_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65668,),  # {U_Python.Lib.Numpy.{import_0}1}
      (65673, "numpy")  # {U_Python.RTS.importModule1}
    )

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

# Python.Builtins.list
def _idris_Python_46_Builtins_46_list():
  while True:
    return _idris_Python_46__47__46_(
      None,
      None,
      _idris_Python_46_Builtins_46_builtins(),
      "list",
      None
    )

# Main.main
def _idris_Main_46_main():
  while True:
    return (
      65678,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_PE_95_printLn_95_140bcc5c(None, _idris_Main_46_f(4)),
      (65641,)  # {U_Main.{main4}1}
    )

# Prelude.Functor.map
def _idris_Prelude_46_Functor_46_map(e0, e1, e2, e3):
  while True:
    return APPLY0(APPLY0(e3, e1), e2)

# mkForeignPrim
def _idris_mkForeignPrim():
  while True:
    return None

# Python.Lib.Numpy.mul
def _idris_Python_46_Lib_46_Numpy_46_mul(e0, e1, e2, e3):
  while True:
    return (65704, None, None, None, None, "__mul__", None)  # {U_Python.Lib.Numpy.binop2}

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

# prim_io_bind
def _idris_prim_95_io_95_bind(e0, e1, e2, e3):
  while True:
    return APPLY0(e3, e2)

# Prelude.Interactive.putStr
def _idris_Prelude_46_Interactive_46_putStr(e0, e1):
  while True:
    return (65678, None, None, None, (65645, e1), (65646,))  # {U_io_bind1}, {U_Prelude.Interactive.{putStr0}1}, {U_Prelude.Interactive.{putStr1}1}

# Python.Lib.Numpy.reshape
def _idris_Python_46_Lib_46_Numpy_46_reshape(e0, e1, e2, e3, e4, e5, e6, e7):
  while True:
    return APPLY0(
      _idris_Python_46_Lib_46_Numpy_46_unsafeNpMtx(None, None, None, None),
      (65669, e6, e4, e5)  # {U_Python.Lib.Numpy.{reshape0}1}
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
      return ("(" + (e1 + ")"))
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.showPrec
def _idris_Prelude_46_Show_46_showPrec(e0, e1):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Show.Show
    in0, in1 = e1[1:]
    return in1
    return _idris_error("unreachable due to case in tail position")

# Python.toDyn
def _idris_Python_46_toDyn(e0):
  while True:
    return (65677, None, None)  # {U_believe_me1}

# Python.Builtins.toPyList
def _idris_Python_46_Builtins_46_toPyList(e0, e1):
  while True:
    return _idris_unsafePerformIO(
      None,
      None,
      _idris_Python_46__36__46_(
        None,
        None,
        None,
        _idris_Python_46_Builtins_46_list(),
        None,
        (1, APPLY0(_idris_Python_46_toDyn(None), e1), (0,))  # Python.::, Python.Nil
      )
    )

# Python.toString
def _idris_Python_46_toString(e0):
  while True:
    return _idris_unsafePerformIO(None, None, (65676, e0))  # {U_Python.{toString0}1}

# Python.Lib.Numpy.transpose
def _idris_Python_46_Lib_46_Numpy_46_transpose(e0, e1, e2, e3, e4):
  while True:
    return APPLY0(
      _idris_Python_46_Lib_46_Numpy_46_unsafeNpMtx(None, None, None, None),
      (65670, e4)  # {U_Python.Lib.Numpy.{transpose0}1}
    )

# Python.IO.unRaw
def _idris_Python_46_IO_46_unRaw(e0, e1):
  while True:
    return e1

# Python.Lib.Numpy.unsafeNpMtx
def _idris_Python_46_Lib_46_Numpy_46_unsafeNpMtx(e0, e1, e2, e3):
  while True:
    return (65642, None, None, None, (65671,), (65660, None))  # {U_Prelude.Basics..1}, {U_Python.Lib.Numpy.{unsafeNpMtx0}1}, {U_Python.Lib.Numpy.unsafeNumpy1}

# Python.Lib.Numpy.unsafeNumpy
def _idris_Python_46_Lib_46_Numpy_46_unsafeNumpy(e0, e1):
  while True:
    return _idris_unsafePerformIO(
      None,
      None,
      (65678, None, None, None, _idris_Python_46_Lib_46_Numpy_46_import_95_(), e1)  # {U_io_bind1}
    )

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
    return _idris_Python_46_Lib_46_Numpy_46_array(
      None,
      None,
      None,
      "float",
      (
        1,  # Data.VectType.Vect.::
        (1, 1.0, (1, -2.1, (1, 3.3, (1, -0.1, (0,))))),  # Data.VectType.Vect.::, Data.VectType.Vect.::, Data.VectType.Vect.::, Data.VectType.Vect.::, Data.VectType.Vect.Nil
        (
          1,  # Data.VectType.Vect.::
          (1, 3.5, (1, 7.0, (1, 0.0, (1, -5.2, (0,))))),  # Data.VectType.Vect.::, Data.VectType.Vect.::, Data.VectType.Vect.::, Data.VectType.Vect.::, Data.VectType.Vect.Nil
          (1, (1, 0.5, (1, 7.2, (1, -1.1, (1, 0.0, (0,))))), (0,))  # Data.VectType.Vect.::, Data.VectType.Vect.::, Data.VectType.Vect.::, Data.VectType.Vect.::, Data.VectType.Vect.::, Data.VectType.Vect.Nil, Data.VectType.Vect.Nil
        )
      )
    )

# Main.ys
def _idris_Main_46_ys():
  while True:
    return _idris_Python_46_Lib_46_Numpy_46_reshape(
      None,
      None,
      None,
      None,
      6,
      2,
      _idris_Main_46_xs(),
      None
    )

# Main.zs
def _idris_Main_46_zs():
  while True:
    return _idris_Python_46_Lib_46_Numpy_46_reshape(
      None,
      None,
      None,
      None,
      4,
      3,
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

# Python.{$.0}
def _idris_Python_46__123__36__46_0_125_(e3, e5, in0):
  while True:
    return _idris_call(
      e3,
      _idris_Python_46__36__46__58_fromHList_58_0(
        None, None, None, None, None, None, None, e5
      )
    )

# Python.{/.0}
def _idris_Python_46__123__47__46_0_125_(e2, e3, in0):
  while True:
    return getattr(e2, e3)

# {APPLY0}
def APPLY0(fn0, arg0):
  while True:
    if fn0[0] < 65672:
      if fn0[0] < 65654:
        if fn0[0] < 65645:
          if fn0[0] < 65640:
            if fn0[0] < 65638:
              if fn0[0] == 65636:  # {U_Data.VectType.Vect.Vect n instance of Prelude.Functor.Functor1}
                P_c0, P_c1, P_c2, P_c3 = fn0[1:]
                return _idris_Data_46_VectType_46_Vect_46__64_Prelude_46_Functor_46_Functor_36_Vect_32_n(
                  P_c0, P_c1, P_c2, P_c3, arg0
                )
              else:  # {U_Main.{main0}1}
                return _idris_Main_46__123_main0_125_(arg0)
            else:
              if fn0[0] == 65638:  # {U_Main.{main1}1}
                return _idris_Main_46__123_main1_125_(arg0)
              else:  # {U_Main.{main2}1}
                return _idris_Main_46__123_main2_125_(arg0)
          else:
            if fn0[0] < 65642:
              if fn0[0] == 65640:  # {U_Main.{main3}1}
                return _idris_Main_46__123_main3_125_(arg0)
              else:  # {U_Main.{main4}1}
                return _idris_Main_46__123_main4_125_(arg0)
            else:
              if fn0[0] == 65642:  # {U_Prelude.Basics..1}
                P_c0, P_c1, P_c2, P_c3, P_c4 = fn0[1:]
                return _idris_Prelude_46_Basics_46__46_(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
              elif fn0[0] == 65643:  # {U_Prelude.Basics.id1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Basics_46_id(P_c0, arg0)
              else:  # {U_Prelude.Functor.{Prelude.Monad.IO' ffi instance of Prelude.Functor.Functor, method map_lam0}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Functor_46__123_Prelude_46_Monad_46_IO_39__32_ffi_32_instance_32_of_32_Prelude_46_Functor_46_Functor_44__32_method_32_map_95_lam0_125_(
                  P_c0, arg0
                )
        else:
          if fn0[0] < 65649:
            if fn0[0] < 65647:
              if fn0[0] == 65645:  # {U_Prelude.Interactive.{putStr0}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Interactive_46__123_putStr0_125_(P_c0, arg0)
              else:  # {U_Prelude.Interactive.{putStr1}1}
                return _idris_Prelude_46_Interactive_46__123_putStr1_125_(arg0)
            else:
              if fn0[0] == 65647:  # {U_Prelude.Nat.Nat instance of Prelude.Classes.Eq1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Eq_36_Nat(P_c0, arg0)
              else:  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam0}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
                  P_c0, arg0
                )
          else:
            if fn0[0] < 65651:
              if fn0[0] == 65649:  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam1}1}
                return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
                  arg0
                )
              else:  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam2}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
                  P_c0, arg0
                )
            else:
              if fn0[0] == 65651:  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam3}1}
                return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
                  arg0
                )
              elif fn0[0] == 65652:  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam0}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
                  P_c0, arg0
                )
              else:  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam1}1}
                return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
                  arg0
                )
      else:
        if fn0[0] < 65663:
          if fn0[0] < 65658:
            if fn0[0] < 65656:
              if fn0[0] == 65654:  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam2}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
                  P_c0, arg0
                )
              else:  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam3}1}
                return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
                  arg0
                )
            else:
              if fn0[0] == 65656:  # {U_Python.IO.unRaw1}
                P_c0 = fn0[1]
                return _idris_Python_46_IO_46_unRaw(P_c0, arg0)
              else:  # {U_Python.Lib.Numpy.array, mkList1}
                P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, P_c6 = fn0[1:]
                return _idris_Python_46_Lib_46_Numpy_46_array_58_mkList_58_0(
                  P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, P_c6, arg0
                )
          else:
            if fn0[0] < 65660:
              if fn0[0] == 65658:  # {U_Python.Lib.Numpy.binop1}
                P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, P_c6 = fn0[1:]
                return _idris_Python_46_Lib_46_Numpy_46_binop(
                  P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, P_c6, arg0
                )
              else:  # {U_Python.Lib.Numpy.fromDyn1}
                P_c0, P_c1, P_c2, P_c3 = fn0[1:]
                return _idris_Python_46_Lib_46_Numpy_46_fromDyn(P_c0, P_c1, P_c2, P_c3, arg0)
            else:
              if fn0[0] == 65660:  # {U_Python.Lib.Numpy.unsafeNumpy1}
                P_c0 = fn0[1]
                return _idris_Python_46_Lib_46_Numpy_46_unsafeNumpy(P_c0, arg0)
              elif fn0[0] == 65661:  # {U_Python.Lib.Numpy.{array0}1}
                P_c0 = fn0[1]
                return _idris_Python_46_Lib_46_Numpy_46__123_array0_125_(P_c0, arg0)
              else:  # {U_Python.Lib.Numpy.{array1}1}
                return _idris_Python_46_Lib_46_Numpy_46__123_array1_125_(arg0)
        else:
          if fn0[0] < 65667:
            if fn0[0] < 65665:
              if fn0[0] == 65663:  # {U_Python.Lib.Numpy.{array2}1}
                P_c0, P_c1 = fn0[1:]
                return _idris_Python_46_Lib_46_Numpy_46__123_array2_125_(P_c0, P_c1, arg0)
              else:  # {U_Python.Lib.Numpy.{binop0}1}
                P_c0, P_c1, P_c2 = fn0[1:]
                return _idris_Python_46_Lib_46_Numpy_46__123_binop0_125_(P_c0, P_c1, P_c2, arg0)
            else:
              if fn0[0] == 65665:  # {U_Python.Lib.Numpy.{dot0}1}
                P_c0, P_c1 = fn0[1:]
                return _idris_Python_46_Lib_46_Numpy_46__123_dot0_125_(P_c0, P_c1, arg0)
              else:  # {U_Python.Lib.Numpy.{fromDyn0}1}
                P_c0, P_c1, P_c2 = fn0[1:]
                return _idris_Python_46_Lib_46_Numpy_46__123_fromDyn0_125_(P_c0, P_c1, P_c2, arg0)
          else:
            if fn0[0] < 65669:
              if fn0[0] == 65667:  # {U_Python.Lib.Numpy.{fromDyn1}1}
                P_c0, P_c1, P_c2, P_c3 = fn0[1:]
                return _idris_Python_46_Lib_46_Numpy_46__123_fromDyn1_125_(
                  P_c0, P_c1, P_c2, P_c3, arg0
                )
              else:  # {U_Python.Lib.Numpy.{import_0}1}
                return _idris_Python_46_Lib_46_Numpy_46__123_import_95_0_125_(arg0)
            else:
              if fn0[0] == 65669:  # {U_Python.Lib.Numpy.{reshape0}1}
                P_c0, P_c1, P_c2 = fn0[1:]
                return _idris_Python_46_Lib_46_Numpy_46__123_reshape0_125_(P_c0, P_c1, P_c2, arg0)
              elif fn0[0] == 65670:  # {U_Python.Lib.Numpy.{transpose0}1}
                P_c0 = fn0[1]
                return _idris_Python_46_Lib_46_Numpy_46__123_transpose0_125_(P_c0, arg0)
              else:  # {U_Python.Lib.Numpy.{unsafeNpMtx0}1}
                return _idris_Python_46_Lib_46_Numpy_46__123_unsafeNpMtx0_125_(arg0)
    else:
      if fn0[0] < 65690:
        if fn0[0] < 65681:
          if fn0[0] < 65676:
            if fn0[0] < 65674:
              if fn0[0] == 65672:  # {U_Python.RTS.getGlobal1}
                P_c0 = fn0[1]
                return _idris_Python_46_RTS_46_getGlobal(P_c0, arg0)
              else:  # {U_Python.RTS.importModule1}
                P_c0 = fn0[1]
                return _idris_Python_46_RTS_46_importModule(P_c0, arg0)
            else:
              if fn0[0] == 65674:  # {U_Python.{$.0}1}
                P_c0, P_c1 = fn0[1:]
                return _idris_Python_46__123__36__46_0_125_(P_c0, P_c1, arg0)
              else:  # {U_Python.{/.0}1}
                P_c0, P_c1 = fn0[1:]
                return _idris_Python_46__123__47__46_0_125_(P_c0, P_c1, arg0)
          else:
            if fn0[0] < 65678:
              if fn0[0] == 65676:  # {U_Python.{toString0}1}
                P_c0 = fn0[1]
                return _idris_Python_46__123_toString0_125_(P_c0, arg0)
              else:  # {U_believe_me1}
                P_c0, P_c1 = fn0[1:]
                return _idris_believe_95_me(P_c0, P_c1, arg0)
            else:
              if fn0[0] == 65678:  # {U_io_bind1}
                P_c0, P_c1, P_c2, P_c3, P_c4 = fn0[1:]
                return _idris_io_95_bind(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
              elif fn0[0] == 65679:  # {U_io_return1}
                P_c0, P_c1, P_c2 = fn0[1:]
                return _idris_io_95_return(P_c0, P_c1, P_c2, arg0)
              else:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac60}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac60_125_(
                  arg0
                )
        else:
          if fn0[0] < 65685:
            if fn0[0] < 65683:
              if fn0[0] == 65681:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac610}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac610_125_(
                  arg0
                )
              else:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac611}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac611_125_(
                  arg0
                )
            else:
              if fn0[0] == 65683:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac612}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac612_125_(
                  arg0
                )
              else:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac613}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac613_125_(
                  arg0
                )
          else:
            if fn0[0] < 65687:
              if fn0[0] == 65685:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac614}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac614_125_(
                  arg0
                )
              else:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac615}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac615_125_(
                  arg0
                )
            else:
              if fn0[0] == 65687:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac616}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac616_125_(
                  arg0
                )
              elif fn0[0] == 65688:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac617}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac617_125_(
                  arg0
                )
              else:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac61}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac61_125_(
                  arg0
                )
      else:
        if fn0[0] < 65699:
          if fn0[0] < 65694:
            if fn0[0] < 65692:
              if fn0[0] == 65690:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac62}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac62_125_(
                  arg0
                )
              else:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac63}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac63_125_(
                  arg0
                )
            else:
              if fn0[0] == 65692:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac64}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac64_125_(
                  arg0
                )
              else:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac65}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac65_125_(
                  arg0
                )
          else:
            if fn0[0] < 65696:
              if fn0[0] == 65694:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac66}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac66_125_(
                  arg0
                )
              else:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac67}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac67_125_(
                  arg0
                )
            else:
              if fn0[0] == 65696:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac68}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac68_125_(
                  arg0
                )
              elif fn0[0] == 65697:  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac69}1}
                return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac69_125_(
                  arg0
                )
              else:  # {U_{Python.Lib.Numpy.array, mkList_lam0}1}
                P_c0 = fn0[1]
                return _idris__123_Python_46_Lib_46_Numpy_46_array_44__32_mkList_95_lam0_125_(
                  P_c0, arg0
                )
        else:
          if fn0[0] < 65703:
            if fn0[0] < 65701:
              if fn0[0] == 65699:  # {U_{Python.Lib.Numpy.array, mkList_lam1}1}
                return _idris__123_Python_46_Lib_46_Numpy_46_array_44__32_mkList_95_lam1_125_(arg0)
              else:  # {U_{io_bind1}1}
                P_c0, P_c1, P_c2, P_c3, P_c4, P_c5 = fn0[1:]
                return io_bind1(P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, arg0)
            else:
              if fn0[0] == 65701:  # {U_{unsafePerformIO0}1}
                return unsafePerformIO0(arg0)
              else:  # {U_Data.VectType.Vect.Vect n instance of Prelude.Functor.Functor2}
                P_c0, P_c1, P_c2 = fn0[1:]
                return (65636, P_c0, P_c1, P_c2, arg0)  # {U_Data.VectType.Vect.Vect n instance of Prelude.Functor.Functor1}
          else:
            if fn0[0] < 65705:
              if fn0[0] == 65703:  # {U_Prelude.Nat.Nat instance of Prelude.Classes.Eq2}
                return (65647, arg0)  # {U_Prelude.Nat.Nat instance of Prelude.Classes.Eq1}
              else:  # {U_Python.Lib.Numpy.binop2}
                P_c0, P_c1, P_c2, P_c3, P_c4, P_c5 = fn0[1:]
                return (65658, P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, arg0)  # {U_Python.Lib.Numpy.binop1}
            else:
              if fn0[0] == 65705:  # {U_Data.VectType.Vect.Vect n instance of Prelude.Functor.Functor3}
                P_c0, P_c1 = fn0[1:]
                return (65702, P_c0, P_c1, arg0)  # {U_Data.VectType.Vect.Vect n instance of Prelude.Functor.Functor2}
              else:  # {U_Data.VectType.Vect.Vect n instance of Prelude.Functor.Functor4}
                P_c0 = fn0[1]
                return (65705, P_c0, arg0)  # {U_Data.VectType.Vect.Vect n instance of Prelude.Functor.Functor3}
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

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac60}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac60_125_(
  in1
):
  while True:
    if not in1:  # Prelude.Bool.False
      return "False"
    else:  # Prelude.Bool.True
      return "True"
    return _idris_error("unreachable due to case in tail position")

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
    return (65679, None, None, APPLY0(e3, in0))  # {U_io_return1}

# Prelude.Classes.{Prelude.Show.Prec instance of Prelude.Classes.Ord, method >=_lam0}
def _idris_Prelude_46_Classes_46__123_Prelude_46_Show_46_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__62__61__95_lam0_125_(
  e0, e1
):
  while True:
    return _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Eq_36_Prec_58__33__61__61__58_0(
      e0, e1
    )

# {Python.Lib.Numpy.array, mkList_lam0}
def _idris__123_Python_46_Lib_46_Numpy_46_array_44__32_mkList_95_lam0_125_(in0, in1):
  while True:
    return in1.cons(in0)

# Python.Lib.Numpy.{array0}
def _idris_Python_46_Lib_46_Numpy_46__123_array0_125_(in1, in2):
  while True:
    return in2.cons(in1)

# Python.Lib.Numpy.{binop0}
def _idris_Python_46_Lib_46_Numpy_46__123_binop0_125_(e4, e6, e7, in0):
  while True:
    return _idris_Python_46__36__46_(
      None,
      None,
      None,
      _idris_Python_46__47__46_(None, None, in0, e4, None),
      None,
      (1, e6, (1, e7, (0,)))  # Python.::, Python.::, Python.Nil
    )

# Python.Lib.Numpy.{dot0}
def _idris_Python_46_Lib_46_Numpy_46__123_dot0_125_(e5, e6, in0):
  while True:
    return _idris_Python_46__36__46_(
      None,
      None,
      None,
      _idris_Python_46__47__46_(None, None, in0, "dot", None),
      None,
      (1, e5, (1, e6, (0,)))  # Python.::, Python.::, Python.Nil
    )

# Python.Lib.Numpy.{fromDyn0}
def _idris_Python_46_Lib_46_Numpy_46__123_fromDyn0_125_(in0, e1, e2, in1):
  while True:
    return _idris_Python_46__36__46_(
      None,
      None,
      None,
      _idris_Python_46__47__46_(None, None, in0, "tile", None),
      None,
      (
        1,  # Python.::
        in1,
        (
          1,  # Python.::
          _idris_Python_46_Builtins_46_toPyList(None, ConsList().cons(e2).cons(e1)),
          (0,)  # Python.Nil
        )
      )
    )

# Python.Lib.Numpy.{import_0}
def _idris_Python_46_Lib_46_Numpy_46__123_import_95_0_125_(in0):
  while True:
    return in0

# {io_bind0}
def io_bind0(e0, e1, e2, e3, e4, _idris_w, in0):
  while True:
    return APPLY0(e4, in0)

# Main.{main0}
def _idris_Main_46__123_main0_125_(in4):
  while True:
    return _idris_Prelude_46_Interactive_46_putStr(
      None,
      (_idris_Python_46_toString(
        APPLY0(
          APPLY0(
            _idris_Python_46_Lib_46_Numpy_46_add(None, None, None, None),
            APPLY0(
              APPLY0(
                _idris_Python_46_Lib_46_Numpy_46_mul(None, None, None, None),
                APPLY0(_idris_Python_46_Lib_46_Numpy_46_fromInteger(None, 4, 4, "float"), 2)
              ),
              _idris_Python_46_Lib_46_Numpy_46_transpose(
                None,
                None,
                None,
                None,
                _idris_Python_46_Lib_46_Numpy_46_dot(
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
          APPLY0(_idris_Python_46_Lib_46_Numpy_46_fromFloat(None, 4, 4, "float"), 0.2)
        )
      ) + "\n")
    )

# Prelude.Interactive.{putStr0}
def _idris_Prelude_46_Interactive_46__123_putStr0_125_(e1, in0):
  while True:
    return sys.stdout.write(e1)

# Python.Lib.Numpy.{reshape0}
def _idris_Python_46_Lib_46_Numpy_46__123_reshape0_125_(e6, e4, e5, in0):
  while True:
    return _idris_Python_46__36__46_(
      None,
      None,
      None,
      _idris_Python_46__47__46_(None, None, in0, "reshape", None),
      None,
      (
        1,  # Python.::
        e6,
        (
          1,  # Python.::
          _idris_Python_46_Builtins_46_toPyList(None, ConsList().cons(e5).cons(e4)),
          (0,)  # Python.Nil
        )
      )
    )

# {runMain0}
def runMain0():
  while True:
    return EVAL0(APPLY0(_idris_Main_46_main(), None))

# Python.{toString0}
def _idris_Python_46__123_toString0_125_(e0, in0):
  while True:
    return str(e0)

# Python.Lib.Numpy.{transpose0}
def _idris_Python_46_Lib_46_Numpy_46__123_transpose0_125_(e4, in0):
  while True:
    return _idris_Python_46__36__46_(
      None,
      None,
      None,
      _idris_Python_46__47__46_(None, None, in0, "transpose", None),
      None,
      (1, e4, (0,))  # Python.::, Python.Nil
    )

# Python.Lib.Numpy.{unsafeNpMtx0}
def _idris_Python_46_Lib_46_Numpy_46__123_unsafeNpMtx0_125_(in0):
  while True:
    return in0

# {unsafePerformIO0}
def unsafePerformIO0(in0):
  while True:
    return in0

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam1}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
  in0
):
  while True:
    return (65648, in0)  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam0}1}

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac61}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac61_125_(
  in3
):
  while True:
    if not in3:  # Prelude.Bool.False
      return "False"
    else:  # Prelude.Bool.True
      return "True"
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam1}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
  in0
):
  while True:
    return (65652, in0)  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam0}1}

# {Python.Lib.Numpy.array, mkList_lam1}
def _idris__123_Python_46_Lib_46_Numpy_46_array_44__32_mkList_95_lam1_125_(in0):
  while True:
    return (65698, in0)  # {U_{Python.Lib.Numpy.array, mkList_lam0}1}

# Python.Lib.Numpy.{array1}
def _idris_Python_46_Lib_46_Numpy_46__123_array1_125_(in1):
  while True:
    return (65661, in1)  # {U_Python.Lib.Numpy.{array0}1}

# Python.Lib.Numpy.{fromDyn1}
def _idris_Python_46_Lib_46_Numpy_46__123_fromDyn1_125_(e4, e3, e1, e2, in0):
  while True:
    return (
      65678,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Python_46__36__46_(
        None,
        None,
        None,
        _idris_Python_46__47__46_(None, None, in0, "array", None),
        None,
        (1, e4, (1, e3, (0,)))  # Python.::, Python.::, Python.Nil
      ),
      (65666, in0, e1, e2)  # {U_Python.Lib.Numpy.{fromDyn0}1}
    )

# {io_bind1}
def io_bind1(e0, e1, e2, e3, e4, _idris_w, in0):
  while True:
    return APPLY0(io_bind0(e0, e1, e2, e3, e4, _idris_w, in0), _idris_w)

# Main.{main1}
def _idris_Main_46__123_main1_125_(in3):
  while True:
    return (
      65678,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr(
        None,
        (_idris_Python_46_toString(
          _idris_Python_46_Lib_46_Numpy_46_dot(
            None,
            None,
            None,
            None,
            None,
            _idris_Main_46_xs(),
            _idris_Main_46_zs()
          )
        ) + "\n")
      ),
      (65637,)  # {U_Main.{main0}1}
    )

# Prelude.Interactive.{putStr1}
def _idris_Prelude_46_Interactive_46__123_putStr1_125_(in1):
  while True:
    return (65679, None, None, Unit)  # {U_io_return1}

# {unsafePerformIO1}
def unsafePerformIO1(e0, e1, e2):
  while True:
    return (65701,)  # {U_{unsafePerformIO0}1}

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

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac62}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac62_125_(
  in2
):
  while True:
    return (65689,)  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac61}1}

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

# Python.Lib.Numpy.{array2}
def _idris_Python_46_Lib_46_Numpy_46__123_array2_125_(e4, e3, in0):
  while True:
    return _idris_Python_46__36__46_(
      None,
      None,
      None,
      _idris_Python_46__47__46_(None, None, in0, "array", None),
      None,
      (
        1,  # Python.::
        APPLY0(
          _idris_Python_46_toDyn(None),
          _idris_Python_46_Builtins_46_toPyList(
            None,
            _idris_Data_46_VectType_46_Vect_46_foldrImpl(
              None,
              None,
              None,
              (65662,),  # {U_Python.Lib.Numpy.{array1}1}
              ConsList(),
              (65643, None),  # {U_Prelude.Basics.id1}
              _idris_Prelude_46_Functor_46_Data_46_VectType_46_Vect_46__64_Prelude_46_Functor_46_Functor_36_Vect_32_n_58__33_map_58_0(
                None,
                None,
                None,
                None,
                (65657, None, None, None, None, None, None, None),  # {U_Python.Lib.Numpy.array, mkList1}
                e4
              )
            )
          )
        ),
        (1, e3, (0,))  # Python.::, Python.Nil
      )
    )

# {io_bind2}
def io_bind2(e0, e1, e2, e3, e4, _idris_w):
  while True:
    return (65700, e0, e1, e2, e3, e4, _idris_w)  # {U_{io_bind1}1}

# Main.{main2}
def _idris_Main_46__123_main2_125_(in2):
  while True:
    return (
      65678,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr(
        None,
        (_idris_Python_46_toString(_idris_Main_46_zs()) + "\n")
      ),
      (65638,)  # {U_Main.{main1}1}
    )

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam3}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
  in2
):
  while True:
    return (65650, in2)  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam2}1}

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac63}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac63_125_(
  in0
):
  while True:
    return ("[" + (_idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
      None,
      None,
      None,
      (0, (65680,), (65690,)),  # constructor of Prelude.Show.Show, {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac60}1}, {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac62}1}
      "",
      in0
    ) + "]"))

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam3}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
  in2
):
  while True:
    return (65654, in2)  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam2}1}

# Main.{main3}
def _idris_Main_46__123_main3_125_(in1):
  while True:
    return (
      65678,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr(
        None,
        (_idris_Python_46_toString(_idris_Main_46_ys()) + "\n")
      ),
      (65639,)  # {U_Main.{main2}1}
    )

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac64}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac64_125_(
  in6
):
  while True:
    if not in6:  # Prelude.Bool.False
      return "False"
    else:  # Prelude.Bool.True
      return "True"
    return _idris_error("unreachable due to case in tail position")

# Main.{main4}
def _idris_Main_46__123_main4_125_(in0):
  while True:
    return (
      65678,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_Prelude_46_Interactive_46_putStr(
        None,
        (_idris_Python_46_toString(_idris_Main_46_xs()) + "\n")
      ),
      (65640,)  # {U_Main.{main3}1}
    )

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac65}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac65_125_(
  in8
):
  while True:
    if not in8:  # Prelude.Bool.False
      return "False"
    else:  # Prelude.Bool.True
      return "True"
    return _idris_error("unreachable due to case in tail position")

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac66}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac66_125_(
  in7
):
  while True:
    return (65693,)  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac65}1}

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac67}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac67_125_(
  in5
):
  while True:
    return _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0(
      None,
      None,
      (0, (65692,), (65694,)),  # constructor of Prelude.Show.Show, {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac64}1}, {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac66}1}
      in5
    )

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac68}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac68_125_(
  in4
):
  while True:
    return (65695,)  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac67}1}

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac69}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac69_125_(
  in1
):
  while True:
    if not in1:  # Prelude.Bool.False
      return "False"
    else:  # Prelude.Bool.True
      return "True"
    return _idris_error("unreachable due to case in tail position")

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac610}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac610_125_(
  in3
):
  while True:
    if not in3:  # Prelude.Bool.False
      return "False"
    else:  # Prelude.Bool.True
      return "True"
    return _idris_error("unreachable due to case in tail position")

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac611}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac611_125_(
  in2
):
  while True:
    return (65681,)  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac610}1}

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac612}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac612_125_(
  in0
):
  while True:
    return ("[" + (_idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
      None,
      None,
      None,
      (0, (65697,), (65682,)),  # constructor of Prelude.Show.Show, {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac69}1}, {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac611}1}
      "",
      in0
    ) + "]"))

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac613}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac613_125_(
  in6
):
  while True:
    if not in6:  # Prelude.Bool.False
      return "False"
    else:  # Prelude.Bool.True
      return "True"
    return _idris_error("unreachable due to case in tail position")

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac614}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac614_125_(
  in8
):
  while True:
    if not in8:  # Prelude.Bool.False
      return "False"
    else:  # Prelude.Bool.True
      return "True"
    return _idris_error("unreachable due to case in tail position")

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac615}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac615_125_(
  in7
):
  while True:
    return (65685,)  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac614}1}

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac616}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac616_125_(
  in5
):
  while True:
    return _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0(
      None,
      None,
      (0, (65684,), (65686,)),  # constructor of Prelude.Show.Show, {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac613}1}, {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac615}1}
      in5
    )

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac617}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac617_125_(
  in4
):
  while True:
    return (65687,)  # {U_{PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac616}1}

# Python.$., fromHList
def _idris_Python_46__36__46__58_fromHList_58_0(e0, e1, e2, e3, e4, e5, e6, e7):
  while True:
    if e7[0] == 1:  # Python.::
      in0, in1 = e7[1:]
      return _idris_Python_46__36__46__58_fromHList_58_0(
        None, None, None, None, None, None, None, in1
      ).cons(_idris_believe_95_me(None, None, in0))
    else:  # Python.Nil
      return ConsList()
    return _idris_error("unreachable due to case in tail position")

# Python.Lib.Numpy.array, mkList
def _idris_Python_46_Lib_46_Numpy_46_array_58_mkList_58_0(
  e0, e1, e2, e3, e4, e5, e6, e7
):
  while True:
    return _idris_Python_46_Builtins_46_toPyList(
      None,
      _idris_Data_46_VectType_46_Vect_46_foldrImpl(
        None,
        None,
        None,
        (65699,),  # {U_{Python.Lib.Numpy.array, mkList_lam1}1}
        ConsList(),
        (65643, None),  # {U_Prelude.Basics.id1}
        e7
      )
    )

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

# Prelude.Show.Prelude.Show.List a instance of Prelude.Show.Show, method show, show'
def _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
  e0, e1, e2, e3, e4, e5
):
  while True:
    if e5:  # Prelude.List.::
      in0, in1 = e5.head, e5.tail
      if not in1:  # Prelude.List.Nil
        return (e4 + APPLY0(_idris_Prelude_46_Show_46_show(None, e3), in0))
      else:
        e0, e1, e2, e3, e4, e5, = None, None, None, e3, (e4 + (APPLY0(_idris_Prelude_46_Show_46_show(None, e3), in0) + ", ")), in1,
        continue
        return _idris_error("unreachable due to tail call")
      return _idris_error("unreachable due to case in tail position")
    else:  # Prelude.List.Nil
      return e4
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
        return APPLY0(APPLY0(_idris_Prelude_46_Classes_46__61__61_(None, (65703,)), in1), in0)  # {U_Prelude.Nat.Nat instance of Prelude.Classes.Eq2}
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

# Prelude.Functor.Prelude.Monad.IO' ffi instance of Prelude.Functor.Functor, method map
def _idris_Prelude_46_Functor_46_Prelude_46_Monad_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
  e0, e1, e2, e3, e4
):
  while True:
    return (65678, None, None, None, e4, (65644, e3))  # {U_io_bind1}, {U_Prelude.Functor.{Prelude.Monad.IO' ffi instance of Prelude.Functor.Functor, method map_lam0}1}

# Prelude.Functor.Data.VectType.Vect.Vect n instance of Prelude.Functor.Functor, method map
def _idris_Prelude_46_Functor_46_Data_46_VectType_46_Vect_46__64_Prelude_46_Functor_46_Functor_36_Vect_32_n_58__33_map_58_0(
  e0, e1, e2, e3, e4, e5
):
  while True:
    if e5[0] == 1:  # Data.VectType.Vect.::
      in0, in1 = e5[1:]
      return (
        1,  # Data.VectType.Vect.::
        APPLY0(e4, in0),
        APPLY0(
          APPLY0(_idris_Prelude_46_Functor_46_map(None, None, None, (65706, None)), e4),  # {U_Data.VectType.Vect.Vect n instance of Prelude.Functor.Functor4}
          in1
        )
      )
    else:  # Data.VectType.Vect.Nil
      return (0,)  # Data.VectType.Vect.Nil
    return _idris_error("unreachable due to case in tail position")

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

# Prelude.Show.Prelude.Show.Bool instance of Prelude.Show.Show, method show
def _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_Bool_58__33_show_58_0(
  e0
):
  while True:
    if not e0:  # Prelude.Bool.False
      return "False"
    else:  # Prelude.Bool.True
      return "True"
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.Prelude.Show.List a instance of Prelude.Show.Show, method show
def _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0(
  e0, e1, e2, e3
):
  while True:
    return ("[" + (_idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
      None, None, None, e2, "", e3
    ) + "]"))

# Prelude.Show.Python.Lib.Numpy.Matrix m n ty instance of Prelude.Show.Show, method show
def _idris_Prelude_46_Show_46_Python_46_Lib_46_Numpy_46__64_Prelude_46_Show_46_Show_36_Matrix_32_m_32_n_32_ty_58__33_show_58_0(
  e0, e1, e2, e3, e4
):
  while True:
    return _idris_Python_46_toString(e4)

# Prelude.Show.Prelude.Show.Maybe a instance of Prelude.Show.Show, method showPrec
def _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_Maybe_32_a_58__33_showPrec_58_0(
  e0, e1, e2, e3, e4
):
  while True:
    if e4 is not None:  # Prelude.Maybe.Just
      in0 = e4
      aux1 = _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33__62__61__58_0(
        e3, (6,)  # Prelude.Show.App
      )
      if not aux1:  # Prelude.Bool.False
        return ("Just" + (" " + APPLY0(APPLY0(_idris_Prelude_46_Show_46_showPrec(None, e2), None), in0)))
      else:  # Prelude.Bool.True
        return ("(" + (("Just" + (" " + APPLY0(APPLY0(_idris_Prelude_46_Show_46_showPrec(None, e2), None), in0))) + ")"))
      return _idris_error("unreachable due to case in tail position")
    else:  # Prelude.Maybe.Nothing
      return "Nothing"
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

# with block in Prelude.Classes.Prelude.Nat.Nat instance of Prelude.Classes.Ord, method >
def _idris__95_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat_58__33__62__58_0_95_with_95_83(
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

# Data.VectType.Vect.Vect n instance of Prelude.Functor.Functor
def _idris_Data_46_VectType_46_Vect_46__64_Prelude_46_Functor_46_Functor_36_Vect_32_n(
  e0, meth0, meth1, meth2, meth3
):
  while True:
    return _idris_Prelude_46_Functor_46_Data_46_VectType_46_Vect_46__64_Prelude_46_Functor_46_Functor_36_Vect_32_n_58__33_map_58_0(
      None, None, None, None, meth2, meth3
    )

# Prelude.Nat.Nat instance of Prelude.Classes.Ord
def _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat():
  while True:
    return (0, (65649,), (65651,))  # constructor of Prelude.Classes.Ord, {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam1}1}, {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam3}1}

# Prelude.Show.Prec instance of Prelude.Classes.Ord
def _idris_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec():
  while True:
    return (0, (65653,), (65655,))  # constructor of Prelude.Classes.Ord, {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam1}1}, {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam3}1}

# case block in Void
def _idris_Void_95_case():
  while True:
    return None

# case block in io_bind
def _idris_io_95_bind_95_case(e0, e1, e2, e3, e4, e5, e6, e7):
  while True:
    return APPLY0(e7, e5)

# <<Void eliminator>>
def _idris_Void_95_elim():
  while True:
    return None

if __name__ == '__main__':
  runMain0()
