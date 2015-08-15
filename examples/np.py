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
def _idris_Python_46__36__46_(e0, e1, _idris_w):
  while True:
    return _idris_call(e0, e1)

# Python.$:
def _idris_Python_46__36__58_(e0, e1):
  while True:
    return (U_io_bind1, None, None, None, e0, (_idris__123_U_95_Python_46__123__36__58_0_125_1_125_, e1,),)

# Prelude.Basics..
def _idris_Prelude_46_Basics_46__46_(e0, e1, e2, e3, e4, _idris_x):
  while True:
    return e3(e4(_idris_x))

# Python./.
def _idris_Python_46__47__46_(e0, e1, _idris_w):
  while True:
    return getattr(e0, e1)

# Python./:
def _idris_Python_46__47__58_(e0, e1):
  while True:
    return (U_io_bind1, None, None, None, e0, (_idris__123_U_95_Python_46__123__47__58_0_125_1_125_, e1,),)

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
        return ("Just" + (" " + _idris_Prelude_46_Show_46_showPrec(
          None,
          (
            0,  # constructor of Prelude.Show.Show
            (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac63_125_1_125_,),
            (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac68_125_1_125_,)
          )
        )(
          None
        )(
          in9
        )))
      else:  # Prelude.Bool.True
        return ("(" + (("Just" + (" " + _idris_Prelude_46_Show_46_showPrec(
          None,
          (
            0,  # constructor of Prelude.Show.Show
            (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac612_125_1_125_,),
            (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac617_125_1_125_,)
          )
        )(
          None
        )(
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

# Python.Lib.Numpy.array
def _idris_Python_46_Lib_46_Numpy_46_array(e0, e1, e2, e3, e4):
  while True:
    return _idris_Python_46_Lib_46_Numpy_46_unsafeNpArr(None, None, None, None)(
      (_idris__123_U_95_Python_46_Lib_46_Numpy_46__123_array0_125_1_125_, e4, e3,)
    )

# believe_me
def _idris_believe_95_me(e0, e1, e2):
  while True:
    return e2

# call__IO
def _idris_call_95__95_IO(e0, e1, e2):
  while True:
    return e2(None)

# Prelude.Classes.compare
def _idris_Prelude_46_Classes_46_compare(e0, e1):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Classes.Ord
    in0, in1 = e1[1:]
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

# Data.VectType.Vect.foldrImpl
def _idris_Data_46_VectType_46_Vect_46_foldrImpl(e0, e1, e2, e3, e4, e5, e6):
  while True:
    if e6[0] == 1:  # Data.VectType.Vect.::
      in0, in1 = e6[1:]
      e0, e1, e2, e3, e4, e5, e6, = None, None, None, e3, e4, (_idris__123_U_95_Prelude_46_Basics_46__46_1_125_, None, None, None, e5, e3(in0),), in1,
      continue
      return _idris_error("unreachable due to tail call")
    else:  # Data.VectType.Vect.Nil
      return e5(e4)
    return _idris_error("unreachable due to case in tail position")

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
    return io_bind2(e0, e1, e2, e3, e4, _idris_w)(e3(_idris_w))

# io_return
def _idris_io_95_return(e0, e1, e2, _idris_w):
  while True:
    return e2

# Main.main
def _idris_Main_46_main():
  while True:
    return (U_io_bind1, None, None, None, _idris_PE_95_printLn_95_140bcc5c(None, _idris_Main_46_f(4)), (_idris__123_U_95_Main_46__123_main1_125_1_125_,),)

# Prelude.Functor.map
def _idris_Prelude_46_Functor_46_map(e0, e1, e2, e3):
  while True:
    return e3(e1)(e2)

# Python.Builtins.mk
def _idris_Python_46_Builtins_46_mk(e0):
  while True:
    return _idris_unsafePerformIO(
      None,
      None,
      (_idris__123_U_95_Python_46__47__46_1_125_, _idris_unsafePerformIO(
        None,
        None,
        (_idris__123_U_95_Python_46_RTS_46_getGlobal1_125_, "__builtins__",)
      ), e0,)
    )

# mkForeignPrim
def _idris_mkForeignPrim():
  while True:
    return None

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
    return e3(e2)

# Prelude.Interactive.putStr
def _idris_Prelude_46_Interactive_46_putStr(e0, e1):
  while True:
    return (U_io_bind1, None, None, None, (_idris__123_U_95_Prelude_46_Interactive_46__123_putStr0_125_1_125_, e1,), (_idris__123_U_95_Prelude_46_Interactive_46__123_putStr1_125_1_125_,),)

# Python.Lib.Numpy.reshape
def _idris_Python_46_Lib_46_Numpy_46_reshape(e0, e1, e2, e3, e4, e5, e6, e7):
  while True:
    return _idris_Python_46_Lib_46_Numpy_46_unsafeNpArr(None, None, None, None)(
      (_idris__123_U_95_Python_46_Lib_46_Numpy_46__123_reshape0_125_1_125_, e6, e4, e5,)
    )

# run__IO
def _idris_run_95__95_IO(e0, e1):
  while True:
    return e1(None)

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

# Python.Builtins.toList
def _idris_Python_46_Builtins_46_toList(e0):
  while True:
    return _idris_unsafePerformIO(
      None,
      None,
      (_idris__123_U_95_Python_46__36__46_1_125_, _idris_Python_46_Builtins_46_mk("list"), ConsList().cons(e0),)
    )

# Python.toRef
def _idris_Python_46_toRef(e0):
  while True:
    return (U_believe_me1, None, None,)

# Python.toString
def _idris_Python_46_toString(e0):
  while True:
    return _idris_unsafePerformIO(
      None,
      None,
      (_idris__123_U_95_Python_46__123_toString0_125_1_125_, e0,)
    )

# Python.Lib.Numpy.unsafeNpArr
def _idris_Python_46_Lib_46_Numpy_46_unsafeNpArr(e0, e1, e2, e3):
  while True:
    return (_idris__123_U_95_Prelude_46_Basics_46__46_1_125_, None, None, None, (_idris__123_U_95_Python_46_Lib_46_Numpy_46__123_unsafeNpArr0_125_1_125_,), (_idris__123_U_95_Python_46_Lib_46_Numpy_46_unsafeNumpy1_125_, None,),)

# Python.Lib.Numpy.unsafeNumpy
def _idris_Python_46_Lib_46_Numpy_46_unsafeNumpy(e0, e1):
  while True:
    return _idris_unsafePerformIO(
      None,
      None,
      (U_io_bind1, None, None, None, (_idris__123_U_95_Python_46_RTS_46_importModule1_125_, "numpy",), e1,)
    )

# unsafePerformIO
def _idris_unsafePerformIO(e0, e1, e2):
  while True:
    return unsafePerformIO1(e0, e1, e2)(e2(None))

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

# Prelude.Bool.||
def _idris_Prelude_46_Bool_46__124__124_(e0, e1):
  while True:
    if not e0:  # Prelude.Bool.False
      return EVAL0(e1)
    else:  # Prelude.Bool.True
      return True
    return _idris_error("unreachable due to case in tail position")

# Python.{$:0}
def _idris_Python_46__123__36__58_0_125_(e1, in0):
  while True:
    return (_idris__123_U_95_Python_46__36__46_1_125_, in0, e1,)

# Python.{/:0}
def _idris_Python_46__123__47__58_0_125_(e1, in0):
  while True:
    return (_idris__123_U_95_Python_46__47__46_1_125_, in0, e1,)

def _idris__123_U_95_Data_46_VectType_46_Vect_46_Vect_32_n_32_instance_32_of_32_Prelude_46_Functor_46_Functor1_125_(arg0):
  P_c0, P_c1, P_c2, P_c3 = _idris_arg[1:]
  return _idris_Data_46_VectType_46_Vect_46__64_Prelude_46_Functor_46_Functor_36_Vect_32_n(
    P_c0, P_c1, P_c2, P_c3, arg0
  )

def _idris__123_U_95_Main_46__123_main0_125_1_125_(arg0):
  return _idris_Main_46__123_main0_125_(arg0)

def _idris__123_U_95_Main_46__123_main1_125_1_125_(arg0):
  return _idris_Main_46__123_main1_125_(arg0)

def _idris__123_U_95_Prelude_46_Basics_46__46_1_125_(arg0):
  P_c0, P_c1, P_c2, P_c3, P_c4 = _idris_arg[1:]
  return _idris_Prelude_46_Basics_46__46_(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)

def _idris__123_U_95_Prelude_46_Basics_46_id1_125_(arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Basics_46_id(P_c0, arg0)

def _idris__123_U_95_Prelude_46_Interactive_46__123_putStr0_125_1_125_(arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Interactive_46__123_putStr0_125_(P_c0, arg0)

def _idris__123_U_95_Prelude_46_Interactive_46__123_putStr1_125_1_125_(arg0):
  return _idris_Prelude_46_Interactive_46__123_putStr1_125_(arg0)

def _idris__123_U_95_Prelude_46_Nat_46_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Eq1_125_(arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Eq_36_Nat(P_c0, arg0)

def _idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_1_125_(arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
    P_c0, arg0
  )

def _idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_1_125_(arg0):
  return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
    arg0
  )

def _idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_1_125_(arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
    P_c0, arg0
  )

def _idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_1_125_(arg0):
  return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
    arg0
  )

def _idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_1_125_(arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
    P_c0, arg0
  )

def _idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_1_125_(arg0):
  return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
    arg0
  )

def _idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_1_125_(arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
    P_c0, arg0
  )

def _idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_1_125_(arg0):
  return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
    arg0
  )

def _idris__123_U_95_Python_46__36__46_1_125_(arg0):
  P_c0, P_c1 = _idris_arg[1:]
  return _idris_Python_46__36__46_(P_c0, P_c1, arg0)

def _idris__123_U_95_Python_46__47__46_1_125_(arg0):
  P_c0, P_c1 = _idris_arg[1:]
  return _idris_Python_46__47__46_(P_c0, P_c1, arg0)

def _idris__123_U_95_Python_46_Lib_46_Numpy_46_array_44__32_mkList1_125_(arg0):
  P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, P_c6 = _idris_arg[1:]
  return _idris_Python_46_Lib_46_Numpy_46_array_58_mkList_58_0(
    P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, P_c6, arg0
  )

def _idris__123_U_95_Python_46_Lib_46_Numpy_46_unsafeNumpy1_125_(arg0):
  P_c0 = _idris_arg[1]
  return _idris_Python_46_Lib_46_Numpy_46_unsafeNumpy(P_c0, arg0)

def _idris__123_U_95_Python_46_Lib_46_Numpy_46__123_array0_125_1_125_(arg0):
  P_c0, P_c1 = _idris_arg[1:]
  return _idris_Python_46_Lib_46_Numpy_46__123_array0_125_(P_c0, P_c1, arg0)

def _idris__123_U_95_Python_46_Lib_46_Numpy_46__123_reshape0_125_1_125_(arg0):
  P_c0, P_c1, P_c2 = _idris_arg[1:]
  return _idris_Python_46_Lib_46_Numpy_46__123_reshape0_125_(P_c0, P_c1, P_c2, arg0)

def _idris__123_U_95_Python_46_Lib_46_Numpy_46__123_unsafeNpArr0_125_1_125_(arg0):
  return _idris_Python_46_Lib_46_Numpy_46__123_unsafeNpArr0_125_(arg0)

def _idris__123_U_95_Python_46_RTS_46_getGlobal1_125_(arg0):
  P_c0 = _idris_arg[1]
  return _idris_Python_46_RTS_46_getGlobal(P_c0, arg0)

def _idris__123_U_95_Python_46_RTS_46_importModule1_125_(arg0):
  P_c0 = _idris_arg[1]
  return _idris_Python_46_RTS_46_importModule(P_c0, arg0)

def _idris__123_U_95_Python_46__123__36__58_0_125_1_125_(arg0):
  P_c0 = _idris_arg[1]
  return _idris_Python_46__123__36__58_0_125_(P_c0, arg0)

def _idris__123_U_95_Python_46__123__47__58_0_125_1_125_(arg0):
  P_c0 = _idris_arg[1]
  return _idris_Python_46__123__47__58_0_125_(P_c0, arg0)

def _idris__123_U_95_Python_46__123_toString0_125_1_125_(arg0):
  P_c0 = _idris_arg[1]
  return _idris_Python_46__123_toString0_125_(P_c0, arg0)

def U_believe_me1(arg0):
  P_c0, P_c1 = _idris_arg[1:]
  return _idris_believe_95_me(P_c0, P_c1, arg0)

def U_io_bind1(arg0):
  P_c0, P_c1, P_c2, P_c3, P_c4 = _idris_arg[1:]
  return _idris_io_95_bind(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)

def U_io_return1(arg0):
  P_c0, P_c1, P_c2 = _idris_arg[1:]
  return _idris_io_95_return(P_c0, P_c1, P_c2, arg0)

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac60_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac60_125_(
    arg0
  )

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac610_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac610_125_(
    arg0
  )

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac611_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac611_125_(
    arg0
  )

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac612_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac612_125_(
    arg0
  )

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac613_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac613_125_(
    arg0
  )

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac614_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac614_125_(
    arg0
  )

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac615_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac615_125_(
    arg0
  )

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac616_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac616_125_(
    arg0
  )

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac617_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac617_125_(
    arg0
  )

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac61_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac61_125_(
    arg0
  )

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac62_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac62_125_(
    arg0
  )

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac63_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac63_125_(
    arg0
  )

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac64_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac64_125_(
    arg0
  )

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac65_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac65_125_(
    arg0
  )

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac66_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac66_125_(
    arg0
  )

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac67_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac67_125_(
    arg0
  )

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac68_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac68_125_(
    arg0
  )

def _idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac69_125_1_125_(arg0):
  return _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac69_125_(
    arg0
  )

def _idris__123_U_95__123_Python_46_Lib_46_Numpy_46_array_44__32_mkList_95_lam0_125_1_125_(arg0):
  P_c0 = _idris_arg[1]
  return _idris__123_Python_46_Lib_46_Numpy_46_array_44__32_mkList_95_lam0_125_(
    P_c0, arg0
  )

def _idris__123_U_95__123_Python_46_Lib_46_Numpy_46_array_44__32_mkList_95_lam1_125_1_125_(arg0):
  return _idris__123_Python_46_Lib_46_Numpy_46_array_44__32_mkList_95_lam1_125_(arg0)

def _idris__123_U_95__123_io_95_bind1_125_1_125_(arg0):
  P_c0, P_c1, P_c2, P_c3, P_c4, P_c5 = _idris_arg[1:]
  return io_bind1(P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, arg0)

def _idris__123_U_95__123_unsafePerformIO0_125_1_125_(arg0):
  return unsafePerformIO0(arg0)

def _idris__123_U_95_Data_46_VectType_46_Vect_46_Vect_32_n_32_instance_32_of_32_Prelude_46_Functor_46_Functor2_125_(arg0):
  P_c0, P_c1, P_c2 = _idris_arg[1:]
  return (_idris__123_U_95_Data_46_VectType_46_Vect_46_Vect_32_n_32_instance_32_of_32_Prelude_46_Functor_46_Functor1_125_, P_c0, P_c1, P_c2, arg0,)

def _idris__123_U_95_Prelude_46_Nat_46_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Eq2_125_(arg0):
  return (_idris__123_U_95_Prelude_46_Nat_46_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Eq1_125_, arg0,)

def _idris__123_U_95_Data_46_VectType_46_Vect_46_Vect_32_n_32_instance_32_of_32_Prelude_46_Functor_46_Functor3_125_(arg0):
  P_c0, P_c1 = _idris_arg[1:]
  return (_idris__123_U_95_Data_46_VectType_46_Vect_46_Vect_32_n_32_instance_32_of_32_Prelude_46_Functor_46_Functor2_125_, P_c0, P_c1, arg0,)

def _idris__123_U_95_Data_46_VectType_46_Vect_46_Vect_32_n_32_instance_32_of_32_Prelude_46_Functor_46_Functor4_125_(arg0):
  P_c0 = _idris_arg[1]
  return (_idris__123_U_95_Data_46_VectType_46_Vect_46_Vect_32_n_32_instance_32_of_32_Prelude_46_Functor_46_Functor3_125_, P_c0, arg0,)

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
def _idris_Python_46_Lib_46_Numpy_46__123_array0_125_(e4, e3, in0):
  while True:
    return _idris_Python_46__36__58_(
      (_idris__123_U_95_Python_46__47__46_1_125_, in0, "array",),
      ConsList().cons(_idris_Python_46_toRef(None)(e3)).cons(_idris_Python_46_toRef(None)(
        _idris_Python_46_Lib_46_Numpy_46_array_58_mkList_58_0(
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          _idris_Prelude_46_Functor_46_Data_46_VectType_46_Vect_46__64_Prelude_46_Functor_46_Functor_36_Vect_32_n_58__33_map_58_0(
            None,
            None,
            None,
            None,
            (_idris__123_U_95_Python_46_Lib_46_Numpy_46_array_44__32_mkList1_125_, None, None, None, None, None, None, None,),
            e4
          )
        )
      ))
    )

# {io_bind0}
def io_bind0(e0, e1, e2, e3, e4, _idris_w, in0):
  while True:
    return e4(in0)

# Main.{main0}
def _idris_Main_46__123_main0_125_(in1):
  while True:
    return _idris_Prelude_46_Interactive_46_putStr(
      None,
      (_idris_Python_46_toString(_idris_Main_46_ys()) + "\n")
    )

# Prelude.Interactive.{putStr0}
def _idris_Prelude_46_Interactive_46__123_putStr0_125_(e1, in0):
  while True:
    return sys.stdout.write(e1)

# Python.Lib.Numpy.{reshape0}
def _idris_Python_46_Lib_46_Numpy_46__123_reshape0_125_(e6, e4, e5, in0):
  while True:
    return _idris_Python_46__36__58_(
      _idris_Python_46__47__58_(
        (_idris__123_U_95_Python_46__47__46_1_125_, in0, "ndarray",),
        "reshape"
      ),
      ConsList().cons(_idris_Python_46_toRef(None)(e5)).cons(_idris_Python_46_toRef(None)(e4)).cons(e6)
    )

# {runMain0}
def runMain0():
  while True:
    return EVAL0(_idris_Main_46_main()(None))

# Python.{toString0}
def _idris_Python_46__123_toString0_125_(e0, in0):
  while True:
    return str(e0)

# Python.Lib.Numpy.{unsafeNpArr0}
def _idris_Python_46_Lib_46_Numpy_46__123_unsafeNpArr0_125_(in0):
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
    return (_idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_1_125_, in0,)

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
    return (_idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_1_125_, in0,)

# {Python.Lib.Numpy.array, mkList_lam1}
def _idris__123_Python_46_Lib_46_Numpy_46_array_44__32_mkList_95_lam1_125_(in0):
  while True:
    return (_idris__123_U_95__123_Python_46_Lib_46_Numpy_46_array_44__32_mkList_95_lam0_125_1_125_, in0,)

# {io_bind1}
def io_bind1(e0, e1, e2, e3, e4, _idris_w, in0):
  while True:
    return io_bind0(e0, e1, e2, e3, e4, _idris_w, in0)(_idris_w)

# Main.{main1}
def _idris_Main_46__123_main1_125_(in0):
  while True:
    return (U_io_bind1, None, None, None, _idris_Prelude_46_Interactive_46_putStr(
      None,
      (_idris_Python_46_toString(_idris_Main_46_xs()) + "\n")
    ), (_idris__123_U_95_Main_46__123_main0_125_1_125_,),)

# Prelude.Interactive.{putStr1}
def _idris_Prelude_46_Interactive_46__123_putStr1_125_(in1):
  while True:
    return (U_io_return1, None, None, Unit,)

# {unsafePerformIO1}
def unsafePerformIO1(e0, e1, e2):
  while True:
    return (_idris__123_U_95__123_unsafePerformIO0_125_1_125_,)

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam2}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
  in2, in3
):
  while True:
    aux1 = _idris_Prelude_46_Classes_46_compare(
      None,
      _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat()
    )(in2)(in3)
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
    return (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac61_125_1_125_,)

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam2}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
  in2, in3
):
  while True:
    aux1 = _idris_Prelude_46_Classes_46_compare(
      None,
      _idris_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec()
    )(in2)(in3)
    if aux1[0] == 2:  # Prelude.Classes.GT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# {io_bind2}
def io_bind2(e0, e1, e2, e3, e4, _idris_w):
  while True:
    return (_idris__123_U_95__123_io_95_bind1_125_1_125_, e0, e1, e2, e3, e4, _idris_w,)

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam3}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
  in2
):
  while True:
    return (_idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_1_125_, in2,)

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac63}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac63_125_(
  in0
):
  while True:
    return ("[" + (_idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
      None,
      None,
      None,
      (
        0,  # constructor of Prelude.Show.Show
        (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac60_125_1_125_,),
        (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac62_125_1_125_,)
      ),
      "",
      in0
    ) + "]"))

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam3}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
  in2
):
  while True:
    return (_idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_1_125_, in2,)

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
    return (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac65_125_1_125_,)

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac67}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac67_125_(
  in5
):
  while True:
    return _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0(
      None,
      None,
      (
        0,  # constructor of Prelude.Show.Show
        (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac64_125_1_125_,),
        (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac66_125_1_125_,)
      ),
      in5
    )

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac68}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac68_125_(
  in4
):
  while True:
    return (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac67_125_1_125_,)

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
    return (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac610_125_1_125_,)

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac612}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac612_125_(
  in0
):
  while True:
    return ("[" + (_idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
      None,
      None,
      None,
      (
        0,  # constructor of Prelude.Show.Show
        (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac69_125_1_125_,),
        (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac611_125_1_125_,)
      ),
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
    return (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac614_125_1_125_,)

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac616}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac616_125_(
  in5
):
  while True:
    return _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0(
      None,
      None,
      (
        0,  # constructor of Prelude.Show.Show
        (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac613_125_1_125_,),
        (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac615_125_1_125_,)
      ),
      in5
    )

# {PE_Prelude.Show.Maybe a instance of Prelude.Show.Show, method show_bb6a4ac617}
def _idris__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac617_125_(
  in4
):
  while True:
    return (_idris__123_U_95__123_PE_95_Prelude_46_Show_46_Maybe_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_bb6a4ac616_125_1_125_,)

# Python.Lib.Numpy.array, mkList
def _idris_Python_46_Lib_46_Numpy_46_array_58_mkList_58_0(
  e0, e1, e2, e3, e4, e5, e6, e7
):
  while True:
    return _idris_Python_46_Builtins_46_toList(
      _idris_Python_46_toRef(None)(
        _idris_Data_46_VectType_46_Vect_46_foldrImpl(
          None,
          None,
          None,
          (_idris__123_U_95__123_Python_46_Lib_46_Numpy_46_array_44__32_mkList_95_lam1_125_1_125_,),
          ConsList(),
          (_idris__123_U_95_Prelude_46_Basics_46_id1_125_, None,),
          e7
        )
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
        return (e4 + _idris_Prelude_46_Show_46_show(None, e3)(in0))
      else:
        e0, e1, e2, e3, e4, e5, = None, None, None, e3, (e4 + (_idris_Prelude_46_Show_46_show(None, e3)(in0) + ", ")), in1,
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
        return _idris_Prelude_46_Classes_46__61__61_(
          None,
          (_idris__123_U_95_Prelude_46_Nat_46_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Eq2_125_,)
        )(
          in1
        )(
          in0
        )
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

# Prelude.Functor.Data.VectType.Vect.Vect n instance of Prelude.Functor.Functor, method map
def _idris_Prelude_46_Functor_46_Data_46_VectType_46_Vect_46__64_Prelude_46_Functor_46_Functor_36_Vect_32_n_58__33_map_58_0(
  e0, e1, e2, e3, e4, e5
):
  while True:
    if e5[0] == 1:  # Data.VectType.Vect.::
      in0, in1 = e5[1:]
      return (
        1,  # Data.VectType.Vect.::
        e4(in0),
        _idris_Prelude_46_Functor_46_map(
          None,
          None,
          None,
          (_idris__123_U_95_Data_46_VectType_46_Vect_46_Vect_32_n_32_instance_32_of_32_Prelude_46_Functor_46_Functor4_125_, None,)
        )(
          e4
        )(
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
        return _idris_Prelude_46_Classes_46_compare(
          None,
          _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat()
        )(in2)(in1)
      return _idris_error("unreachable due to case in tail position")
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.Prelude.Show.Prec instance of Prelude.Classes.Ord, method >=
def _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33__62__61__58_0(
  e0, e1
):
  while True:
    aux1 = _idris_Prelude_46_Classes_46__62_(
      None,
      _idris_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec()
    )(e0)(e1)
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

# Prelude.Show.Python.Lib.Numpy.Array m n ty instance of Prelude.Show.Show, method show
def _idris_Prelude_46_Show_46_Python_46_Lib_46_Numpy_46__64_Prelude_46_Show_46_Show_36_Array_32_m_32_n_32_ty_58__33_show_58_0(
  e0, e1, e2, e3, e4
):
  while True:
    return _idris_Python_46_toString(e4)

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
        return ("Just" + (" " + _idris_Prelude_46_Show_46_showPrec(None, e2)(None)(in0)))
      else:  # Prelude.Bool.True
        return ("(" + (("Just" + (" " + _idris_Prelude_46_Show_46_showPrec(None, e2)(None)(in0))) + ")"))
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
    return (
      0,  # constructor of Prelude.Classes.Ord
      (_idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_1_125_,),
      (_idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_1_125_,)
    )

# Prelude.Show.Prec instance of Prelude.Classes.Ord
def _idris_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec():
  while True:
    return (
      0,  # constructor of Prelude.Classes.Ord
      (_idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_1_125_,),
      (_idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_1_125_,)
    )

# case block in Void
def _idris_Void_95_case():
  while True:
    return None

# case block in io_bind
def _idris_io_95_bind_95_case(e0, e1, e2, e3, e4, e5, e6, e7):
  while True:
    return e7(e5)

# <<Void eliminator>>
def _idris_Void_95_elim():
  while True:
    return None

if __name__ == '__main__':
  runMain0()
