#!/usr/bin/env python

import sys

class IdrisError(Exception):
  pass

def idris_error(msg):
  raise IdrisError(msg)

MODULES = dict()

def idris_pymodule(name):
  mod = MODULES.get(name)
  if mod is None:
    mod = __import__(name)
    MODULES[name] = mod
  return mod

def idris_getfield(o, f):
  try:
    return o.__getattribute__(f)
  except AttributeError:
    # it's a module
    return o.__dict__[f]

def idris_call(f, args):
  native_args = []
  while len(args) == 3:  # it's a cons
    native_args.append(args[1])
    args = args[2]
  return f(*native_args)

def idris_foreach(it, st, f):
  for x in it:
    # Apply st, x, world
    st = APPLY0(APPLY0(APPLY0(f, st), x), None)
  return st

def idris_is_none(x):
  return 1 if x is None else 0

def idris_try(f, fail, succ):
  try:
    result = APPLY0(f, None)  # apply to world
    return APPLY0(succ, result)
  except Exception as e:
    return APPLY0(APPLY0(fail, e.__class__.__name__), e)

def idris_raise(e):
  raise e

# Python.$.
def idris_Python_46__36__46_(e0, e1, e2, idris_args):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65685, None),  # {U_Python.FFI.unRaw1}
      (65722, e2, idris_args)  # {U_Python.{$.0}1}
    )

# Python.$:
def idris_Python_46__36__58_(e0, e1, e2, idris_args):
  while True:
    return (65726, None, None, None, e2, (65723, idris_args))  # {U_io_bind1}, {U_Python.{$:0}1}

# Prelude.Basics..
def idris_Prelude_46_Basics_46__46_(e0, e1, e2, e3, e4, idris_x):
  while True:
    return APPLY0(e3, APPLY0(e4, idris_x))

# Python./.
def idris_Python_46__47__46_(e0, e1, e2, e3, e4):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65685, None),  # {U_Python.FFI.unRaw1}
      (65724, e2, e3)  # {U_Python.{/.0}1}
    )

# Python./:
def idris_Python_46__47__58_(e0, e1, e2, e3, e4):
  while True:
    return (65726, None, None, None, e2, (65725, e3))  # {U_io_bind1}, {U_Python.{/:0}1}

# Prelude.Algebra.<+>
def idris_Prelude_46_Algebra_46__60__43__62_(e0, e1):
  while True:
    return e1

# Python.>:
def idris_Python_46__62__58_(e0, e1, e2, e3):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65721, None, None, None),  # {U_Python.mixout1}
      e1
    )

# Prelude.Monad.>>=
def idris_Prelude_46_Monad_46__62__62__61_(e0, e1, e2, e3):
  while True:
    if e3[0] == 0:  # constructor of Prelude.Monad.Monad
      in0, in1, = e3[1:]
      aux1 = APPLY0(APPLY0(in1, e1), e2)
    else:
      idris_error("unreachable case")
    return aux1

# @@constructor of Prelude.Algebra.Monoid#Semigroup a
def idris__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a(
  e0, e1
):
  while True:
    if e1[0] == 0:  # constructor of Prelude.Algebra.Monoid
      in0, in1, = e1[1:]
      aux1 = in0
    else:
      idris_error("unreachable case")
    return aux1

# @@constructor of Prelude.Monad.Monad#Applicative m
def idris__64__64_constructor_32_of_32_Prelude_46_Monad_46_Monad_35_Applicative_32_m(
  e0, e1
):
  while True:
    if e1[0] == 0:  # constructor of Prelude.Monad.Monad
      in0, in1, = e1[1:]
      aux1 = in0
    else:
      idris_error("unreachable case")
    return aux1

# call__IO
def idris_call_95__95_IO(e0, e1, e2):
  while True:
    return APPLY0(e2, None)

# Python.Exceptions.catch
def idris_Python_46_Exceptions_46_catch(e0, e1, e2):
  while True:
    return (65726, None, None, None, e1, (65678, e2))  # {U_io_bind1}, {U_Python.Exceptions.{catch6}1}

# Python.Prim.collect
def idris_Python_46_Prim_46_collect(e0, e1):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65668, None, (0,)),  # {U_Prelude.List.reverse, reverse'1}, Prelude.List.Nil
      idris_Python_46_Prim_46_foreach(None, None, e1, (0,), (65689,))  # Prelude.List.Nil, {U_Python.Prim.{collect1}1}
    )

# Prelude.Foldable.concat
def idris_Prelude_46_Foldable_46_concat(e0, e1, e2, e3):
  while True:
    if e3[0] == 0:  # constructor of Prelude.Algebra.Monoid
      in0, in1, = e3[1:]
      aux1 = in0
    else:
      idris_error("unreachable case")
    if e3[0] == 0:  # constructor of Prelude.Algebra.Monoid
      in2, in3, = e3[1:]
      aux2 = in3
    else:
      idris_error("unreachable case")
    return APPLY0(
      APPLY0(idris_Prelude_46_Foldable_46_foldr(None, None, None, e2), aux1),
      aux2
    )

# Prelude.Foldable.foldr
def idris_Prelude_46_Foldable_46_foldr(e0, e1, e2, e3):
  while True:
    return APPLY0(APPLY0(e3, e1), e2)

# Python.Prim.foreach
def idris_Python_46_Prim_46_foreach(e0, e1, e2, e3, e4):
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46__36__58_(
        None,
        None,
        idris_Python_46__47__46_(None, None, e2, "__iter__", None),
        (0,)  # Python.Nil
      ),
      (65691, e2, e3, e4)  # {U_Python.Prim.{foreach1}1}
    )

# Python.importModule
def idris_Python_46_importModule(e0, e1, idris_w):
  while True:
    return idris_pymodule(e1)

# Python.Lib.BeautifulSoup.import_
def idris_Python_46_Lib_46_BeautifulSoup_46_import_95_():
  while True:
    return (65720, None, "bs4")  # {U_Python.importModule1}

# Python.Lib.Os.import_
def idris_Python_46_Lib_46_Os_46_import_95_():
  while True:
    return (65720, None, "os")  # {U_Python.importModule1}

# Python.Lib.Requests.import_
def idris_Python_46_Lib_46_Requests_46_import_95_():
  while True:
    return (65720, None, "requests")  # {U_Python.importModule1}

# io_bind
def idris_io_95_bind(e0, e1, e2, e3, e4, idris_w):
  while True:
    return APPLY0(io_bind2(e0, e1, e2, e3, e4, idris_w), APPLY0(e3, idris_w))

# io_return
def idris_io_95_return(e0, e1, e2, idris_w):
  while True:
    return e2

# Python.Prim.iter
def idris_Python_46_Prim_46_iter(e0, e1, e2, e3, e4):
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46_Prim_46_next(None, e2),
      (65696, e4, e3, e2)  # {U_Python.Prim.{iter13}1}
    )

# Python.Prim.iterate
def idris_Python_46_Prim_46_iterate(e0, e1, e2, e3, e4):
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46__36__58_(
        None,
        None,
        idris_Python_46__47__46_(None, None, e2, "__iter__", None),
        (0,)  # Python.Nil
      ),
      (65706, e3, e4)  # {U_Python.Prim.{iterate0}1}
    )

# Main.main
def idris_Main_46_main():
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46_Lib_46_Requests_46_import_95_(),
      (65656,)  # {U_Main.{main28}1}
    )

# Python.mixout
def idris_Python_46_mixout(e0, e1, e2, e3):
  while True:
    return e3

# mkForeignPrim
def idris_mkForeignPrim():
  while True:
    return None

# Prelude.Algebra.neutral
def idris_Prelude_46_Algebra_46_neutral(e0, e1):
  while True:
    if e1[0] == 0:  # constructor of Prelude.Algebra.Monoid
      in0, in1, = e1[1:]
      aux1 = in1
    else:
      idris_error("unreachable case")
    return aux1

# Python.Prim.next
def idris_Python_46_Prim_46_next(e0, e1):
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46_Exceptions_46_try(
        None,
        idris_Python_46__36__58_(
          None,
          None,
          idris_Python_46__47__46_(None, None, e1, "next", None),
          (0,)  # Python.Nil
        )
      ),
      (65710,)  # {U_Python.Prim.{next12}1}
    )

# prim__addInt
def idris_prim_95__95_addInt(op0, op1):
  while True:
    return op0 + op1

# prim__concat
def idris_prim_95__95_concat(op0, op1):
  while True:
    return op0 + op1

# prim__null
def idris_prim_95__95_null():
  while True:
    return None

# prim__readFile
def idris_prim_95__95_readFile(op0, op1):
  while True:
    return idris_error("unimplemented external: prim__readFile")

# prim__registerPtr
def idris_prim_95__95_registerPtr(op0, op1):
  while True:
    return idris_error("unimplemented external: prim__registerPtr")

# prim__stderr
def idris_prim_95__95_stderr():
  while True:
    return idris_error("unimplemented external: prim__stderr")

# prim__stdin
def idris_prim_95__95_stdin():
  while True:
    return idris_error("unimplemented external: prim__stdin")

# prim__stdout
def idris_prim_95__95_stdout():
  while True:
    return idris_error("unimplemented external: prim__stdout")

# prim__toStrInt
def idris_prim_95__95_toStrInt(op0):
  while True:
    return str(op0)

# prim__vm
def idris_prim_95__95_vm():
  while True:
    return idris_error("unimplemented external: prim__vm")

# prim__writeFile
def idris_prim_95__95_writeFile(op0, op1, op2):
  while True:
    return idris_error("unimplemented external: prim__writeFile")

# prim__writeString
def idris_prim_95__95_writeString(op0, op1):
  while True:
    return sys.stdout.write(op1)

# prim_io_bind
def idris_prim_95_io_95_bind(e0, e1, e2, e3):
  while True:
    return APPLY0(e3, e2)

# Prelude.Applicative.pure
def idris_Prelude_46_Applicative_46_pure(e0, e1, e2):
  while True:
    return APPLY0(e2, e1)

# Prelude.putStr
def idris_Prelude_46_putStr(e0, e1):
  while True:
    return (65726, None, None, None, (65669, e1), (65670,))  # {U_io_bind1}, {U_Prelude.{putStr0}1}, {U_Prelude.{putStr1}1}

# Python.Exceptions.raise
def idris_Python_46_Exceptions_46_raise(e0, e1):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65685, None),  # {U_Python.FFI.unRaw1}
      (65679, e1)  # {U_Python.Exceptions.{raise0}1}
    )

# run__IO
def idris_run_95__95_IO(e0, e1):
  while True:
    return APPLY0(e1, None)

# Python.Exceptions.showException
def idris_Python_46_Exceptions_46_showException(e0):
  while True:
    return idris_unsafePerformIO(None, None, (65680, e0))  # {U_Python.Exceptions.{showException0}1}

# Python.Exceptions.try
def idris_Python_46_Exceptions_46_try(e0, e1):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65685, None),  # {U_Python.FFI.unRaw1}
      (65684, e1)  # {U_Python.Exceptions.{try3}1}
    )

# Python.FFI.unRaw
def idris_Python_46_FFI_46_unRaw(e0, e1):
  while True:
    return e1

# unsafePerformIO
def idris_unsafePerformIO(e0, e1, e2):
  while True:
    return APPLY0(unsafePerformIO1(e0, e1, e2), APPLY0(e2, None))

# unsafePerformPrimIO
def idris_unsafePerformPrimIO():
  while True:
    return None

# world
def idris_world(e0):
  while True:
    return e0

# Python.{$.0}
def idris_Python_46__123__36__46_0_125_(e2, idris_args, in0):
  while True:
    return idris_call(e2, idris_args)

# Python.{$:0}
def idris_Python_46__123__36__58_0_125_(idris_args, in0):
  while True:
    return idris_Python_46__36__46_(None, None, in0, idris_args)

# Python.{/.0}
def idris_Python_46__123__47__46_0_125_(e2, e3, in0):
  while True:
    return idris_getfield(e2, e3)

# Python.{/:0}
def idris_Python_46__123__47__58_0_125_(e3, in0):
  while True:
    return idris_Python_46__47__46_(None, None, in0, e3, None)

# {APPLY0}
def APPLY0(fn0, arg0):
  while True:
    if fn0[0] < 65685:
      if fn0[0] < 65660:
        if fn0[0] < 65648:
          if fn0[0] < 65642:
            if fn0[0] < 65639:
              if fn0[0] == 65636:  # {U_Main.{main0}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = idris_Main_46__123_main0_125_(P_c0, P_c1, arg0)
              elif fn0[0] == 65637:  # {U_Main.{main10}1}
                P_c0, = fn0[1:]
                aux1 = idris_Main_46__123_main10_125_(P_c0, arg0)
              elif fn0[0] == 65638:  # {U_Main.{main11}1}
                aux1 = idris_Main_46__123_main11_125_(arg0)
            else:
              if fn0[0] == 65639:  # {U_Main.{main12}1}
                aux1 = idris_Main_46__123_main12_125_(arg0)
              elif fn0[0] == 65640:  # {U_Main.{main13}1}
                P_c0, = fn0[1:]
                aux1 = idris_Main_46__123_main13_125_(P_c0, arg0)
              elif fn0[0] == 65641:  # {U_Main.{main14}1}
                aux1 = idris_Main_46__123_main14_125_(arg0)
          else:
            if fn0[0] < 65645:
              if fn0[0] == 65642:  # {U_Main.{main15}1}
                aux1 = idris_Main_46__123_main15_125_(arg0)
              elif fn0[0] == 65643:  # {U_Main.{main16}1}
                P_c0, = fn0[1:]
                aux1 = idris_Main_46__123_main16_125_(P_c0, arg0)
              elif fn0[0] == 65644:  # {U_Main.{main17}1}
                P_c0, = fn0[1:]
                aux1 = idris_Main_46__123_main17_125_(P_c0, arg0)
            else:
              if fn0[0] == 65645:  # {U_Main.{main18}1}
                aux1 = idris_Main_46__123_main18_125_(arg0)
              elif fn0[0] == 65646:  # {U_Main.{main19}1}
                aux1 = idris_Main_46__123_main19_125_(arg0)
              elif fn0[0] == 65647:  # {U_Main.{main1}1}
                P_c0, = fn0[1:]
                aux1 = idris_Main_46__123_main1_125_(P_c0, arg0)
        else:
          if fn0[0] < 65654:
            if fn0[0] < 65651:
              if fn0[0] == 65648:  # {U_Main.{main20}1}
                aux1 = idris_Main_46__123_main20_125_(arg0)
              elif fn0[0] == 65649:  # {U_Main.{main21}1}
                aux1 = idris_Main_46__123_main21_125_(arg0)
              elif fn0[0] == 65650:  # {U_Main.{main22}1}
                P_c0, = fn0[1:]
                aux1 = idris_Main_46__123_main22_125_(P_c0, arg0)
            else:
              if fn0[0] == 65651:  # {U_Main.{main23}1}
                aux1 = idris_Main_46__123_main23_125_(arg0)
              elif fn0[0] == 65652:  # {U_Main.{main24}1}
                aux1 = idris_Main_46__123_main24_125_(arg0)
              elif fn0[0] == 65653:  # {U_Main.{main25}1}
                P_c0, = fn0[1:]
                aux1 = idris_Main_46__123_main25_125_(P_c0, arg0)
          else:
            if fn0[0] < 65657:
              if fn0[0] == 65654:  # {U_Main.{main26}1}
                aux1 = idris_Main_46__123_main26_125_(arg0)
              elif fn0[0] == 65655:  # {U_Main.{main27}1}
                aux1 = idris_Main_46__123_main27_125_(arg0)
              elif fn0[0] == 65656:  # {U_Main.{main28}1}
                aux1 = idris_Main_46__123_main28_125_(arg0)
            else:
              if fn0[0] == 65657:  # {U_Main.{main2}1}
                aux1 = idris_Main_46__123_main2_125_(arg0)
              elif fn0[0] == 65658:  # {U_Main.{main3}1}
                aux1 = idris_Main_46__123_main3_125_(arg0)
              elif fn0[0] == 65659:  # {U_Main.{main4}1}
                aux1 = idris_Main_46__123_main4_125_(arg0)
      else:
        if fn0[0] < 65672:
          if fn0[0] < 65666:
            if fn0[0] < 65663:
              if fn0[0] == 65660:  # {U_Main.{main5}1}
                P_c0, = fn0[1:]
                aux1 = idris_Main_46__123_main5_125_(P_c0, arg0)
              elif fn0[0] == 65661:  # {U_Main.{main6}1}
                aux1 = idris_Main_46__123_main6_125_(arg0)
              elif fn0[0] == 65662:  # {U_Main.{main7}1}
                aux1 = idris_Main_46__123_main7_125_(arg0)
            else:
              if fn0[0] == 65663:  # {U_Main.{main8}1}
                P_c0, = fn0[1:]
                aux1 = idris_Main_46__123_main8_125_(P_c0, arg0)
              elif fn0[0] == 65664:  # {U_Main.{main9}1}
                P_c0, = fn0[1:]
                aux1 = idris_Main_46__123_main9_125_(P_c0, arg0)
              elif fn0[0] == 65665:  # {U_Prelude.Basics..1}
                P_c0, P_c1, P_c2, P_c3, P_c4, = fn0[1:]
                aux1 = idris_Prelude_46_Basics_46__46_(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
          else:
            if fn0[0] < 65669:
              if fn0[0] == 65666:  # {U_Prelude.Functor.{Prelude.IO' ffi instance of Prelude.Functor.Functor, method map_lam0}1}
                P_c0, = fn0[1:]
                aux1 = idris_Prelude_46_Functor_46__123_Prelude_46_IO_39__32_ffi_32_instance_32_of_32_Prelude_46_Functor_46_Functor_44__32_method_32_map_95_lam0_125_(
                  P_c0, arg0
                )
              elif fn0[0] == 65667:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable1}
                P_c0, P_c1, P_c2, P_c3, = fn0[1:]
                aux1 = idris_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List(
                  P_c0, P_c1, P_c2, P_c3, arg0
                )
              elif fn0[0] == 65668:  # {U_Prelude.List.reverse, reverse'1}
                P_c0, P_c1, = fn0[1:]
                aux1 = idris_Prelude_46_List_46_reverse_58_reverse_39__58_0(P_c0, P_c1, arg0)
            else:
              if fn0[0] == 65669:  # {U_Prelude.{putStr0}1}
                P_c0, = fn0[1:]
                aux1 = idris_Prelude_46__123_putStr0_125_(P_c0, arg0)
              elif fn0[0] == 65670:  # {U_Prelude.{putStr1}1}
                aux1 = idris_Prelude_46__123_putStr1_125_(arg0)
              elif fn0[0] == 65671:  # {U_Python.Exceptions.case block in try1}
                P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, P_c6, = fn0[1:]
                aux1 = idris_Python_46_Exceptions_46_try_95_case(
                  P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, P_c6, arg0
                )
        else:
          if fn0[0] < 65678:
            if fn0[0] < 65675:
              if fn0[0] == 65672:  # {U_Python.Exceptions.{catch0}1}
                aux1 = idris_Python_46_Exceptions_46__123_catch0_125_(arg0)
              elif fn0[0] == 65673:  # {U_Python.Exceptions.{catch1}1}
                aux1 = idris_Python_46_Exceptions_46__123_catch1_125_(arg0)
              elif fn0[0] == 65674:  # {U_Python.Exceptions.{catch2}1}
                P_c0, = fn0[1:]
                aux1 = idris_Python_46_Exceptions_46__123_catch2_125_(P_c0, arg0)
            else:
              if fn0[0] == 65675:  # {U_Python.Exceptions.{catch3}1}
                aux1 = idris_Python_46_Exceptions_46__123_catch3_125_(arg0)
              elif fn0[0] == 65676:  # {U_Python.Exceptions.{catch4}1}
                aux1 = idris_Python_46_Exceptions_46__123_catch4_125_(arg0)
              elif fn0[0] == 65677:  # {U_Python.Exceptions.{catch5}1}
                aux1 = idris_Python_46_Exceptions_46__123_catch5_125_(arg0)
          else:
            if fn0[0] < 65681:
              if fn0[0] == 65678:  # {U_Python.Exceptions.{catch6}1}
                P_c0, = fn0[1:]
                aux1 = idris_Python_46_Exceptions_46__123_catch6_125_(P_c0, arg0)
              elif fn0[0] == 65679:  # {U_Python.Exceptions.{raise0}1}
                P_c0, = fn0[1:]
                aux1 = idris_Python_46_Exceptions_46__123_raise0_125_(P_c0, arg0)
              elif fn0[0] == 65680:  # {U_Python.Exceptions.{showException0}1}
                P_c0, = fn0[1:]
                aux1 = idris_Python_46_Exceptions_46__123_showException0_125_(P_c0, arg0)
            else:
              if fn0[0] < 65683:
                if fn0[0] == 65681:  # {U_Python.Exceptions.{try0}1}
                  aux1 = idris_Python_46_Exceptions_46__123_try0_125_(arg0)
                elif fn0[0] == 65682:  # {U_Python.Exceptions.{try1}1}
                  aux1 = idris_Python_46_Exceptions_46__123_try1_125_(arg0)
              else:
                if fn0[0] == 65683:  # {U_Python.Exceptions.{try2}1}
                  aux1 = idris_Python_46_Exceptions_46__123_try2_125_(arg0)
                elif fn0[0] == 65684:  # {U_Python.Exceptions.{try3}1}
                  P_c0, = fn0[1:]
                  aux1 = idris_Python_46_Exceptions_46__123_try3_125_(P_c0, arg0)
    else:
      if fn0[0] < 65710:
        if fn0[0] < 65697:
          if fn0[0] < 65691:
            if fn0[0] < 65688:
              if fn0[0] == 65685:  # {U_Python.FFI.unRaw1}
                P_c0, = fn0[1:]
                aux1 = idris_Python_46_FFI_46_unRaw(P_c0, arg0)
              elif fn0[0] == 65686:  # {U_Python.Prim.collect1}
                P_c0, = fn0[1:]
                aux1 = idris_Python_46_Prim_46_collect(P_c0, arg0)
              elif fn0[0] == 65687:  # {U_Python.Prim.{case block in iter_lam0}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = idris_Python_46_Prim_46__123_case_32_block_32_in_32_iter_95_lam0_125_(
                  P_c0, P_c1, arg0
                )
            else:
              if fn0[0] == 65688:  # {U_Python.Prim.{collect0}1}
                P_c0, = fn0[1:]
                aux1 = idris_Python_46_Prim_46__123_collect0_125_(P_c0, arg0)
              elif fn0[0] == 65689:  # {U_Python.Prim.{collect1}1}
                aux1 = idris_Python_46_Prim_46__123_collect1_125_(arg0)
              elif fn0[0] == 65690:  # {U_Python.Prim.{foreach0}1}
                P_c0, P_c1, P_c2, = fn0[1:]
                aux1 = idris_Python_46_Prim_46__123_foreach0_125_(P_c0, P_c1, P_c2, arg0)
          else:
            if fn0[0] < 65694:
              if fn0[0] == 65691:  # {U_Python.Prim.{foreach1}1}
                P_c0, P_c1, P_c2, = fn0[1:]
                aux1 = idris_Python_46_Prim_46__123_foreach1_125_(P_c0, P_c1, P_c2, arg0)
              elif fn0[0] == 65692:  # {U_Python.Prim.{iter0}1}
                aux1 = idris_Python_46_Prim_46__123_iter0_125_(arg0)
              elif fn0[0] == 65693:  # {U_Python.Prim.{iter10}1}
                aux1 = idris_Python_46_Prim_46__123_iter10_125_(arg0)
            else:
              if fn0[0] == 65694:  # {U_Python.Prim.{iter11}1}
                aux1 = idris_Python_46_Prim_46__123_iter11_125_(arg0)
              elif fn0[0] == 65695:  # {U_Python.Prim.{iter12}1}
                aux1 = idris_Python_46_Prim_46__123_iter12_125_(arg0)
              elif fn0[0] == 65696:  # {U_Python.Prim.{iter13}1}
                P_c0, P_c1, P_c2, = fn0[1:]
                aux1 = idris_Python_46_Prim_46__123_iter13_125_(P_c0, P_c1, P_c2, arg0)
        else:
          if fn0[0] < 65703:
            if fn0[0] < 65700:
              if fn0[0] == 65697:  # {U_Python.Prim.{iter1}1}
                aux1 = idris_Python_46_Prim_46__123_iter1_125_(arg0)
              elif fn0[0] == 65698:  # {U_Python.Prim.{iter2}1}
                P_c0, = fn0[1:]
                aux1 = idris_Python_46_Prim_46__123_iter2_125_(P_c0, arg0)
              elif fn0[0] == 65699:  # {U_Python.Prim.{iter3}1}
                aux1 = idris_Python_46_Prim_46__123_iter3_125_(arg0)
            else:
              if fn0[0] == 65700:  # {U_Python.Prim.{iter4}1}
                aux1 = idris_Python_46_Prim_46__123_iter4_125_(arg0)
              elif fn0[0] == 65701:  # {U_Python.Prim.{iter5}1}
                aux1 = idris_Python_46_Prim_46__123_iter5_125_(arg0)
              elif fn0[0] == 65702:  # {U_Python.Prim.{iter6}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = idris_Python_46_Prim_46__123_iter6_125_(P_c0, P_c1, arg0)
          else:
            if fn0[0] < 65706:
              if fn0[0] == 65703:  # {U_Python.Prim.{iter7}1}
                aux1 = idris_Python_46_Prim_46__123_iter7_125_(arg0)
              elif fn0[0] == 65704:  # {U_Python.Prim.{iter8}1}
                aux1 = idris_Python_46_Prim_46__123_iter8_125_(arg0)
              elif fn0[0] == 65705:  # {U_Python.Prim.{iter9}1}
                P_c0, = fn0[1:]
                aux1 = idris_Python_46_Prim_46__123_iter9_125_(P_c0, arg0)
            else:
              if fn0[0] < 65708:
                if fn0[0] == 65706:  # {U_Python.Prim.{iterate0}1}
                  P_c0, P_c1, = fn0[1:]
                  aux1 = idris_Python_46_Prim_46__123_iterate0_125_(P_c0, P_c1, arg0)
                elif fn0[0] == 65707:  # {U_Python.Prim.{next0}1}
                  aux1 = idris_Python_46_Prim_46__123_next0_125_(arg0)
              else:
                if fn0[0] == 65708:  # {U_Python.Prim.{next10}1}
                  aux1 = idris_Python_46_Prim_46__123_next10_125_(arg0)
                elif fn0[0] == 65709:  # {U_Python.Prim.{next11}1}
                  aux1 = idris_Python_46_Prim_46__123_next11_125_(arg0)
      else:
        if fn0[0] < 65722:
          if fn0[0] < 65716:
            if fn0[0] < 65713:
              if fn0[0] == 65710:  # {U_Python.Prim.{next12}1}
                aux1 = idris_Python_46_Prim_46__123_next12_125_(arg0)
              elif fn0[0] == 65711:  # {U_Python.Prim.{next1}1}
                aux1 = idris_Python_46_Prim_46__123_next1_125_(arg0)
              elif fn0[0] == 65712:  # {U_Python.Prim.{next2}1}
                P_c0, = fn0[1:]
                aux1 = idris_Python_46_Prim_46__123_next2_125_(P_c0, arg0)
            else:
              if fn0[0] == 65713:  # {U_Python.Prim.{next3}1}
                aux1 = idris_Python_46_Prim_46__123_next3_125_(arg0)
              elif fn0[0] == 65714:  # {U_Python.Prim.{next4}1}
                aux1 = idris_Python_46_Prim_46__123_next4_125_(arg0)
              elif fn0[0] == 65715:  # {U_Python.Prim.{next5}1}
                aux1 = idris_Python_46_Prim_46__123_next5_125_(arg0)
          else:
            if fn0[0] < 65719:
              if fn0[0] == 65716:  # {U_Python.Prim.{next6}1}
                aux1 = idris_Python_46_Prim_46__123_next6_125_(arg0)
              elif fn0[0] == 65717:  # {U_Python.Prim.{next7}1}
                aux1 = idris_Python_46_Prim_46__123_next7_125_(arg0)
              elif fn0[0] == 65718:  # {U_Python.Prim.{next8}1}
                P_c0, = fn0[1:]
                aux1 = idris_Python_46_Prim_46__123_next8_125_(P_c0, arg0)
            else:
              if fn0[0] == 65719:  # {U_Python.Prim.{next9}1}
                aux1 = idris_Python_46_Prim_46__123_next9_125_(arg0)
              elif fn0[0] == 65720:  # {U_Python.importModule1}
                P_c0, P_c1, = fn0[1:]
                aux1 = idris_Python_46_importModule(P_c0, P_c1, arg0)
              elif fn0[0] == 65721:  # {U_Python.mixout1}
                P_c0, P_c1, P_c2, = fn0[1:]
                aux1 = idris_Python_46_mixout(P_c0, P_c1, P_c2, arg0)
        else:
          if fn0[0] < 65728:
            if fn0[0] < 65725:
              if fn0[0] == 65722:  # {U_Python.{$.0}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = idris_Python_46__123__36__46_0_125_(P_c0, P_c1, arg0)
              elif fn0[0] == 65723:  # {U_Python.{$:0}1}
                P_c0, = fn0[1:]
                aux1 = idris_Python_46__123__36__58_0_125_(P_c0, arg0)
              elif fn0[0] == 65724:  # {U_Python.{/.0}1}
                P_c0, P_c1, = fn0[1:]
                aux1 = idris_Python_46__123__47__46_0_125_(P_c0, P_c1, arg0)
            else:
              if fn0[0] == 65725:  # {U_Python.{/:0}1}
                P_c0, = fn0[1:]
                aux1 = idris_Python_46__123__47__58_0_125_(P_c0, arg0)
              elif fn0[0] == 65726:  # {U_io_bind1}
                P_c0, P_c1, P_c2, P_c3, P_c4, = fn0[1:]
                aux1 = idris_io_95_bind(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
              elif fn0[0] == 65727:  # {U_io_return1}
                P_c0, P_c1, P_c2, = fn0[1:]
                aux1 = idris_io_95_return(P_c0, P_c1, P_c2, arg0)
          else:
            if fn0[0] < 65731:
              if fn0[0] == 65728:  # {U_{io_bind1}1}
                P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, = fn0[1:]
                aux1 = io_bind1(P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, arg0)
              elif fn0[0] == 65729:  # {U_{unsafePerformIO0}1}
                aux1 = unsafePerformIO0(arg0)
              elif fn0[0] == 65730:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable2}
                P_c0, P_c1, P_c2, = fn0[1:]
                aux1 = (65667, P_c0, P_c1, P_c2, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable1}
            else:
              if fn0[0] < 65733:
                if fn0[0] == 65731:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable3}
                  P_c0, P_c1, = fn0[1:]
                  aux1 = (65730, P_c0, P_c1, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable2}
                elif fn0[0] == 65732:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable4}
                  P_c0, = fn0[1:]
                  aux1 = (65731, P_c0, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable3}
              else:
                if fn0[0] == 65733:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable5}
                  aux1 = (65732, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable4}
    return aux1

# {EVAL0}
def EVAL0(arg0):
  while True:
    return arg0

# Prelude.Functor.{Prelude.IO' ffi instance of Prelude.Functor.Functor, method map_lam0}
def idris_Prelude_46_Functor_46__123_Prelude_46_IO_39__32_ffi_32_instance_32_of_32_Prelude_46_Functor_46_Functor_44__32_method_32_map_95_lam0_125_(
  e3, in0
):
  while True:
    return (65727, None, None, APPLY0(e3, in0))  # {U_io_return1}

# Python.Prim.{case block in iter_lam0}
def idris_Python_46_Prim_46__123_case_32_block_32_in_32_iter_95_lam0_125_(
  e1, e4, in1
):
  while True:
    return idris_Python_46_Prim_46_iter(None, None, e1, in1, e4)

# Python.Exceptions.{catch0}
def idris_Python_46_Exceptions_46__123_catch0_125_(in2):
  while True:
    return (65727, None, None, in2)  # {U_io_return1}

# Python.Prim.{collect0}
def idris_Python_46_Prim_46__123_collect0_125_(in0, in1):
  while True:
    return (65727, None, None, (1, in1, in0))  # {U_io_return1}, Prelude.List.::

# Python.Prim.{foreach0}
def idris_Python_46_Prim_46__123_foreach0_125_(e2, e3, e4, in1):
  while True:
    return idris_foreach(e2, e3, e4)

# {io_bind0}
def io_bind0(e0, e1, e2, e3, e4, idris_w, in0):
  while True:
    return APPLY0(e4, in0)

# Python.Prim.{iter0}
def idris_Python_46_Prim_46__123_iter0_125_(in2):
  while True:
    return (65727, None, None, in2)  # {U_io_return1}

# Python.Prim.{iterate0}
def idris_Python_46_Prim_46__123_iterate0_125_(e3, e4, in0):
  while True:
    return idris_Python_46_Prim_46_iter(None, None, in0, e3, e4)

# Main.{main0}
def idris_Main_46__123_main0_125_(in12, in13, in14):
  while True:
    return idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
      None, None, in12, in13, in14
    )

# Python.Prim.{next0}
def idris_Python_46_Prim_46__123_next0_125_(in2):
  while True:
    return (65727, None, None, in2)  # {U_io_return1}

# Prelude.{putStr0}
def idris_Prelude_46__123_putStr0_125_(e1, in0):
  while True:
    return sys.stdout.write(e1)

# Python.Exceptions.{raise0}
def idris_Python_46_Exceptions_46__123_raise0_125_(e1, in0):
  while True:
    return idris_raise(e1)

# {runMain0}
def runMain0():
  while True:
    return EVAL0(APPLY0(idris_Main_46_main(), None))

# Python.Exceptions.{showException0}
def idris_Python_46_Exceptions_46__123_showException0_125_(e0, in0):
  while True:
    return str(e0)

# Python.Exceptions.{try0}
def idris_Python_46_Exceptions_46__123_try0_125_(in1):
  while True:
    return (65671, None, None, None, None, None, in1, None)  # {U_Python.Exceptions.case block in try1}

# {unsafePerformIO0}
def unsafePerformIO0(in0):
  while True:
    return in0

# Python.Exceptions.{catch1}
def idris_Python_46_Exceptions_46__123_catch1_125_(in1):
  while True:
    return (65672,)  # {U_Python.Exceptions.{catch0}1}

# Python.Prim.{collect1}
def idris_Python_46_Prim_46__123_collect1_125_(in0):
  while True:
    return (65688, in0)  # {U_Python.Prim.{collect0}1}

# Python.Prim.{foreach1}
def idris_Python_46_Prim_46__123_foreach1_125_(e2, e3, e4, in0):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65685, None),  # {U_Python.FFI.unRaw1}
      (65690, e2, e3, e4)  # {U_Python.Prim.{foreach0}1}
    )

# {io_bind1}
def io_bind1(e0, e1, e2, e3, e4, idris_w, in0):
  while True:
    return APPLY0(io_bind0(e0, e1, e2, e3, e4, idris_w, in0), idris_w)

# Python.Prim.{iter1}
def idris_Python_46_Prim_46__123_iter1_125_(in1):
  while True:
    return (65692,)  # {U_Python.Prim.{iter0}1}

# Main.{main1}
def idris_Main_46__123_main1_125_(in12, in13):
  while True:
    return (65636, in12, in13)  # {U_Main.{main0}1}

# Python.Prim.{next1}
def idris_Python_46_Prim_46__123_next1_125_(in1):
  while True:
    return (65707,)  # {U_Python.Prim.{next0}1}

# Prelude.{putStr1}
def idris_Prelude_46__123_putStr1_125_(in1):
  while True:
    return (65727, None, None, (0,))  # {U_io_return1}, MkUnit

# Python.Exceptions.{try1}
def idris_Python_46_Exceptions_46__123_try1_125_(in2):
  while True:
    return in2

# {unsafePerformIO1}
def unsafePerformIO1(e0, e1, e2):
  while True:
    return (65729,)  # {U_{unsafePerformIO0}1}

# Python.Exceptions.{catch2}
def idris_Python_46_Exceptions_46__123_catch2_125_(in5, in6):
  while True:
    return (65726, None, None, None, in5, in6)  # {U_io_bind1}

# {io_bind2}
def io_bind2(e0, e1, e2, e3, e4, idris_w):
  while True:
    return (65728, e0, e1, e2, e3, e4, idris_w)  # {U_{io_bind1}1}

# Python.Prim.{iter2}
def idris_Python_46_Prim_46__123_iter2_125_(in5, in6):
  while True:
    return (65726, None, None, None, in5, in6)  # {U_io_bind1}

# Main.{main2}
def idris_Main_46__123_main2_125_(in12):
  while True:
    return (65647, in12)  # {U_Main.{main1}1}

# Python.Prim.{next2}
def idris_Python_46_Prim_46__123_next2_125_(in5, in6):
  while True:
    return (65726, None, None, None, in5, in6)  # {U_io_bind1}

# Python.Exceptions.{try2}
def idris_Python_46_Exceptions_46__123_try2_125_(in3):
  while True:
    return (0, in3)  # Python.Exceptions.OK

# Python.Exceptions.{catch3}
def idris_Python_46_Exceptions_46__123_catch3_125_(in5):
  while True:
    return (65674, in5)  # {U_Python.Exceptions.{catch2}1}

# Python.Prim.{iter3}
def idris_Python_46_Prim_46__123_iter3_125_(in5):
  while True:
    return (65698, in5)  # {U_Python.Prim.{iter2}1}

# Main.{main3}
def idris_Main_46__123_main3_125_(in11):
  while True:
    return (65657,)  # {U_Main.{main2}1}

# Python.Prim.{next3}
def idris_Python_46_Prim_46__123_next3_125_(in5):
  while True:
    return (65712, in5)  # {U_Python.Prim.{next2}1}

# Python.Exceptions.{try3}
def idris_Python_46_Exceptions_46__123_try3_125_(e1, in0):
  while True:
    return idris_try(
      e1,
      (65681,),  # {U_Python.Exceptions.{try0}1}
      (
        65665,  # {U_Prelude.Basics..1}
        None,
        None,
        None,
        (65665, None, None, None, (65682,), (65683,)),  # {U_Prelude.Basics..1}, {U_Python.Exceptions.{try1}1}, {U_Python.Exceptions.{try2}1}
        (65685, None)  # {U_Python.FFI.unRaw1}
      )
    )

# Python.Exceptions.{catch4}
def idris_Python_46_Exceptions_46__123_catch4_125_(in4):
  while True:
    return (65675,)  # {U_Python.Exceptions.{catch3}1}

# Python.Prim.{iter4}
def idris_Python_46_Prim_46__123_iter4_125_(in4):
  while True:
    return (65699,)  # {U_Python.Prim.{iter3}1}

# Main.{main4}
def idris_Main_46__123_main4_125_(in10):
  while True:
    return (65658,)  # {U_Main.{main3}1}

# Python.Prim.{next4}
def idris_Python_46_Prim_46__123_next4_125_(in4):
  while True:
    return (65713,)  # {U_Python.Prim.{next3}1}

# Python.Exceptions.{catch5}
def idris_Python_46_Exceptions_46__123_catch5_125_(in3):
  while True:
    return (65676,)  # {U_Python.Exceptions.{catch4}1}

# Python.Prim.{iter5}
def idris_Python_46_Prim_46__123_iter5_125_(in3):
  while True:
    return (65700,)  # {U_Python.Prim.{iter4}1}

# Main.{main5}
def idris_Main_46__123_main5_125_(in15, in16):
  while True:
    return in15 + in16

# Python.Prim.{next5}
def idris_Python_46_Prim_46__123_next5_125_(in3):
  while True:
    return (65714,)  # {U_Python.Prim.{next4}1}

# Python.Exceptions.{catch6}
def idris_Python_46_Exceptions_46__123_catch6_125_(e2, in0):
  while True:
    if in0[0] == 1:  # Python.Exceptions.Except
      in7, in8, = in0[1:]
      aux1 = APPLY0(APPLY0(e2, in7), in8)
    elif in0[0] == 0:  # Python.Exceptions.OK
      in9, = in0[1:]
      aux2 = (0, (65673,), (65677,))  # constructor of Prelude.Monad.Monad, {U_Python.Exceptions.{catch1}1}, {U_Python.Exceptions.{catch5}1}
      if aux2[0] == 0:  # constructor of Prelude.Monad.Monad
        in10, in11, = aux2[1:]
        aux3 = in10
      else:
        idris_error("unreachable case")
      aux1 = APPLY0(idris_Prelude_46_Applicative_46_pure(None, None, aux3), in9)
    else:
      idris_error("unreachable case")
    return aux1

# Python.Prim.{iter6}
def idris_Python_46_Prim_46__123_iter6_125_(e2, e4, in8):
  while True:
    return idris_Python_46_Prim_46_iter(None, None, e2, in8, e4)

# Main.{main6}
def idris_Main_46__123_main6_125_(in15):
  while True:
    return (65660, in15)  # {U_Main.{main5}1}

# Python.Prim.{next6}
def idris_Python_46_Prim_46__123_next6_125_(in2):
  while True:
    return (65727, None, None, in2)  # {U_io_return1}

# Python.Prim.{iter7}
def idris_Python_46_Prim_46__123_iter7_125_(in2):
  while True:
    return (65727, None, None, in2)  # {U_io_return1}

# Main.{main7}
def idris_Main_46__123_main7_125_(in9):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      idris_Prelude_46_Foldable_46_concat(None, None, (65659,), (0, (65661,), "")),  # {U_Main.{main4}1}, constructor of Prelude.Algebra.Monoid, {U_Main.{main6}1}
      in9
    )

# Python.Prim.{next7}
def idris_Python_46_Prim_46__123_next7_125_(in1):
  while True:
    return (65716,)  # {U_Python.Prim.{next6}1}

# Python.Prim.{iter8}
def idris_Python_46_Prim_46__123_iter8_125_(in1):
  while True:
    return (65703,)  # {U_Python.Prim.{iter7}1}

# Main.{main8}
def idris_Main_46__123_main8_125_(in7, in18):
  while True:
    return (65727, None, None, in7 + 1)  # {U_io_return1}

# Python.Prim.{next8}
def idris_Python_46_Prim_46__123_next8_125_(in5, in6):
  while True:
    return (65726, None, None, None, in5, in6)  # {U_io_bind1}

# Python.Prim.{iter9}
def idris_Python_46_Prim_46__123_iter9_125_(in5, in6):
  while True:
    return (65726, None, None, None, in5, in6)  # {U_io_bind1}

# Main.{main9}
def idris_Main_46__123_main9_125_(in7, in17):
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Prelude_46_putStr(None, str(in7 + 1) + ". " + in17 + "\n"),
      (65663, in7)  # {U_Main.{main8}1}
    )

# Python.Prim.{next9}
def idris_Python_46_Prim_46__123_next9_125_(in5):
  while True:
    return (65718, in5)  # {U_Python.Prim.{next8}1}

# Python.Prim.{iter10}
def idris_Python_46_Prim_46__123_iter10_125_(in5):
  while True:
    return (65705, in5)  # {U_Python.Prim.{iter9}1}

# Main.{main10}
def idris_Main_46__123_main10_125_(in7, in8):
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      (
        65726,  # {U_io_bind1}
        None,
        None,
        None,
        idris_Python_46__62__58_(
          None,
          idris_Python_46__47__46_(None, None, in8, "strings", None),
          None,
          None
        ),
        (65665, None, None, None, (65662,), (65686, None))  # {U_Prelude.Basics..1}, {U_Main.{main7}1}, {U_Python.Prim.collect1}
      ),
      (65664, in7)  # {U_Main.{main9}1}
    )

# Python.Prim.{next10}
def idris_Python_46_Prim_46__123_next10_125_(in4):
  while True:
    return (65719,)  # {U_Python.Prim.{next9}1}

# Python.Prim.{iter11}
def idris_Python_46_Prim_46__123_iter11_125_(in4):
  while True:
    return (65693,)  # {U_Python.Prim.{iter10}1}

# Main.{main11}
def idris_Main_46__123_main11_125_(in7):
  while True:
    return (65637, in7)  # {U_Main.{main10}1}

# Python.Prim.{next11}
def idris_Python_46_Prim_46__123_next11_125_(in3):
  while True:
    return (65708,)  # {U_Python.Prim.{next10}1}

# Python.Prim.{iter12}
def idris_Python_46_Prim_46__123_iter12_125_(in3):
  while True:
    return (65694,)  # {U_Python.Prim.{iter11}1}

# Main.{main12}
def idris_Main_46__123_main12_125_(in24):
  while True:
    return idris_Prelude_46_putStr(
      None,
      "Something's wrong, your root's homedir is writable!\n"
    )

# Python.Prim.{next12}
def idris_Python_46_Prim_46__123_next12_125_(in0):
  while True:
    if in0[0] == 1:  # Python.Exceptions.Except
      in7, in8, = in0[1:]
      if in7[0] == 0:  # Python.Exceptions.StopIteration
        aux3 = (0, (65711,), (65715,))  # constructor of Prelude.Monad.Monad, {U_Python.Prim.{next1}1}, {U_Python.Prim.{next5}1}
        if aux3[0] == 0:  # constructor of Prelude.Monad.Monad
          in9, in10, = aux3[1:]
          aux4 = in9
        else:
          idris_error("unreachable case")
        aux2 = APPLY0(idris_Prelude_46_Applicative_46_pure(None, None, aux4), (0,))  # Prelude.Maybe.Nothing
      else:
        aux2 = idris_Python_46_Exceptions_46_raise(None, in8)
      aux1 = aux2
    elif in0[0] == 0:  # Python.Exceptions.OK
      in11, = in0[1:]
      aux5 = (0, (65717,), (65709,))  # constructor of Prelude.Monad.Monad, {U_Python.Prim.{next7}1}, {U_Python.Prim.{next11}1}
      if aux5[0] == 0:  # constructor of Prelude.Monad.Monad
        in12, in13, = aux5[1:]
        aux6 = in12
      else:
        idris_error("unreachable case")
      aux1 = APPLY0(idris_Prelude_46_Applicative_46_pure(None, None, aux6), (1, in11))  # Prelude.Maybe.Just
    else:
      idris_error("unreachable case")
    return aux1

# Python.Prim.{iter13}
def idris_Python_46_Prim_46__123_iter13_125_(e4, e3, e2, in0):
  while True:
    if in0[0] == 1:  # Prelude.Maybe.Just
      in7, = in0[1:]
      aux1 = APPLY0(
        APPLY0(
          idris_Prelude_46_Monad_46__62__62__61_(
            None,
            None,
            None,
            (0, (65697,), (65701,))  # constructor of Prelude.Monad.Monad, {U_Python.Prim.{iter1}1}, {U_Python.Prim.{iter5}1}
          ),
          APPLY0(APPLY0(e4, e3), in7)
        ),
        (65702, e2, e4)  # {U_Python.Prim.{iter6}1}
      )
    elif in0[0] == 0:  # Prelude.Maybe.Nothing
      aux2 = (0, (65704,), (65695,))  # constructor of Prelude.Monad.Monad, {U_Python.Prim.{iter8}1}, {U_Python.Prim.{iter12}1}
      if aux2[0] == 0:  # constructor of Prelude.Monad.Monad
        in9, in10, = aux2[1:]
        aux3 = in9
      else:
        idris_error("unreachable case")
      aux1 = APPLY0(idris_Prelude_46_Applicative_46_pure(None, None, aux3), e3)
    else:
      idris_error("unreachable case")
    return aux1

# Main.{main13}
def idris_Main_46__123_main13_125_(in25, in26):
  while True:
    if in25[0] == 11:  # Python.Exceptions.OSError
      aux1 = idris_Prelude_46_putStr(
        None,
        "  -> (1) everything's fine: " + idris_Python_46_Exceptions_46_showException(in26) + "\n"
      )
    else:
      aux1 = idris_Python_46_Exceptions_46_raise(None, in26)
    return aux1

# Main.{main14}
def idris_Main_46__123_main14_125_(in25):
  while True:
    return (65640, in25)  # {U_Main.{main13}1}

# Main.{main15}
def idris_Main_46__123_main15_125_(in28):
  while True:
    if in28[0] == 1:  # Python.Exceptions.Except
      in29, in30, = in28[1:]
      if in29[0] == 11:  # Python.Exceptions.OSError
        aux2 = idris_Prelude_46_putStr(
          None,
          "  -> (2) everything's fine: " + idris_Python_46_Exceptions_46_showException(in30) + "\n"
        )
      else:
        aux2 = idris_Python_46_Exceptions_46_raise(None, in30)
      aux1 = aux2
    elif in28[0] == 0:  # Python.Exceptions.OK
      in31, = in28[1:]
      aux1 = idris_Prelude_46_putStr(
        None,
        "Your root could probably use some security lessons!\n"
      )
    else:
      idris_error("unreachable case")
    return aux1

# Main.{main16}
def idris_Main_46__123_main16_125_(in22, in27):
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46_Exceptions_46_try(
        None,
        idris_Python_46__36__58_(
          None,
          None,
          idris_Python_46__47__46_(None, None, in22, "mkdir", None),
          (1, "/root/hello", (0,))  # Python.::, Python.Nil
        )
      ),
      (65642,)  # {U_Main.{main15}1}
    )

# Main.{main17}
def idris_Main_46__123_main17_125_(in22, in23):
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46_Exceptions_46_catch(
        None,
        idris_Python_46_Exceptions_46_try(
          None,
          (
            65726,  # {U_io_bind1}
            None,
            None,
            None,
            idris_Python_46__36__58_(
              None,
              None,
              idris_Python_46__47__46_(None, None, in22, "mkdir", None),
              (1, "/root/hello", (0,))  # Python.::, Python.Nil
            ),
            (65639,)  # {U_Main.{main12}1}
          )
        ),
        (65641,)  # {U_Main.{main14}1}
      ),
      (65643, in22)  # {U_Main.{main16}1}
    )

# Main.{main18}
def idris_Main_46__123_main18_125_(in22):
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Prelude_46_putStr(None, "And now, let's fail!\n"),
      (65644, in22)  # {U_Main.{main17}1}
    )

# Main.{main19}
def idris_Main_46__123_main19_125_(in21):
  while True:
    return (65726, None, None, None, idris_Python_46_Lib_46_Os_46_import_95_(), (65645,))  # {U_io_bind1}, {U_Main.{main18}1}

# Main.{main20}
def idris_Main_46__123_main20_125_(in20):
  while True:
    return (65726, None, None, None, idris_Prelude_46_putStr(None, "\n"), (65646,))  # {U_io_bind1}, {U_Main.{main19}1}

# Main.{main21}
def idris_Main_46__123_main21_125_(in19):
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Prelude_46_putStr(None, "Total number of features: " + str(in19) + "\n"),
      (65648,)  # {U_Main.{main20}1}
    )

# Main.{main22}
def idris_Main_46__123_main22_125_(in5, in6):
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46_Prim_46_iterate(None, None, in5, 0, (65638,)),  # {U_Main.{main11}1}
      (65649,)  # {U_Main.{main21}1}
    )

# Main.{main23}
def idris_Main_46__123_main23_125_(in5):
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Prelude_46_putStr(
        None,
        "Idris has got the following exciting features:\n"
      ),
      (65650, in5)  # {U_Main.{main22}1}
    )

# Main.{main24}
def idris_Main_46__123_main24_125_(in4):
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46__62__58_(
        None,
        idris_Python_46__36__58_(
          None,
          None,
          idris_Python_46__47__46_(None, None, in4, "select", None),
          (1, "div.entry-content li", (0,))  # Python.::, Python.Nil
        ),
        None,
        None
      ),
      (65651,)  # {U_Main.{main23}1}
    )

# Main.{main25}
def idris_Main_46__123_main25_125_(in2, in3):
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46__36__58_(
        None,
        None,
        idris_Python_46__47__46_(None, None, in3, "BeautifulSoup", None),
        (1, in2, (0,))  # Python.::, Python.Nil
      ),
      (65652,)  # {U_Main.{main24}1}
    )

# Main.{main26}
def idris_Main_46__123_main26_125_(in2):
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46_Lib_46_BeautifulSoup_46_import_95_(),
      (65653, in2)  # {U_Main.{main25}1}
    )

# Main.{main27}
def idris_Main_46__123_main27_125_(in1):
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46__47__58_(
        None,
        None,
        idris_Python_46__36__58_(
          None,
          None,
          idris_Python_46__47__46_(None, None, in1, "get", None),
          (1, "http://idris-lang.org", (0,))  # Python.::, Python.Nil
        ),
        "text",
        None
      ),
      (65654,)  # {U_Main.{main26}1}
    )

# Main.{main28}
def idris_Main_46__123_main28_125_(in0):
  while True:
    return (
      65726,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46__36__58_(
        None,
        None,
        idris_Python_46__47__46_(None, None, in0, "Session", None),
        (0,)  # Python.Nil
      ),
      (65655,)  # {U_Main.{main27}1}
    )

# Prelude.List.reverse, reverse'
def idris_Prelude_46_List_46_reverse_58_reverse_39__58_0(e0, e1, e2):
  while True:
    if e2[0] == 1:  # Prelude.List.::
      in0, in1, = e2[1:]
      e0, e1, e2, = None, (1, in0, e1), in1,  # Prelude.List.::
      continue
      aux1 = idris_error("unreachable due to tail call")
    elif e2[0] == 0:  # Prelude.List.Nil
      aux1 = e1
    else:
      idris_error("unreachable case")
    return aux1

# Decidable.Equality.Decidable.Equality.Char instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Char_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Decidable.Equality.Decidable.Equality.Int instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Int_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Decidable.Equality.Decidable.Equality.Integer instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Integer_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Decidable.Equality.Decidable.Equality.String instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_String_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Prelude.Foldable.Prelude.List.List instance of Prelude.Foldable.Foldable, method foldr
def idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
  e0, e1, e2, e3, e4
):
  while True:
    if e4[0] == 1:  # Prelude.List.::
      in0, in1, = e4[1:]
      aux1 = APPLY0(
        APPLY0(e2, in0),
        APPLY0(
          APPLY0(
            APPLY0(idris_Prelude_46_Foldable_46_foldr(None, None, None, (65733,)), e2),  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable5}
            e3
          ),
          in1
        )
      )
    elif e4[0] == 0:  # Prelude.List.Nil
      aux1 = e3
    else:
      idris_error("unreachable case")
    return aux1

# Prelude.Functor.Prelude.IO' ffi instance of Prelude.Functor.Functor, method map
def idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
  e0, e1, e2, e3, e4
):
  while True:
    return (65726, None, None, None, e4, (65666, e3))  # {U_io_bind1}, {U_Prelude.Functor.{Prelude.IO' ffi instance of Prelude.Functor.Functor, method map_lam0}1}

# Prelude.List.List instance of Prelude.Foldable.Foldable
def idris_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List(
  meth0, meth1, meth2, meth3, meth4
):
  while True:
    return idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
      None, None, meth2, meth3, meth4
    )

# case block in Void
def idris_Void_95_case():
  while True:
    return None

# Python.Exceptions.case block in catch
def idris_Python_46_Exceptions_46_catch_95_case(e0, e1, e2, e3, e4, e5):
  while True:
    if e4[0] == 1:  # Python.Exceptions.Except
      in0, in1, = e4[1:]
      aux1 = APPLY0(APPLY0(e2, in0), in1)
    elif e4[0] == 0:  # Python.Exceptions.OK
      in2, = e4[1:]
      if e3[0] == 0:  # constructor of Prelude.Monad.Monad
        in3, in4, = e3[1:]
        aux2 = in3
      else:
        idris_error("unreachable case")
      aux1 = APPLY0(idris_Prelude_46_Applicative_46_pure(None, None, aux2), in2)
    else:
      idris_error("unreachable case")
    return aux1

# Python.Exceptions.case block in fromString
def idris_Python_46_Exceptions_46_fromString_95_case(e0, e1):
  while True:
    if e0 == "ArithmeticError":
      aux1 = (3,)  # Python.Exceptions.ArithmeticError
    elif e0 == "AssertionError":
      aux1 = (7,)  # Python.Exceptions.AssertionError
    elif e0 == "AttributeError":
      aux1 = (8,)  # Python.Exceptions.AttributeError
    elif e0 == "BufferError":
      aux1 = (2,)  # Python.Exceptions.BufferError
    elif e0 == "EOFError":
      aux1 = (14,)  # Python.Exceptions.EOFError
    elif e0 == "EnvironmentError":
      aux1 = (9,)  # Python.Exceptions.EnvironmentError
    elif e0 == "FloatingPointError":
      aux1 = (4,)  # Python.Exceptions.FloatingPointError
    elif e0 == "IOError":
      aux1 = (10,)  # Python.Exceptions.IOError
    elif e0 == "ImportError":
      aux1 = (15,)  # Python.Exceptions.ImportError
    elif e0 == "IndentationError":
      aux1 = (26,)  # Python.Exceptions.IndentationError
    elif e0 == "IndexError":
      aux1 = (17,)  # Python.Exceptions.IndexError
    elif e0 == "KeyError":
      aux1 = (18,)  # Python.Exceptions.KeyError
    elif e0 == "LookupError":
      aux1 = (16,)  # Python.Exceptions.LookupError
    elif e0 == "MemoryError":
      aux1 = (19,)  # Python.Exceptions.MemoryError
    elif e0 == "NameError":
      aux1 = (20,)  # Python.Exceptions.NameError
    elif e0 == "NotImplementedError":
      aux1 = (24,)  # Python.Exceptions.NotImplementedError
    elif e0 == "OSError":
      aux1 = (11,)  # Python.Exceptions.OSError
    elif e0 == "OverflowError":
      aux1 = (5,)  # Python.Exceptions.OverflowError
    elif e0 == "ReferenceError":
      aux1 = (22,)  # Python.Exceptions.ReferenceError
    elif e0 == "RuntimeError":
      aux1 = (23,)  # Python.Exceptions.RuntimeError
    elif e0 == "StandardError":
      aux1 = (1,)  # Python.Exceptions.StandardError
    elif e0 == "StopIteration":
      aux1 = (0,)  # Python.Exceptions.StopIteration
    elif e0 == "SyntaxError":
      aux1 = (25,)  # Python.Exceptions.SyntaxError
    elif e0 == "SystemError":
      aux1 = (28,)  # Python.Exceptions.SystemError
    elif e0 == "TabError":
      aux1 = (27,)  # Python.Exceptions.TabError
    elif e0 == "TypeError":
      aux1 = (29,)  # Python.Exceptions.TypeError
    elif e0 == "UnboundLocalError":
      aux1 = (21,)  # Python.Exceptions.UnboundLocalError
    elif e0 == "UnicodeDecodeError":
      aux1 = (32,)  # Python.Exceptions.UnicodeDecodeError
    elif e0 == "UnicodeEncodeError":
      aux1 = (33,)  # Python.Exceptions.UnicodeEncodeError
    elif e0 == "UnicodeError":
      aux1 = (31,)  # Python.Exceptions.UnicodeError
    elif e0 == "UnicodeTranslateError":
      aux1 = (34,)  # Python.Exceptions.UnicodeTranslateError
    elif e0 == "VMSError":
      aux1 = (13,)  # Python.Exceptions.VMSError
    elif e0 == "ValueError":
      aux1 = (30,)  # Python.Exceptions.ValueError
    elif e0 == "WindowsError":
      aux1 = (12,)  # Python.Exceptions.WindowsError
    elif e0 == "ZeroDivisionError":
      aux1 = (6,)  # Python.Exceptions.ZeroDivisionError
    else:
      aux1 = (35,)  # Python.Exceptions.Other
    return aux1

# case block in io_bind
def idris_io_95_bind_95_case(e0, e1, e2, e3, e4, e5, e6, e7):
  while True:
    return APPLY0(e7, e5)

# Python.Prim.case block in iter
def idris_Python_46_Prim_46_iter_95_case(e0, e1, e2, e3, e4, e5, e6, e7):
  while True:
    if e6[0] == 1:  # Prelude.Maybe.Just
      in0, = e6[1:]
      aux1 = APPLY0(
        APPLY0(
          idris_Prelude_46_Monad_46__62__62__61_(None, None, None, e5),
          APPLY0(APPLY0(e4, e3), in0)
        ),
        (65687, e1, e4)  # {U_Python.Prim.{case block in iter_lam0}1}
      )
    elif e6[0] == 0:  # Prelude.Maybe.Nothing
      if e5[0] == 0:  # constructor of Prelude.Monad.Monad
        in2, in3, = e5[1:]
        aux2 = in2
      else:
        idris_error("unreachable case")
      aux1 = APPLY0(idris_Prelude_46_Applicative_46_pure(None, None, aux2), e3)
    else:
      idris_error("unreachable case")
    return aux1

# Main.case block in main
def idris_Main_46_main_95_case(
  e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14,
  e15, e16, e17, e18, e19, e20, e21, e22, e23, e24, e25, e26, e27
):
  while True:
    if e25[0] == 11:  # Python.Exceptions.OSError
      aux1 = idris_Prelude_46_putStr(
        None,
        "  -> (1) everything's fine: " + idris_Python_46_Exceptions_46_showException(e26) + "\n"
      )
    else:
      aux1 = idris_Python_46_Exceptions_46_raise(None, e26)
    return aux1

# Main.case block in main1
def idris_Main_46_main1_95_case(
  e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14,
  e15, e16, e17, e18, e19, e20, e21, e22, e23, e24, e25, e26, e27,
  e28
):
  while True:
    if e27[0] == 1:  # Python.Exceptions.Except
      in0, in1, = e27[1:]
      if in0[0] == 11:  # Python.Exceptions.OSError
        aux2 = idris_Prelude_46_putStr(
          None,
          "  -> (2) everything's fine: " + idris_Python_46_Exceptions_46_showException(in1) + "\n"
        )
      else:
        aux2 = idris_Python_46_Exceptions_46_raise(None, in1)
      aux1 = aux2
    elif e27[0] == 0:  # Python.Exceptions.OK
      in2, = e27[1:]
      aux1 = idris_Prelude_46_putStr(
        None,
        "Your root could probably use some security lessons!\n"
      )
    else:
      idris_error("unreachable case")
    return aux1

# Python.Prim.case block in next
def idris_Python_46_Prim_46_next_95_case(e0, e1, e2, e3, e4):
  while True:
    if e3[0] == 1:  # Python.Exceptions.Except
      in0, in1, = e3[1:]
      if in0[0] == 0:  # Python.Exceptions.StopIteration
        if e2[0] == 0:  # constructor of Prelude.Monad.Monad
          in2, in3, = e2[1:]
          aux3 = in2
        else:
          idris_error("unreachable case")
        aux2 = APPLY0(idris_Prelude_46_Applicative_46_pure(None, None, aux3), (0,))  # Prelude.Maybe.Nothing
      else:
        aux2 = idris_Python_46_Exceptions_46_raise(None, in1)
      aux1 = aux2
    elif e3[0] == 0:  # Python.Exceptions.OK
      in4, = e3[1:]
      if e2[0] == 0:  # constructor of Prelude.Monad.Monad
        in5, in6, = e2[1:]
        aux4 = in5
      else:
        idris_error("unreachable case")
      aux1 = APPLY0(idris_Prelude_46_Applicative_46_pure(None, None, aux4), (1, in4))  # Prelude.Maybe.Just
    else:
      idris_error("unreachable case")
    return aux1

# Python.Exceptions.case block in try
def idris_Python_46_Exceptions_46_try_95_case(e0, e1, e2, e3, e4, e5, e6, lamp7):
  while True:
    if e5 == "ArithmeticError":
      aux1 = (3,)  # Python.Exceptions.ArithmeticError
    elif e5 == "AssertionError":
      aux1 = (7,)  # Python.Exceptions.AssertionError
    elif e5 == "AttributeError":
      aux1 = (8,)  # Python.Exceptions.AttributeError
    elif e5 == "BufferError":
      aux1 = (2,)  # Python.Exceptions.BufferError
    elif e5 == "EOFError":
      aux1 = (14,)  # Python.Exceptions.EOFError
    elif e5 == "EnvironmentError":
      aux1 = (9,)  # Python.Exceptions.EnvironmentError
    elif e5 == "FloatingPointError":
      aux1 = (4,)  # Python.Exceptions.FloatingPointError
    elif e5 == "IOError":
      aux1 = (10,)  # Python.Exceptions.IOError
    elif e5 == "ImportError":
      aux1 = (15,)  # Python.Exceptions.ImportError
    elif e5 == "IndentationError":
      aux1 = (26,)  # Python.Exceptions.IndentationError
    elif e5 == "IndexError":
      aux1 = (17,)  # Python.Exceptions.IndexError
    elif e5 == "KeyError":
      aux1 = (18,)  # Python.Exceptions.KeyError
    elif e5 == "LookupError":
      aux1 = (16,)  # Python.Exceptions.LookupError
    elif e5 == "MemoryError":
      aux1 = (19,)  # Python.Exceptions.MemoryError
    elif e5 == "NameError":
      aux1 = (20,)  # Python.Exceptions.NameError
    elif e5 == "NotImplementedError":
      aux1 = (24,)  # Python.Exceptions.NotImplementedError
    elif e5 == "OSError":
      aux1 = (11,)  # Python.Exceptions.OSError
    elif e5 == "OverflowError":
      aux1 = (5,)  # Python.Exceptions.OverflowError
    elif e5 == "ReferenceError":
      aux1 = (22,)  # Python.Exceptions.ReferenceError
    elif e5 == "RuntimeError":
      aux1 = (23,)  # Python.Exceptions.RuntimeError
    elif e5 == "StandardError":
      aux1 = (1,)  # Python.Exceptions.StandardError
    elif e5 == "StopIteration":
      aux1 = (0,)  # Python.Exceptions.StopIteration
    elif e5 == "SyntaxError":
      aux1 = (25,)  # Python.Exceptions.SyntaxError
    elif e5 == "SystemError":
      aux1 = (28,)  # Python.Exceptions.SystemError
    elif e5 == "TabError":
      aux1 = (27,)  # Python.Exceptions.TabError
    elif e5 == "TypeError":
      aux1 = (29,)  # Python.Exceptions.TypeError
    elif e5 == "UnboundLocalError":
      aux1 = (21,)  # Python.Exceptions.UnboundLocalError
    elif e5 == "UnicodeDecodeError":
      aux1 = (32,)  # Python.Exceptions.UnicodeDecodeError
    elif e5 == "UnicodeEncodeError":
      aux1 = (33,)  # Python.Exceptions.UnicodeEncodeError
    elif e5 == "UnicodeError":
      aux1 = (31,)  # Python.Exceptions.UnicodeError
    elif e5 == "UnicodeTranslateError":
      aux1 = (34,)  # Python.Exceptions.UnicodeTranslateError
    elif e5 == "VMSError":
      aux1 = (13,)  # Python.Exceptions.VMSError
    elif e5 == "ValueError":
      aux1 = (30,)  # Python.Exceptions.ValueError
    elif e5 == "WindowsError":
      aux1 = (12,)  # Python.Exceptions.WindowsError
    elif e5 == "ZeroDivisionError":
      aux1 = (6,)  # Python.Exceptions.ZeroDivisionError
    else:
      aux1 = (35,)  # Python.Exceptions.Other
    return (1, aux1, lamp7)  # Python.Exceptions.Except

# Main.case block in case block in main
def idris_Main_46_main_95_case_95_case(
  e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14,
  e15, e16, e17, e18, e19, e20, e21, e22, e23, e24, e25, e26, e27,
  e28
):
  while True:
    if e25[0] == 11:  # Python.Exceptions.OSError
      aux1 = idris_Prelude_46_putStr(
        None,
        "  -> (1) everything's fine: " + idris_Python_46_Exceptions_46_showException(e26) + "\n"
      )
    else:
      aux1 = idris_Python_46_Exceptions_46_raise(None, e26)
    return aux1

# <<Void eliminator>>
def idris_Void_95_elim():
  while True:
    return None

if __name__ == '__main__':
  runMain0()
