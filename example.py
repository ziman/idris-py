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

# Python.$.
def idris_Python_46__36__46_(e0, e1, e2, idris_args):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65668, None, None),  # {U_believe_me1}
      (65660, e2, idris_args)  # {U_Python.{$.0}1}
    )

# Python.$:
def idris_Python_46__36__58_(e0, e1, e2, idris_args):
  while True:
    return (65669, None, None, None, e2, (65661, idris_args))  # {U_io_bind1}, {U_Python.{$:0}1}

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
      (65668, None, None),  # {U_believe_me1}
      (65662, e2, e3)  # {U_Python.{/.0}1}
    )

# Python./:
def idris_Python_46__47__58_(e0, e1, e2, e3, e4):
  while True:
    return (65669, None, None, None, e2, (65663, e3))  # {U_io_bind1}, {U_Python.{/:0}1}

# Prelude.Algebra.<+>
def idris_Prelude_46_Algebra_46__60__43__62_(e0, e1):
  while True:
    return e1

# @@constructor of Prelude.Algebra.Monoid#Semigroup a
def idris__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a(
  e0,
  e1
):
  while True:
    if e1[0] == 0:  # constructor of Prelude.Algebra.Monoid
      in0, in1, = e1[1:]
      aux1 = in0
    else:
      idris_error("unreachable case")
    return aux1

# believe_me
def idris_believe_95_me(e0, e1, e2):
  while True:
    return e2

# call__IO
def idris_call_95__95_IO(e0, e1, e2):
  while True:
    return APPLY0(e2, None)

# Python.collect
def idris_Python_46_collect(e0, e1):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65656, None, (0,)),  # {U_Prelude.List.reverse, reverse'1}, Prelude.List.Nil
      idris_Python_46_foreach(None, None, e1, (0,), (65665,))  # Prelude.List.Nil, {U_Python.{collect1}1}
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

# Python.foreach
def idris_Python_46_foreach(e0, e1, e2, e3, e4):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65668, None, None),  # {U_believe_me1}
      (65666, e2, e3, e4)  # {U_Python.{foreach0}1}
    )

# Python.importModule
def idris_Python_46_importModule(e0, e1):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      (65668, None, None),  # {U_believe_me1}
      (65667, e1)  # {U_Python.{importModule0}1}
    )

# Python.BeautifulSoup.import_
def idris_Python_46_BeautifulSoup_46_import_95_():
  while True:
    return idris_Python_46_importModule(None, "bs4")

# Python.Requests.import_
def idris_Python_46_Requests_46_import_95_():
  while True:
    return idris_Python_46_importModule(None, "requests")

# io_bind
def idris_io_95_bind(e0, e1, e2, e3, e4, idris_w):
  while True:
    return APPLY0(io_bind2(e0, e1, e2, e3, e4, idris_w), APPLY0(e3, idris_w))

# io_return
def idris_io_95_return(e0, e1, e2, idris_w):
  while True:
    return e2

# Main.main
def idris_Main_46_main():
  while True:
    return (65669, None, None, None, idris_Python_46_Requests_46_import_95_(), (65643,))  # {U_io_bind1}, {U_Main.{main19}1}

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

# Prelude.putStr
def idris_Prelude_46_putStr(e0, e1):
  while True:
    return (65669, None, None, None, (65657, e1), (65658,))  # {U_io_bind1}, {U_Prelude.{putStr0}1}, {U_Prelude.{putStr1}1}

# run__IO
def idris_run_95__95_IO(e0, e1):
  while True:
    return APPLY0(e1, None)

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
    return idris_call(e2, idris_believe_95_me(None, None, idris_args))

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
    if fn0[0] < 65655:
      if fn0[0] < 65644:
        if fn0[0] < 65638:
          if fn0[0] < 65635:
            if fn0[0] == 65633:  # {U_Main.{main0}1}
              P_c0, P_c1, = fn0[1:]
              aux1 = idris_Main_46__123_main0_125_(P_c0, P_c1, arg0)
            elif fn0[0] == 65634:  # {U_Main.{main10}1}
              P_c0, = fn0[1:]
              aux1 = idris_Main_46__123_main10_125_(P_c0, arg0)
          else:
            if fn0[0] == 65635:  # {U_Main.{main11}1}
              aux1 = idris_Main_46__123_main11_125_(arg0)
            elif fn0[0] == 65636:  # {U_Main.{main12}1}
              aux1 = idris_Main_46__123_main12_125_(arg0)
            elif fn0[0] == 65637:  # {U_Main.{main13}1}
              P_c0, = fn0[1:]
              aux1 = idris_Main_46__123_main13_125_(P_c0, arg0)
        else:
          if fn0[0] < 65641:
            if fn0[0] == 65638:  # {U_Main.{main14}1}
              aux1 = idris_Main_46__123_main14_125_(arg0)
            elif fn0[0] == 65639:  # {U_Main.{main15}1}
              aux1 = idris_Main_46__123_main15_125_(arg0)
            elif fn0[0] == 65640:  # {U_Main.{main16}1}
              P_c0, = fn0[1:]
              aux1 = idris_Main_46__123_main16_125_(P_c0, arg0)
          else:
            if fn0[0] == 65641:  # {U_Main.{main17}1}
              aux1 = idris_Main_46__123_main17_125_(arg0)
            elif fn0[0] == 65642:  # {U_Main.{main18}1}
              aux1 = idris_Main_46__123_main18_125_(arg0)
            elif fn0[0] == 65643:  # {U_Main.{main19}1}
              aux1 = idris_Main_46__123_main19_125_(arg0)
      else:
        if fn0[0] < 65649:
          if fn0[0] < 65646:
            if fn0[0] == 65644:  # {U_Main.{main1}1}
              P_c0, = fn0[1:]
              aux1 = idris_Main_46__123_main1_125_(P_c0, arg0)
            elif fn0[0] == 65645:  # {U_Main.{main2}1}
              aux1 = idris_Main_46__123_main2_125_(arg0)
          else:
            if fn0[0] == 65646:  # {U_Main.{main3}1}
              aux1 = idris_Main_46__123_main3_125_(arg0)
            elif fn0[0] == 65647:  # {U_Main.{main4}1}
              aux1 = idris_Main_46__123_main4_125_(arg0)
            elif fn0[0] == 65648:  # {U_Main.{main5}1}
              P_c0, = fn0[1:]
              aux1 = idris_Main_46__123_main5_125_(P_c0, arg0)
        else:
          if fn0[0] < 65652:
            if fn0[0] == 65649:  # {U_Main.{main6}1}
              aux1 = idris_Main_46__123_main6_125_(arg0)
            elif fn0[0] == 65650:  # {U_Main.{main7}1}
              aux1 = idris_Main_46__123_main7_125_(arg0)
            elif fn0[0] == 65651:  # {U_Main.{main8}1}
              P_c0, = fn0[1:]
              aux1 = idris_Main_46__123_main8_125_(P_c0, arg0)
          else:
            if fn0[0] == 65652:  # {U_Main.{main9}1}
              P_c0, = fn0[1:]
              aux1 = idris_Main_46__123_main9_125_(P_c0, arg0)
            elif fn0[0] == 65653:  # {U_Prelude.Basics..1}
              P_c0, P_c1, P_c2, P_c3, P_c4, = fn0[1:]
              aux1 = idris_Prelude_46_Basics_46__46_(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
            elif fn0[0] == 65654:  # {U_Prelude.Functor.{Prelude.IO' ffi instance of Prelude.Functor.Functor, method map_lam0}1}
              P_c0, = fn0[1:]
              aux1 = idris_Prelude_46_Functor_46__123_Prelude_46_IO_39__32_ffi_32_instance_32_of_32_Prelude_46_Functor_46_Functor_44__32_method_32_map_95_lam0_125_(
                P_c0,
                arg0
              )
    else:
      if fn0[0] < 65666:
        if fn0[0] < 65660:
          if fn0[0] < 65657:
            if fn0[0] == 65655:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable1}
              P_c0, P_c1, P_c2, P_c3, = fn0[1:]
              aux1 = idris_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List(
                P_c0,
                P_c1,
                P_c2,
                P_c3,
                arg0
              )
            elif fn0[0] == 65656:  # {U_Prelude.List.reverse, reverse'1}
              P_c0, P_c1, = fn0[1:]
              aux1 = idris_Prelude_46_List_46_reverse_58_reverse_39__58_0(P_c0, P_c1, arg0)
          else:
            if fn0[0] == 65657:  # {U_Prelude.{putStr0}1}
              P_c0, = fn0[1:]
              aux1 = idris_Prelude_46__123_putStr0_125_(P_c0, arg0)
            elif fn0[0] == 65658:  # {U_Prelude.{putStr1}1}
              aux1 = idris_Prelude_46__123_putStr1_125_(arg0)
            elif fn0[0] == 65659:  # {U_Python.collect1}
              P_c0, = fn0[1:]
              aux1 = idris_Python_46_collect(P_c0, arg0)
        else:
          if fn0[0] < 65663:
            if fn0[0] == 65660:  # {U_Python.{$.0}1}
              P_c0, P_c1, = fn0[1:]
              aux1 = idris_Python_46__123__36__46_0_125_(P_c0, P_c1, arg0)
            elif fn0[0] == 65661:  # {U_Python.{$:0}1}
              P_c0, = fn0[1:]
              aux1 = idris_Python_46__123__36__58_0_125_(P_c0, arg0)
            elif fn0[0] == 65662:  # {U_Python.{/.0}1}
              P_c0, P_c1, = fn0[1:]
              aux1 = idris_Python_46__123__47__46_0_125_(P_c0, P_c1, arg0)
          else:
            if fn0[0] == 65663:  # {U_Python.{/:0}1}
              P_c0, = fn0[1:]
              aux1 = idris_Python_46__123__47__58_0_125_(P_c0, arg0)
            elif fn0[0] == 65664:  # {U_Python.{collect0}1}
              P_c0, = fn0[1:]
              aux1 = idris_Python_46__123_collect0_125_(P_c0, arg0)
            elif fn0[0] == 65665:  # {U_Python.{collect1}1}
              aux1 = idris_Python_46__123_collect1_125_(arg0)
      else:
        if fn0[0] < 65671:
          if fn0[0] < 65668:
            if fn0[0] == 65666:  # {U_Python.{foreach0}1}
              P_c0, P_c1, P_c2, = fn0[1:]
              aux1 = idris_Python_46__123_foreach0_125_(P_c0, P_c1, P_c2, arg0)
            elif fn0[0] == 65667:  # {U_Python.{importModule0}1}
              P_c0, = fn0[1:]
              aux1 = idris_Python_46__123_importModule0_125_(P_c0, arg0)
          else:
            if fn0[0] == 65668:  # {U_believe_me1}
              P_c0, P_c1, = fn0[1:]
              aux1 = idris_believe_95_me(P_c0, P_c1, arg0)
            elif fn0[0] == 65669:  # {U_io_bind1}
              P_c0, P_c1, P_c2, P_c3, P_c4, = fn0[1:]
              aux1 = idris_io_95_bind(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
            elif fn0[0] == 65670:  # {U_io_return1}
              P_c0, P_c1, P_c2, = fn0[1:]
              aux1 = idris_io_95_return(P_c0, P_c1, P_c2, arg0)
        else:
          if fn0[0] < 65674:
            if fn0[0] == 65671:  # {U_{io_bind1}1}
              P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, = fn0[1:]
              aux1 = io_bind1(P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, arg0)
            elif fn0[0] == 65672:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable2}
              P_c0, P_c1, P_c2, = fn0[1:]
              aux1 = (65655, P_c0, P_c1, P_c2, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable1}
            elif fn0[0] == 65673:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable3}
              P_c0, P_c1, = fn0[1:]
              aux1 = (65672, P_c0, P_c1, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable2}
          else:
            if fn0[0] == 65674:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable4}
              P_c0, = fn0[1:]
              aux1 = (65673, P_c0, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable3}
            elif fn0[0] == 65675:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable5}
              aux1 = (65674, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable4}
    return aux1

# {EVAL0}
def EVAL0(arg0):
  while True:
    return arg0

# Prelude.Functor.{Prelude.IO' ffi instance of Prelude.Functor.Functor, method map_lam0}
def idris_Prelude_46_Functor_46__123_Prelude_46_IO_39__32_ffi_32_instance_32_of_32_Prelude_46_Functor_46_Functor_44__32_method_32_map_95_lam0_125_(
  e3,
  in0
):
  while True:
    return (65670, None, None, APPLY0(e3, in0))  # {U_io_return1}

# Python.{collect0}
def idris_Python_46__123_collect0_125_(in0, in1):
  while True:
    return (65670, None, None, (1, in1, in0))  # {U_io_return1}, Prelude.List.::

# Python.{foreach0}
def idris_Python_46__123_foreach0_125_(e2, e3, e4, in0):
  while True:
    return idris_foreach(
      e2,
      idris_believe_95_me(None, None, e3),
      idris_believe_95_me(None, None, e4)
    )

# Python.{importModule0}
def idris_Python_46__123_importModule0_125_(e1, in0):
  while True:
    return idris_pymodule(e1)

# {io_bind0}
def io_bind0(e0, e1, e2, e3, e4, idris_w, in0):
  while True:
    return APPLY0(e4, in0)

# Main.{main0}
def idris_Main_46__123_main0_125_(in12, in13, in14):
  while True:
    return idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
      None,
      None,
      in12,
      in13,
      in14
    )

# Prelude.{putStr0}
def idris_Prelude_46__123_putStr0_125_(e1, in0):
  while True:
    return sys.stdout.write(e1)

# {runMain0}
def runMain0():
  while True:
    return EVAL0(APPLY0(idris_Main_46_main(), None))

# Python.{collect1}
def idris_Python_46__123_collect1_125_(in0):
  while True:
    return (65664, in0)  # {U_Python.{collect0}1}

# {io_bind1}
def io_bind1(e0, e1, e2, e3, e4, idris_w, in0):
  while True:
    return APPLY0(io_bind0(e0, e1, e2, e3, e4, idris_w, in0), idris_w)

# Main.{main1}
def idris_Main_46__123_main1_125_(in12, in13):
  while True:
    return (65633, in12, in13)  # {U_Main.{main0}1}

# Prelude.{putStr1}
def idris_Prelude_46__123_putStr1_125_(in1):
  while True:
    return (65670, None, None, (0,))  # {U_io_return1}, MkUnit

# {io_bind2}
def io_bind2(e0, e1, e2, e3, e4, idris_w):
  while True:
    return (65671, e0, e1, e2, e3, e4, idris_w)  # {U_{io_bind1}1}

# Main.{main2}
def idris_Main_46__123_main2_125_(in12):
  while True:
    return (65644, in12)  # {U_Main.{main1}1}

# Main.{main3}
def idris_Main_46__123_main3_125_(in11):
  while True:
    return (65645,)  # {U_Main.{main2}1}

# Main.{main4}
def idris_Main_46__123_main4_125_(in10):
  while True:
    return (65646,)  # {U_Main.{main3}1}

# Main.{main5}
def idris_Main_46__123_main5_125_(in15, in16):
  while True:
    return in15 + in16

# Main.{main6}
def idris_Main_46__123_main6_125_(in15):
  while True:
    return (65648, in15)  # {U_Main.{main5}1}

# Main.{main7}
def idris_Main_46__123_main7_125_(in9):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
      None,
      None,
      None,
      idris_Prelude_46_Foldable_46_concat(None, None, (65647,), (0, (65649,), "")),  # {U_Main.{main4}1}, constructor of Prelude.Algebra.Monoid, {U_Main.{main6}1}
      in9
    )

# Main.{main8}
def idris_Main_46__123_main8_125_(in7, in18):
  while True:
    return (65670, None, None, in7 + 1)  # {U_io_return1}

# Main.{main9}
def idris_Main_46__123_main9_125_(in7, in17):
  while True:
    return (
      65669,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Prelude_46_putStr(None, str(in7 + 1) + ". " + in17 + "\n"),
      (65651, in7)  # {U_Main.{main8}1}
    )

# Main.{main10}
def idris_Main_46__123_main10_125_(in7, in8):
  while True:
    return (
      65669,  # {U_io_bind1}
      None,
      None,
      None,
      (
        65669,  # {U_io_bind1}
        None,
        None,
        None,
        idris_Python_46__47__46_(None, None, in8, "strings", None),
        (65653, None, None, None, (65650,), (65659, None))  # {U_Prelude.Basics..1}, {U_Main.{main7}1}, {U_Python.collect1}
      ),
      (65652, in7)  # {U_Main.{main9}1}
    )

# Main.{main11}
def idris_Main_46__123_main11_125_(in7):
  while True:
    return (65634, in7)  # {U_Main.{main10}1}

# Main.{main12}
def idris_Main_46__123_main12_125_(in19):
  while True:
    return idris_Prelude_46_putStr(None, "Total number of features: " + str(in19) + "\n")

# Main.{main13}
def idris_Main_46__123_main13_125_(in5, in6):
  while True:
    return (
      65669,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46_foreach(None, None, in5, 0, (65635,)),  # {U_Main.{main11}1}
      (65636,)  # {U_Main.{main12}1}
    )

# Main.{main14}
def idris_Main_46__123_main14_125_(in5):
  while True:
    return (
      65669,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Prelude_46_putStr(
        None,
        "Idris has got the following exciting features:\n"
      ),
      (65637, in5)  # {U_Main.{main13}1}
    )

# Main.{main15}
def idris_Main_46__123_main15_125_(in4):
  while True:
    return (
      65669,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46__36__58_(
        None,
        None,
        idris_Python_46__47__46_(None, None, in4, "select", None),
        (1, "div.entry-content li", (0,))  # Python.::, Python.Nil
      ),
      (65638,)  # {U_Main.{main14}1}
    )

# Main.{main16}
def idris_Main_46__123_main16_125_(in2, in3):
  while True:
    return (
      65669,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46__36__58_(
        None,
        None,
        idris_Python_46__47__46_(None, None, in3, "BeautifulSoup", None),
        (1, in2, (0,))  # Python.::, Python.Nil
      ),
      (65639,)  # {U_Main.{main15}1}
    )

# Main.{main17}
def idris_Main_46__123_main17_125_(in2):
  while True:
    return (
      65669,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46_BeautifulSoup_46_import_95_(),
      (65640, in2)  # {U_Main.{main16}1}
    )

# Main.{main18}
def idris_Main_46__123_main18_125_(in1):
  while True:
    return (
      65669,  # {U_io_bind1}
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
      (65641,)  # {U_Main.{main17}1}
    )

# Main.{main19}
def idris_Main_46__123_main19_125_(in0):
  while True:
    return (
      65669,  # {U_io_bind1}
      None,
      None,
      None,
      idris_Python_46__36__58_(
        None,
        None,
        idris_Python_46__47__46_(None, None, in0, "Session", None),
        (0,)  # Python.Nil
      ),
      (65642,)  # {U_Main.{main18}1}
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
  e0,
  e1,
  e2,
  e3,
  e4
):
  while True:
    if e4[0] == 1:  # Prelude.List.::
      in0, in1, = e4[1:]
      aux1 = APPLY0(
        APPLY0(e2, in0),
        APPLY0(
          APPLY0(
            APPLY0(idris_Prelude_46_Foldable_46_foldr(None, None, None, (65675,)), e2),  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable5}
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
  e0,
  e1,
  e2,
  e3,
  e4
):
  while True:
    return (65669, None, None, None, e4, (65654, e3))  # {U_io_bind1}, {U_Prelude.Functor.{Prelude.IO' ffi instance of Prelude.Functor.Functor, method map_lam0}1}

# Prelude.List.List instance of Prelude.Foldable.Foldable
def idris_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List(
  meth0,
  meth1,
  meth2,
  meth3,
  meth4
):
  while True:
    return idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
      None,
      None,
      meth2,
      meth3,
      meth4
    )

# case block in Void
def idris_Void_95_case():
  while True:
    return None

# case block in io_bind
def idris_io_95_bind_95_case(e0, e1, e2, e3, e4, e5, e6, e7):
  while True:
    return APPLY0(e7, e5)

# <<Void eliminator>>
def idris_Void_95_elim():
  while True:
    return None

if __name__ == '__main__':
  runMain0()
