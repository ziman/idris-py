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
# Python..$
def idris_Python_46__46__36_(e0, e1, e2, idris_args):
  while True:
    return (65645, None, None, None, e2, (65640, idris_args))

# Python../
def idris_Python_46__46__47_(e0, e1, e2, e3, e4):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
             None,
             None,
             None,
             (65644, None, None),
             (65641, e2, e3)
           )

# believe_me
def idris_believe_95_me(e0, e1, e2):
  while True:
    return e2

# Python.callFixedMethod
def idris_Python_46_callFixedMethod(e0, e1, e2, e3):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
             None,
             None,
             None,
             (65644, None, None),
             (65642, e2, e3)
           )

# call__IO
def idris_call_95__95_IO(e0, e1, e2):
  while True:
    return APPLY0(e2, None)

# Python.import_
def idris_Python_46_import_95_(e0, e1):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
             None,
             None,
             None,
             (65644, None, None),
             (65643, e1)
           )

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
    return (65645, # {U_io_bind1}
             None,
             None,
             None,
             idris_Python_46_import_95_(None, "requests"),
             (65635,)
           )

# Prelude.Functor.map
def idris_Prelude_46_Functor_46_map(e0, e1, e2, e3):
  while True:
    return APPLY0(APPLY0(e3, e1), e2)

# Prelude.Nat.minus
def idris_Prelude_46_Nat_46_minus(e0, e1):
  while True:
    if e0 == 0:
      aux1 = 0
    else:
      if e1 == 0:
        aux2 = e0
      else:
        in0 = e1 - 1
        e0, e1, = e0 - 1, in0,
        continue
        aux2 = idris_error("unreachable due to tail call")
      aux1 = aux2
    return aux1

# mkForeignPrim
def idris_mkForeignPrim():
  while True:
    return None

# Main.mysum
def idris_Main_46_mysum(e0, e1):
  while True:
    if e1[0] == 1: # Prelude.List.::
      in0, in1, = e1[1:]
      e0, e1, = in0 + e0, in1,
      continue
      aux1 = idris_error("unreachable due to tail call")
    elif e1[0] == 0: # Prelude.List.Nil
      aux1 = e0
    else:
      idris_error("unreachable case")
    return aux1

# Prelude.natEnumFromTo
def idris_Prelude_46_natEnumFromTo(e0, e1):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46_List_46__64_Prelude_46_Functor_46_Functor_36_List_58__33_map_58_0(
             None,
             None,
             (65647, e0),
             idris_Prelude_46_List_46_reverse_58_reverse_39__58_0(
               None,
               (0,),
               idris_Prelude_46_natRange_58_go_58_0(
                 None,
                 idris_Prelude_46_Nat_46_minus(e1 + 1, e0)
               )
             )
           )

# prim__addBigInt
def idris_prim_95__95_addBigInt(op0, op1):
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

# prim__toStrBigInt
def idris_prim_95__95_toStrBigInt(op0):
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
    return (65645, None, None, None, (65638, e1), (65639,))

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

# Python.{.$0}
def idris_Python_46__123__46__36_0_125_(idris_args, in0):
  while True:
    return idris_Python_46_callFixedMethod(None, None, in0, idris_args)

# Python.{./0}
def idris_Python_46__123__46__47_0_125_(e2, e3, in0):
  while True:
    return idris_getfield(e2, e3)

# {APPLY0}
def APPLY0(fn0, arg0):
  while True:
    if fn0[0] == 65630: # {U_Main.{main0}1}
      aux1 = idris_Main_46__123_main0_125_(arg0)
    elif fn0[0] == 65631: # {U_Main.{main1}1}
      aux1 = idris_Main_46__123_main1_125_(arg0)
    elif fn0[0] == 65632: # {U_Main.{main2}1}
      aux1 = idris_Main_46__123_main2_125_(arg0)
    elif fn0[0] == 65633: # {U_Main.{main3}1}
      aux1 = idris_Main_46__123_main3_125_(arg0)
    elif fn0[0] == 65634: # {U_Main.{main4}1}
      aux1 = idris_Main_46__123_main4_125_(arg0)
    elif fn0[0] == 65635: # {U_Main.{main5}1}
      aux1 = idris_Main_46__123_main5_125_(arg0)
    elif fn0[0] == 65636: # {U_Prelude.Functor.{Prelude.IO' ffi instance of Prelude.Functor.Functor, method map0}1}
      P_c0, = fn0[1:]
      aux1 = idris_Prelude_46_Functor_46__123_Prelude_46_IO_39__32_ffi_32_instance_32_of_32_Prelude_46_Functor_46_Functor_44__32_method_32_map0_125_(
               P_c0,
               arg0
             )
    elif fn0[0] == 65637: # {U_Prelude.List.List instance of Prelude.Functor.Functor1}
      P_c0, P_c1, P_c2, = fn0[1:]
      aux1 = idris_Prelude_46_List_46__64_Prelude_46_Functor_46_Functor_36_List(
               P_c0,
               P_c1,
               P_c2,
               arg0
             )
    elif fn0[0] == 65638: # {U_Prelude.{putStr0}1}
      P_c0, = fn0[1:]
      aux1 = idris_Prelude_46__123_putStr0_125_(P_c0, arg0)
    elif fn0[0] == 65639: # {U_Prelude.{putStr1}1}
      aux1 = idris_Prelude_46__123_putStr1_125_(arg0)
    elif fn0[0] == 65640: # {U_Python.{.$0}1}
      P_c0, = fn0[1:]
      aux1 = idris_Python_46__123__46__36_0_125_(P_c0, arg0)
    elif fn0[0] == 65641: # {U_Python.{./0}1}
      P_c0, P_c1, = fn0[1:]
      aux1 = idris_Python_46__123__46__47_0_125_(P_c0, P_c1, arg0)
    elif fn0[0] == 65642: # {U_Python.{callFixedMethod0}1}
      P_c0, P_c1, = fn0[1:]
      aux1 = idris_Python_46__123_callFixedMethod0_125_(P_c0, P_c1, arg0)
    elif fn0[0] == 65643: # {U_Python.{import_0}1}
      P_c0, = fn0[1:]
      aux1 = idris_Python_46__123_import_95_0_125_(P_c0, arg0)
    elif fn0[0] == 65644: # {U_believe_me1}
      P_c0, P_c1, = fn0[1:]
      aux1 = idris_believe_95_me(P_c0, P_c1, arg0)
    elif fn0[0] == 65645: # {U_io_bind1}
      P_c0, P_c1, P_c2, P_c3, P_c4, = fn0[1:]
      aux1 = idris_io_95_bind(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
    elif fn0[0] == 65646: # {U_io_return1}
      P_c0, P_c1, P_c2, = fn0[1:]
      aux1 = idris_io_95_return(P_c0, P_c1, P_c2, arg0)
    elif fn0[0] == 65647: # {U_prim__addBigInt1}
      P_c0, = fn0[1:]
      aux1 = idris_prim_95__95_addBigInt(P_c0, arg0)
    elif fn0[0] == 65648: # {U_{io_bind1}1}
      P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, = fn0[1:]
      aux1 = io_bind1(P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, arg0)
    elif fn0[0] == 65649: # {U_Prelude.List.List instance of Prelude.Functor.Functor2}
      P_c0, P_c1, = fn0[1:]
      aux1 = (65637, P_c0, P_c1, arg0)
    elif fn0[0] == 65650: # {U_Prelude.List.List instance of Prelude.Functor.Functor3}
      P_c0, = fn0[1:]
      aux1 = (65649, P_c0, arg0)
    elif fn0[0] == 65651: # {U_Prelude.List.List instance of Prelude.Functor.Functor4}
      aux1 = (65650, arg0)
    else:
      aux1 = None
    return aux1

# {EVAL0}
def EVAL0(arg0):
  while True:
    return arg0

# Prelude.Functor.{Prelude.IO' ffi instance of Prelude.Functor.Functor, method map0}
def idris_Prelude_46_Functor_46__123_Prelude_46_IO_39__32_ffi_32_instance_32_of_32_Prelude_46_Functor_46_Functor_44__32_method_32_map0_125_(e3, in0):
  while True:
    return (65646, None, None, APPLY0(e3, in0))

# Python.{callFixedMethod0}
def idris_Python_46__123_callFixedMethod0_125_(e2, e3, in0):
  while True:
    return idris_call(e2, idris_believe_95_me(None, None, e3))

# Python.{import_0}
def idris_Python_46__123_import_95_0_125_(e1, in0):
  while True:
    return idris_pymodule(e1)

# {io_bind0}
def io_bind0(e0, e1, e2, e3, e4, idris_w, in0):
  while True:
    return APPLY0(e4, in0)

# Main.{main0}
def idris_Main_46__123_main0_125_(in5):
  while True:
    return idris_Prelude_46_putStr(
             None,
             "sum [1..10] = " + idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_Nat_58__33_show_58_0(
                                  idris_Main_46_mysum(0, idris_Prelude_46_natEnumFromTo(1, 10))
                                ) + "\n"
           )

# Prelude.{putStr0}
def idris_Prelude_46__123_putStr0_125_(e1, in0):
  while True:
    return sys.stdout.write(e1)

# {runMain0}
def runMain0():
  while True:
    return EVAL0(APPLY0(idris_Main_46_main(), None))

# {io_bind1}
def io_bind1(e0, e1, e2, e3, e4, idris_w, in0):
  while True:
    return APPLY0(io_bind0(e0, e1, e2, e3, e4, idris_w, in0), idris_w)

# Main.{main1}
def idris_Main_46__123_main1_125_(in4):
  while True:
    return (65645, # {U_io_bind1}
             None,
             None,
             None,
             idris_Prelude_46_putStr(None, "Hello world!\n"),
             (65630,)
           )

# Prelude.{putStr1}
def idris_Prelude_46__123_putStr1_125_(in1):
  while True:
    return (65646, None, None, (0,))

# {io_bind2}
def io_bind2(e0, e1, e2, e3, e4, idris_w):
  while True:
    return (65648, e0, e1, e2, e3, e4, idris_w)

# Main.{main2}
def idris_Main_46__123_main2_125_(in3):
  while True:
    return (65645, # {U_io_bind1}
             None,
             None,
             None,
             idris_Prelude_46_putStr(None, in3 + "\n"),
             (65631,)
           )

# Main.{main3}
def idris_Main_46__123_main3_125_(in2):
  while True:
    return (65645, # {U_io_bind1}
             None,
             None,
             None,
             idris_Python_46__46__47_(None, None, in2, "text", None),
             (65632,)
           )

# Main.{main4}
def idris_Main_46__123_main4_125_(in1):
  while True:
    return (65645, # {U_io_bind1}
             None,
             None,
             None,
             idris_Python_46__46__36_(
               None,
               None,
               idris_Python_46__46__47_(None, None, in1, "get", None),
               (1, "http://idris-lang.org", (0,))
             ),
             (65633,)
           )

# Main.{main5}
def idris_Main_46__123_main5_125_(in0):
  while True:
    return (65645, # {U_io_bind1}
             None,
             None,
             None,
             idris_Python_46__46__36_(
               None,
               None,
               idris_Python_46__46__47_(None, None, in0, "Session", None),
               (0,)
             ),
             (65634,)
           )

# Prelude.natRange, go
def idris_Prelude_46_natRange_58_go_58_0(e0, e1):
  while True:
    if e1 == 0:
      aux1 = (0,)
    else:
      in0 = e1 - 1
      aux1 = (1, in0, idris_Prelude_46_natRange_58_go_58_0(None, in0))
    return aux1

# Prelude.List.reverse, reverse'
def idris_Prelude_46_List_46_reverse_58_reverse_39__58_0(e0, e1, e2):
  while True:
    if e2[0] == 1: # Prelude.List.::
      in0, in1, = e2[1:]
      e0, e1, e2, = None, (1, in0, e1), in1,
      continue
      aux1 = idris_error("unreachable due to tail call")
    elif e2[0] == 0: # Prelude.List.Nil
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

# Prelude.Functor.Prelude.IO' ffi instance of Prelude.Functor.Functor, method map
def idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(e0, e1, e2, e3, e4):
  while True:
    return (65645, None, None, None, e4, (65636, e3))

# Prelude.Functor.Prelude.List.List instance of Prelude.Functor.Functor, method map
def idris_Prelude_46_Functor_46_Prelude_46_List_46__64_Prelude_46_Functor_46_Functor_36_List_58__33_map_58_0(e0, e1, e2, e3):
  while True:
    if e3[0] == 1: # Prelude.List.::
      in0, in1, = e3[1:]
      aux1 = (1, # Prelude.List.::
               APPLY0(e2, in0),
               APPLY0(
                 APPLY0(idris_Prelude_46_Functor_46_map(None, None, None, (65651,)), e2),
                 in1
               )
             )
    elif e3[0] == 0: # Prelude.List.Nil
      aux1 = (0,)
    else:
      idris_error("unreachable case")
    return aux1

# Prelude.Prelude.Nat instance of Prelude.Show, method show
def idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_Nat_58__33_show_58_0(e0):
  while True:
    return str(e0)

# Prelude.List.List instance of Prelude.Functor.Functor
def idris_Prelude_46_List_46__64_Prelude_46_Functor_46_Functor_36_List(meth0, meth1, meth2, meth3):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46_List_46__64_Prelude_46_Functor_46_Functor_36_List_58__33_map_58_0(
             None,
             None,
             meth2,
             meth3
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
