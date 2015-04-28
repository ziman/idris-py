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
    return (65659, None, None, None, e2, (65653, idris_args))

# Python../
def idris_Python_46__46__47_(e0, e1, e2, e3, e4):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
             None,
             None,
             None,
             (65658, None, None),
             (65654, e2, e3)
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
             (65658, None, None),
             (65655, e2, e3)
           )

# call__IO
def idris_call_95__95_IO(e0, e1, e2):
  while True:
    return APPLY0(e2, None)

# Python.foreach
def idris_Python_46_foreach(e0, e1, e2, e3, e4):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
             None,
             None,
             None,
             (65658, None, None),
             (65656, e2, e3, e4)
           )

# Python.import_
def idris_Python_46_import_95_(e0, e1):
  while True:
    return idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0(
             None,
             None,
             None,
             (65658, None, None),
             (65657, e1)
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
    return (65659, # {U_io_bind1}
             None,
             None,
             None,
             idris_Python_46_import_95_(None, (65636,)),
             (65639,)
           )

# mkForeignPrim
def idris_mkForeignPrim():
  while True:
    return None

# Python.moduleName
def idris_Python_46_moduleName(e0, e1):
  while True:
    return e1

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
    return (65659, None, None, None, (65651, e1), (65652,))

# Prelude.putStrLn
def idris_Prelude_46_putStrLn(e0, e1):
  while True:
    return idris_Prelude_46_putStr(None, e1 + "\n")

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
    if fn0[0] == 65636: # {U_Main.{main0}1}
      aux1 = idris_Main_46__123_main0_125_(arg0)
    elif fn0[0] == 65637: # {U_Main.{main10}1}
      aux1 = idris_Main_46__123_main10_125_(arg0)
    elif fn0[0] == 65638: # {U_Main.{main11}1}
      aux1 = idris_Main_46__123_main11_125_(arg0)
    elif fn0[0] == 65639: # {U_Main.{main12}1}
      aux1 = idris_Main_46__123_main12_125_(arg0)
    elif fn0[0] == 65640: # {U_Main.{main1}1}
      aux1 = idris_Main_46__123_main1_125_(arg0)
    elif fn0[0] == 65641: # {U_Main.{main2}1}
      P_c0, = fn0[1:]
      aux1 = idris_Main_46__123_main2_125_(P_c0, arg0)
    elif fn0[0] == 65642: # {U_Main.{main3}1}
      P_c0, = fn0[1:]
      aux1 = idris_Main_46__123_main3_125_(P_c0, arg0)
    elif fn0[0] == 65643: # {U_Main.{main4}1}
      aux1 = idris_Main_46__123_main4_125_(arg0)
    elif fn0[0] == 65644: # {U_Main.{main5}1}
      aux1 = idris_Main_46__123_main5_125_(arg0)
    elif fn0[0] == 65645: # {U_Main.{main6}1}
      aux1 = idris_Main_46__123_main6_125_(arg0)
    elif fn0[0] == 65646: # {U_Main.{main7}1}
      aux1 = idris_Main_46__123_main7_125_(arg0)
    elif fn0[0] == 65647: # {U_Main.{main8}1}
      P_c0, = fn0[1:]
      aux1 = idris_Main_46__123_main8_125_(P_c0, arg0)
    elif fn0[0] == 65648: # {U_Main.{main9}1}
      aux1 = idris_Main_46__123_main9_125_(arg0)
    elif fn0[0] == 65649: # {U_Prelude.Functor.{Prelude.IO' ffi instance of Prelude.Functor.Functor, method map0}1}
      P_c0, = fn0[1:]
      aux1 = idris_Prelude_46_Functor_46__123_Prelude_46_IO_39__32_ffi_32_instance_32_of_32_Prelude_46_Functor_46_Functor_44__32_method_32_map0_125_(
               P_c0,
               arg0
             )
    elif fn0[0] == 65650: # {U_Prelude.putStrLn1}
      P_c0, = fn0[1:]
      aux1 = idris_Prelude_46_putStrLn(P_c0, arg0)
    elif fn0[0] == 65651: # {U_Prelude.{putStr0}1}
      P_c0, = fn0[1:]
      aux1 = idris_Prelude_46__123_putStr0_125_(P_c0, arg0)
    elif fn0[0] == 65652: # {U_Prelude.{putStr1}1}
      aux1 = idris_Prelude_46__123_putStr1_125_(arg0)
    elif fn0[0] == 65653: # {U_Python.{.$0}1}
      P_c0, = fn0[1:]
      aux1 = idris_Python_46__123__46__36_0_125_(P_c0, arg0)
    elif fn0[0] == 65654: # {U_Python.{./0}1}
      P_c0, P_c1, = fn0[1:]
      aux1 = idris_Python_46__123__46__47_0_125_(P_c0, P_c1, arg0)
    elif fn0[0] == 65655: # {U_Python.{callFixedMethod0}1}
      P_c0, P_c1, = fn0[1:]
      aux1 = idris_Python_46__123_callFixedMethod0_125_(P_c0, P_c1, arg0)
    elif fn0[0] == 65656: # {U_Python.{foreach0}1}
      P_c0, P_c1, P_c2, = fn0[1:]
      aux1 = idris_Python_46__123_foreach0_125_(P_c0, P_c1, P_c2, arg0)
    elif fn0[0] == 65657: # {U_Python.{import_0}1}
      P_c0, = fn0[1:]
      aux1 = idris_Python_46__123_import_95_0_125_(P_c0, arg0)
    elif fn0[0] == 65658: # {U_believe_me1}
      P_c0, P_c1, = fn0[1:]
      aux1 = idris_believe_95_me(P_c0, P_c1, arg0)
    elif fn0[0] == 65659: # {U_io_bind1}
      P_c0, P_c1, P_c2, P_c3, P_c4, = fn0[1:]
      aux1 = idris_io_95_bind(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
    elif fn0[0] == 65660: # {U_io_return1}
      P_c0, P_c1, P_c2, = fn0[1:]
      aux1 = idris_io_95_return(P_c0, P_c1, P_c2, arg0)
    elif fn0[0] == 65661: # {U_{io_bind1}1}
      P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, = fn0[1:]
      aux1 = io_bind1(P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, arg0)
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
    return (65660, None, None, APPLY0(e3, in0))

# Python.{callFixedMethod0}
def idris_Python_46__123_callFixedMethod0_125_(e2, e3, in0):
  while True:
    return idris_call(e2, idris_believe_95_me(None, None, e3))

# Python.{foreach0}
def idris_Python_46__123_foreach0_125_(e2, e3, e4, in0):
  while True:
    return idris_foreach(
             e2,
             idris_believe_95_me(None, None, e3),
             idris_believe_95_me(None, None, e4)
           )

# Python.{import_0}
def idris_Python_46__123_import_95_0_125_(e1, in0):
  while True:
    return idris_pymodule(APPLY0(idris_Python_46_moduleName(None, e1), None))

# {io_bind0}
def io_bind0(e0, e1, e2, e3, e4, idris_w, in0):
  while True:
    return APPLY0(e4, in0)

# Main.{main0}
def idris_Main_46__123_main0_125_(in0):
  while True:
    return "requests"

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
def idris_Main_46__123_main1_125_(in5):
  while True:
    return "bs4"

# Prelude.{putStr1}
def idris_Prelude_46__123_putStr1_125_(in1):
  while True:
    return (65660, None, None, (0,))

# {io_bind2}
def io_bind2(e0, e1, e2, e3, e4, idris_w):
  while True:
    return (65661, e0, e1, e2, e3, e4, idris_w)

# Main.{main2}
def idris_Main_46__123_main2_125_(in9, in11):
  while True:
    return (65660, None, None, in9 + 1)

# Main.{main3}
def idris_Main_46__123_main3_125_(in9, in10):
  while True:
    return (65659, # {U_io_bind1}
             None,
             None,
             None,
             (65659, # {U_io_bind1}
               None,
               None,
               None,
               idris_Python_46__46__47_(None, None, in10, "string", None),
               (65650, None)
             ),
             (65641, in9)
           )

# Main.{main4}
def idris_Main_46__123_main4_125_(in9):
  while True:
    return (65642, in9)

# Main.{main5}
def idris_Main_46__123_main5_125_(in12):
  while True:
    return idris_Prelude_46_putStr(None, "Total number of features: " + str(in12) + "\n")

# Main.{main6}
def idris_Main_46__123_main6_125_(in8):
  while True:
    return (65659, # {U_io_bind1}
             None,
             None,
             None,
             idris_Python_46_foreach(None, None, in8, 0, (65643,)),
             (65644,)
           )

# Main.{main7}
def idris_Main_46__123_main7_125_(in7):
  while True:
    return (65659, # {U_io_bind1}
             None,
             None,
             None,
             idris_Python_46__46__36_(
               None,
               None,
               idris_Python_46__46__47_(None, None, in7, "select", None),
               (1, "div.entry-content li", (0,))
             ),
             (65645,)
           )

# Main.{main8}
def idris_Main_46__123_main8_125_(in4, in6):
  while True:
    return (65659, # {U_io_bind1}
             None,
             None,
             None,
             idris_Python_46__46__36_(
               None,
               None,
               idris_Python_46__46__47_(None, None, in6, "BeautifulSoup", None),
               (1, in4, (0,))
             ),
             (65646,)
           )

# Main.{main9}
def idris_Main_46__123_main9_125_(in4):
  while True:
    return (65659, # {U_io_bind1}
             None,
             None,
             None,
             idris_Python_46_import_95_(None, (65640,)),
             (65647, in4)
           )

# Main.{main10}
def idris_Main_46__123_main10_125_(in3):
  while True:
    return (65659, # {U_io_bind1}
             None,
             None,
             None,
             idris_Python_46__46__47_(None, None, in3, "text", None),
             (65648,)
           )

# Main.{main11}
def idris_Main_46__123_main11_125_(in2):
  while True:
    return (65659, # {U_io_bind1}
             None,
             None,
             None,
             idris_Python_46__46__36_(
               None,
               None,
               idris_Python_46__46__47_(None, None, in2, "get", None),
               (1, "http://idris-lang.org", (0,))
             ),
             (65637,)
           )

# Main.{main12}
def idris_Main_46__123_main12_125_(in1):
  while True:
    return (65659, # {U_io_bind1}
             None,
             None,
             None,
             idris_Python_46__46__36_(
               None,
               None,
               idris_Python_46__46__47_(None, None, in1, "Session", None),
               (0,)
             ),
             (65638,)
           )

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
    return (65659, None, None, None, e4, (65649, e3))

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
