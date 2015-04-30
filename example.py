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
    return (idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0)(
      None,
      None,
      None,
      (idris_Python_46_FFI_46_unRaw)(None),
      (idris_Python_46__123__36__46_0_125_)(e2, idris_args)
    )

# Python.{$.0}
def idris_Python_46__123__36__46_0_125_(e2, idris_args, in0):
  while True:
    return (idris_call)(e2, idris_args)

# Python.$:
def idris_Python_46__36__58_(e0, e1, e2, idris_args):
  while True:
    return (idris_io_95_bind)(
      None,
      None,
      None,
      e2,
      (idris_Python_46__123__36__58_0_125_)(idris_args)
    )

# Python.{$:0}
def idris_Python_46__123__36__58_0_125_(idris_args, in0):
  while True:
    return (idris_Python_46__36__46_)(None, None, in0, idris_args)

# Prelude.Basics..
def idris_Prelude_46_Basics_46__46_(e0, e1, e2, e3, e4, idris_x):
  while True:
    return (e3)((e4)(idris_x))

# Python./.
def idris_Python_46__47__46_(e0, e1, e2, e3, e4):
  while True:
    return (idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0)(
      None,
      None,
      None,
      (idris_Python_46_FFI_46_unRaw)(None),
      (idris_Python_46__123__47__46_0_125_)(e2, e3)
    )

# Python.{/.0}
def idris_Python_46__123__47__46_0_125_(e2, e3, in0):
  while True:
    return (idris_getfield)(e2, e3)

# Python./:
def idris_Python_46__47__58_(e0, e1, e2, e3, e4):
  while True:
    return (idris_io_95_bind)(
      None,
      None,
      None,
      e2,
      (idris_Python_46__123__47__58_0_125_)(e3)
    )

# Python.{/:0}
def idris_Python_46__123__47__58_0_125_(e3, in0):
  while True:
    return (idris_Python_46__47__46_)(None, None, in0, e3, None)

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

# call__IO
def idris_call_95__95_IO(e0, e1, e2):
  while True:
    return (e2)(None)

# Python.collect
def idris_Python_46_collect(e0, e1):
  while True:
    return (idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0)(
      None,
      None,
      None,
      (idris_Prelude_46_List_46_reverse_58_reverse_39__58_0)(None, (0,)),  # Prelude.List.Nil
      (idris_Python_46_foreach)(
        None,
        None,
        e1,
        (0,),  # Prelude.List.Nil
        (idris_Python_46__123_collect1_125_)()
      )
    )

# Python.{collect1}
def idris_Python_46__123_collect1_125_(in0):
  while True:
    return (idris_Python_46__123_collect0_125_)(in0)

# Python.{collect0}
def idris_Python_46__123_collect0_125_(in0, in1):
  while True:
    return (idris_io_95_return)(None, None, (1, in1, in0))  # Prelude.List.::

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
    return (idris_Prelude_46_Foldable_46_foldr)(None, None, None, e2, aux1, aux2)

# Prelude.Foldable.foldr
def idris_Prelude_46_Foldable_46_foldr(e0, e1, e2, e3):
  while True:
    return (e3)(e1, e2)

# Python.foreach
def idris_Python_46_foreach(e0, e1, e2, e3, e4):
  while True:
    return (idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0)(
      None,
      None,
      None,
      (idris_Python_46_FFI_46_unRaw)(None),
      (idris_Python_46__123_foreach0_125_)(e2, e3, e4)
    )

# Python.{foreach0}
def idris_Python_46__123_foreach0_125_(e2, e3, e4, in0):
  while True:
    return (idris_foreach)(e2, e3, e4)

# Python.importModule
def idris_Python_46_importModule(e0, e1, idris_w):
  while True:
    return (idris_pymodule)(e1)

# Python.BeautifulSoup.import_
def idris_Python_46_BeautifulSoup_46_import_95_():
  while True:
    return (idris_Python_46_importModule)(None, "bs4")

# Python.Requests.import_
def idris_Python_46_Requests_46_import_95_():
  while True:
    return (idris_Python_46_importModule)(None, "requests")

# io_bind
def idris_io_95_bind(e0, e1, e2, e3, e4, idris_w):
  while True:
    return (io_bind2)(e0, e1, e2, e3, e4, idris_w, (e3)(idris_w))

# {io_bind2}
def io_bind2(e0, e1, e2, e3, e4, idris_w):
  while True:
    return (io_bind1)(e0, e1, e2, e3, e4, idris_w)

# {io_bind1}
def io_bind1(e0, e1, e2, e3, e4, idris_w, in0):
  while True:
    return (io_bind0)(e0, e1, e2, e3, e4, idris_w, in0, idris_w)

# {io_bind0}
def io_bind0(e0, e1, e2, e3, e4, idris_w, in0):
  while True:
    return (e4)(in0)

# io_return
def idris_io_95_return(e0, e1, e2, idris_w):
  while True:
    return e2

# Main.main
def idris_Main_46_main():
  while True:
    return (idris_io_95_bind)(
      None,
      None,
      None,
      idris_Python_46_Requests_46_import_95_,
      (idris_Main_46__123_main19_125_)()
    )

# Main.{main19}
def idris_Main_46__123_main19_125_(in0):
  while True:
    return (idris_io_95_bind)(
      None,
      None,
      None,
      (idris_Python_46__36__58_)(
        None,
        None,
        (idris_Python_46__47__46_)(None, None, in0, "Session", None),
        (0,)  # Python.Nil
      ),
      (idris_Main_46__123_main18_125_)()
    )

# Main.{main18}
def idris_Main_46__123_main18_125_(in1):
  while True:
    return (idris_io_95_bind)(
      None,
      None,
      None,
      (idris_Python_46__47__58_)(
        None,
        None,
        (idris_Python_46__36__58_)(
          None,
          None,
          (idris_Python_46__47__46_)(None, None, in1, "get", None),
          (1, "http://idris-lang.org", (0,))  # Python.::, Python.Nil
        ),
        "text",
        None
      ),
      (idris_Main_46__123_main17_125_)()
    )

# Main.{main17}
def idris_Main_46__123_main17_125_(in2):
  while True:
    return (idris_io_95_bind)(
      None,
      None,
      None,
      idris_Python_46_BeautifulSoup_46_import_95_,
      (idris_Main_46__123_main16_125_)(in2)
    )

# Main.{main16}
def idris_Main_46__123_main16_125_(in2, in3):
  while True:
    return (idris_io_95_bind)(
      None,
      None,
      None,
      (idris_Python_46__36__58_)(
        None,
        None,
        (idris_Python_46__47__46_)(None, None, in3, "BeautifulSoup", None),
        (1, in2, (0,))  # Python.::, Python.Nil
      ),
      (idris_Main_46__123_main15_125_)()
    )

# Main.{main15}
def idris_Main_46__123_main15_125_(in4):
  while True:
    return (idris_io_95_bind)(
      None,
      None,
      None,
      (idris_Python_46__36__58_)(
        None,
        None,
        (idris_Python_46__47__46_)(None, None, in4, "select", None),
        (1, "div.entry-content li", (0,))  # Python.::, Python.Nil
      ),
      (idris_Main_46__123_main14_125_)()
    )

# Main.{main14}
def idris_Main_46__123_main14_125_(in5):
  while True:
    return (idris_io_95_bind)(
      None,
      None,
      None,
      (idris_Prelude_46_putStr)(
        None,
        "Idris has got the following exciting features:\n"
      ),
      (idris_Main_46__123_main13_125_)(in5)
    )

# Main.{main13}
def idris_Main_46__123_main13_125_(in5, in6):
  while True:
    return (idris_io_95_bind)(
      None,
      None,
      None,
      (idris_Python_46_foreach)(None, None, in5, 0, (idris_Main_46__123_main11_125_)()),
      (idris_Main_46__123_main12_125_)()
    )

# Main.{main12}
def idris_Main_46__123_main12_125_(in19):
  while True:
    return (idris_Prelude_46_putStr)(None, "Total number of features: " + str(in19) + "\n")

# Main.{main11}
def idris_Main_46__123_main11_125_(in7):
  while True:
    return (idris_Main_46__123_main10_125_)(in7)

# Main.{main10}
def idris_Main_46__123_main10_125_(in7, in8):
  while True:
    return (idris_io_95_bind)(
      None,
      None,
      None,
      (idris_io_95_bind)(
        None,
        None,
        None,
        (idris_Python_46__47__46_)(None, None, in8, "strings", None),
        (idris_Prelude_46_Basics_46__46_)(
          None,
          None,
          None,
          (idris_Main_46__123_main7_125_)(),
          (idris_Python_46_collect)(None)
        )
      ),
      (idris_Main_46__123_main9_125_)(in7)
    )

# Main.{main9}
def idris_Main_46__123_main9_125_(in7, in17):
  while True:
    return (idris_io_95_bind)(
      None,
      None,
      None,
      (idris_Prelude_46_putStr)(None, str(in7 + 1) + ". " + in17 + "\n"),
      (idris_Main_46__123_main8_125_)(in7)
    )

# Main.{main8}
def idris_Main_46__123_main8_125_(in7, in18):
  while True:
    return (idris_io_95_return)(None, None, in7 + 1)

# Main.{main7}
def idris_Main_46__123_main7_125_(in9):
  while True:
    return (idris_Prelude_46_Functor_46_Prelude_46__64_Prelude_46_Functor_46_Functor_36_IO_39__32_ffi_58__33_map_58_0)(
      None,
      None,
      None,
      (idris_Prelude_46_Foldable_46_concat)(
        None,
        None,
        (idris_Main_46__123_main4_125_)(),
        (0, (idris_Main_46__123_main6_125_)(), "")  # constructor of Prelude.Algebra.Monoid
      ),
      in9
    )

# Main.{main6}
def idris_Main_46__123_main6_125_(in15):
  while True:
    return (idris_Main_46__123_main5_125_)(in15)

# Main.{main5}
def idris_Main_46__123_main5_125_(in15, in16):
  while True:
    return in15 + in16

# Main.{main4}
def idris_Main_46__123_main4_125_(in10):
  while True:
    return (idris_Main_46__123_main3_125_)()

# Main.{main3}
def idris_Main_46__123_main3_125_(in11):
  while True:
    return (idris_Main_46__123_main2_125_)()

# Main.{main2}
def idris_Main_46__123_main2_125_(in12):
  while True:
    return (idris_Main_46__123_main1_125_)(in12)

# Main.{main1}
def idris_Main_46__123_main1_125_(in12, in13):
  while True:
    return (idris_Main_46__123_main0_125_)(in12, in13)

# Main.{main0}
def idris_Main_46__123_main0_125_(in12, in13, in14):
  while True:
    return (idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0)(
      None,
      None,
      in12,
      in13,
      in14
    )

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
    return (e3)(e2)

# Prelude.putStr
def idris_Prelude_46_putStr(e0, e1):
  while True:
    return (idris_io_95_bind)(
      None,
      None,
      None,
      (idris_Prelude_46__123_putStr0_125_)(e1),
      (idris_Prelude_46__123_putStr1_125_)()
    )

# Prelude.{putStr1}
def idris_Prelude_46__123_putStr1_125_(in1):
  while True:
    return (idris_io_95_return)(None, None, (0,))  # MkUnit

# Prelude.{putStr0}
def idris_Prelude_46__123_putStr0_125_(e1, in0):
  while True:
    return sys.stdout.write(e1)

# run__IO
def idris_run_95__95_IO(e0, e1):
  while True:
    return (e1)(None)

# Python.FFI.unRaw
def idris_Python_46_FFI_46_unRaw(e0, e1):
  while True:
    return e1

# unsafePerformPrimIO
def idris_unsafePerformPrimIO():
  while True:
    return None

# world
def idris_world(e0):
  while True:
    return e0

# Prelude.List.reverse, reverse'
def idris_Prelude_46_List_46_reverse_58_reverse_39__58_0(e0, e1, e2):
  while True:
    if e2[0] == 1:  # Prelude.List.::
      in0, in1, = e2[1:]
      aux1 = (idris_Prelude_46_List_46_reverse_58_reverse_39__58_0)(None, (1, in0, e1), in1)  # Prelude.List.::
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
      aux1 = (e2)(
        in0,
        (idris_Prelude_46_Foldable_46_foldr)(
          None,
          None,
          None,
          idris_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List,
          e2,
          e3,
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
  whil