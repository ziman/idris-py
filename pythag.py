#!/usr/bin/env python

import sys

class IdrisError(Exception):
  pass

def idris_error(msg):
  raise IdrisError(msg)

# Prelude.List.++
def idris_Prelude_46_List_46__43__43_(e0, e1, e2):
  if e1[0] == 1: # Prelude.List.::
    in0, in1, = e1[1:]
    aux1 = (1, # Prelude.List.::
             in0,
             idris_Prelude_46_List_46__43__43_(
               None,
               in1,
               e2
             )
           )
  elif e1[0] == 0: # Prelude.List.Nil
    aux1 = e2
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Basics..
def idris_Prelude_46_Basics_46__46_(e0, e1, e2, e3, e4, idris_x):
  return APPLY0(
           e3,
           APPLY0(
             e4,
             idris_x
           )
         )

# Prelude.Classes.<
def idris_Prelude_46_Classes_46__60_(e0, e1):
  if e1[0] == 0: # constructor of Prelude.Classes.Ord
    in0, in1, = e1[1:]
    aux1 = in1
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Algebra.<+>
def idris_Prelude_46_Algebra_46__60__43__62_(e0, e1):
  return e1

# @@constructor of Prelude.Algebra.Monoid#Semigroup a
def idris__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a(e0, e1):
  if e1[0] == 0: # constructor of Prelude.Algebra.Monoid
    in0, in1, = e1[1:]
    aux1 = in0
  else:
    idris_error("unreachable case")
  return aux1

# @@constructor of Prelude.Applicative.Alternative#Applicative f
def idris__64__64_constructor_32_of_32_Prelude_46_Applicative_46_Alternative_35_Applicative_32_f(e0, e1):
  if e1[0] == 0: # constructor of Prelude.Applicative.Alternative
    in0, in1, = e1[1:]
    aux1 = in0
  else:
    idris_error("unreachable case")
  return aux1

# Force
def idris_Force(e0, e1, e2):
  in0 = EVAL0(
          e2
        )
  return in0

# PE_List a instance of Prelude.Show_f5d3ac2c
def idris_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c(meth0):
  return idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0(
           None,
           None,
           (65652,),
           meth0
         )

# Prelude.Bool.boolElim
def idris_Prelude_46_Bool_46_boolElim(e0, e1, e2, e3):
  if e1[0] == 0: # Prelude.Bool.False
    aux1 = EVAL0(
             e3
           )
  elif e1[0] == 1: # Prelude.Bool.True
    aux1 = EVAL0(
             e2
           )
  else:
    idris_error("unreachable case")
  return aux1

# call__IO
def idris_call_95__95_IO(e0, e1, e2):
  return APPLY0(
           e2,
           None
         )

# Prelude.Classes.compare
def idris_Prelude_46_Classes_46_compare(e0, e1):
  if e1[0] == 0: # constructor of Prelude.Classes.Ord
    in0, in1, = e1[1:]
    aux1 = in0
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Foldable.concatMap
def idris_Prelude_46_Foldable_46_concatMap(e0, e1, e2, e3, e4, e5):
  if e4[0] == 0: # constructor of Prelude.Algebra.Monoid
    in0, in1, = e4[1:]
    aux1 = in0
  else:
    idris_error("unreachable case")
  if e4[0] == 0: # constructor of Prelude.Algebra.Monoid
    in2, in3, = e4[1:]
    aux2 = in3
  else:
    idris_error("unreachable case")
  return APPLY0(
           APPLY0(
             idris_Prelude_46_Foldable_46_foldr(
               None,
               None,
               None,
               e3
             ),
             (65631, # {U_Prelude.Basics..1}
               None,
               None,
               None,
               aux1,
               e5
             )
           ),
           aux2
         )

# Prelude.Applicative.empty
def idris_Prelude_46_Applicative_46_empty(e0, e1, e2):
  if e2[0] == 0: # constructor of Prelude.Applicative.Alternative
    in0, in1, = e2[1:]
    aux1 = APPLY0(
             in1,
             e1
           )
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Foldable.foldr
def idris_Prelude_46_Foldable_46_foldr(e0, e1, e2, e3):
  return APPLY0(
           APPLY0(
             e3,
             e1
           ),
           e2
         )

# Prelude.List.foldrImpl
def idris_Prelude_46_List_46_foldrImpl(e0, e1, e2, e3, e4, e5):
  if e5[0] == 1: # Prelude.List.::
    in0, in1, = e5[1:]
    aux1 = idris_Prelude_46_List_46_foldrImpl(
             None,
             None,
             e2,
             e3,
             (65631, # {U_Prelude.Basics..1}
               None,
               None,
               None,
               e4,
               APPLY0(
                 e2,
                 in0
               )
             ),
             in1
           )
  elif e5[0] == 0: # Prelude.List.Nil
    aux1 = APPLY0(
             e4,
             e3
           )
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Applicative.guard
def idris_Prelude_46_Applicative_46_guard(e0, e1, e2):
  if e2[0] == 0: # Prelude.Bool.False
    if e1[0] == 0: # constructor of Prelude.Applicative.Alternative
      in0, in1, = e1[1:]
      aux2 = APPLY0(
               in1,
               None
             )
    else:
      idris_error("unreachable case")
    aux1 = aux2
  elif e2[0] == 1: # Prelude.Bool.True
    if e1[0] == 0: # constructor of Prelude.Applicative.Alternative
      in2, in3, = e1[1:]
      aux3 = in2
    else:
      idris_error("unreachable case")
    aux1 = APPLY0(
             idris_Prelude_46_Applicative_46_pure(
               None,
               None,
               aux3
             ),
             (0,)
           )
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Basics.id
def idris_Prelude_46_Basics_46_id(e0, e1):
  return e1

# Prelude.Classes.intToBool
def idris_Prelude_46_Classes_46_intToBool(e0):
  if e0 == 0:
    aux1 = (0,)
  else:
    aux1 = (1,)
  return aux1

# io_bind
def idris_io_95_bind(e0, e1, e2, e3, e4, idris_w):
  return APPLY0(
           io_bind2(
             e0,
             e1,
             e2,
             e3,
             e4,
             idris_w
           ),
           APPLY0(
             e3,
             idris_w
           )
         )

# io_return
def idris_io_95_return(e0, e1, e2, idris_w):
  return e2

# Main.main
def idris_Main_46_main():
  return (65646, # {U_io_bind1}
           None,
           None,
           None,
           idris_Prelude_46_putStr(
             None,
             APPLY0(
               idris_Prelude_46_show(
                 None,
                 (65630,)
               ),
               idris_Main_46_pythag(
                 10
               )
             )
           ),
           (65622,)
         )

# mkForeignPrim
def idris_mkForeignPrim():
  return None

# Prelude.Algebra.neutral
def idris_Prelude_46_Algebra_46_neutral(e0, e1):
  if e1[0] == 0: # constructor of Prelude.Algebra.Monoid
    in0, in1, = e1[1:]
    aux1 = in1
  else:
    idris_error("unreachable case")
  return aux1

# prim__addInt
def idris_prim_95__95_addInt(op0, op1):
  return op0 + op1

# prim__concat
def idris_prim_95__95_concat(op0, op1):
  return op0 + op1

# prim__eqInt
def idris_prim_95__95_eqInt(op0, op1):
  return op0 == op1

# prim__mulInt
def idris_prim_95__95_mulInt(op0, op1):
  return op0 * op1

# prim__null
def idris_prim_95__95_null():
  return None

# prim__readFile
def idris_prim_95__95_readFile(op0, op1):
  return idris_error("unimplemented external: prim__readFile")

# prim__registerPtr
def idris_prim_95__95_registerPtr(op0, op1):
  return idris_error("unimplemented external: prim__registerPtr")

# prim__sextInt_BigInt
def idris_prim_95__95_sextInt_95_BigInt(op0):
  return op0

# prim__sltInt
def idris_prim_95__95_sltInt(op0, op1):
  return op0 < op1

# prim__stderr
def idris_prim_95__95_stderr():
  return idris_error("unimplemented external: prim__stderr")

# prim__stdin
def idris_prim_95__95_stdin():
  return idris_error("unimplemented external: prim__stdin")

# prim__stdout
def idris_prim_95__95_stdout():
  return idris_error("unimplemented external: prim__stdout")

# prim__subInt
def idris_prim_95__95_subInt(op0, op1):
  return op0 - op1

# prim__toStrInt
def idris_prim_95__95_toStrInt(op0):
  return str(op0)

# prim__vm
def idris_prim_95__95_vm():
  return idris_error("unimplemented external: prim__vm")

# prim__writeFile
def idris_prim_95__95_writeFile(op0, op1, op2):
  return idris_error("unimplemented external: prim__writeFile")

# prim__writeString
def idris_prim_95__95_writeString(op0, op1):
  return sys.stdout.write(op1)

# prim_io_bind
def idris_prim_95_io_95_bind(e0, e1, e2, e3):
  return APPLY0(
           e3,
           e2
         )

# Prelude.Applicative.pure
def idris_Prelude_46_Applicative_46_pure(e0, e1, e2):
  return APPLY0(
           e2,
           e1
         )

# Prelude.putStr
def idris_Prelude_46_putStr(e0, e1):
  return (65646, # {U_io_bind1}
           None,
           None,
           None,
           (65644, # {U_Prelude.{putStr0}1}
             e1
           ),
           (65645,)
         )

# Main.pythag
def idris_Main_46_pythag(e0):
  return idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
           None,
           None,
           idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(
             1,
             e0
           ),
           (65629,)
         )

# run__IO
def idris_run_95__95_IO(e0, e1):
  return APPLY0(
           e1,
           None
         )

# Prelude.show
def idris_Prelude_46_show(e0, e1):
  return e1

# unsafePerformPrimIO
def idris_unsafePerformPrimIO():
  return None

# world
def idris_world(e0):
  return e0

# Prelude.Bool.||
def idris_Prelude_46_Bool_46__124__124_(e0, e1):
  if e0[0] == 0: # Prelude.Bool.False
    aux1 = EVAL0(
             e1
           )
  elif e0[0] == 1: # Prelude.Bool.True
    aux1 = (1,)
  else:
    idris_error("unreachable case")
  return aux1

# {APPLY0}
def APPLY0(fn0, arg0):
  if fn0[0] == 65622: # {U_Main.{main0}1}
    aux1 = idris_Main_46__123_main0_125_(
             arg0
           )
  elif fn0[0] == 65623: # {U_Main.{pythag0}1}
    aux1 = idris_Main_46__123_pythag0_125_(
             arg0
           )
  elif fn0[0] == 65624: # {U_Main.{pythag1}1}
    aux1 = idris_Main_46__123_pythag1_125_(
             arg0
           )
  elif fn0[0] == 65625: # {U_Main.{pythag2}1}
    aux1 = idris_Main_46__123_pythag2_125_(
             arg0
           )
  elif fn0[0] == 65626: # {U_Main.{pythag3}1}
    P_c0, P_c1, P_c2, = fn0[1:]
    aux1 = idris_Main_46__123_pythag3_125_(
             P_c0,
             P_c1,
             P_c2,
             arg0
           )
  elif fn0[0] == 65627: # {U_Main.{pythag4}1}
    P_c0, P_c1, = fn0[1:]
    aux1 = idris_Main_46__123_pythag4_125_(
             P_c0,
             P_c1,
             arg0
           )
  elif fn0[0] == 65628: # {U_Main.{pythag5}1}
    P_c0, = fn0[1:]
    aux1 = idris_Main_46__123_pythag5_125_(
             P_c0,
             arg0
           )
  elif fn0[0] == 65629: # {U_Main.{pythag6}1}
    aux1 = idris_Main_46__123_pythag6_125_(
             arg0
           )
  elif fn0[0] == 65630: # {U_PE_List a instance of Prelude.Show_f5d3ac2c1}
    aux1 = idris_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c(
             arg0
           )
  elif fn0[0] == 65631: # {U_Prelude.Basics..1}
    P_c0, P_c1, P_c2, P_c3, P_c4, = fn0[1:]
    aux1 = idris_Prelude_46_Basics_46__46_(
             P_c0,
             P_c1,
             P_c2,
             P_c3,
             P_c4,
             arg0
           )
  elif fn0[0] == 65632: # {U_Prelude.Basics.id1}
    P_c0, = fn0[1:]
    aux1 = idris_Prelude_46_Basics_46_id(
             P_c0,
             arg0
           )
  elif fn0[0] == 65633: # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord0}1}
    P_c0, = fn0[1:]
    aux1 = idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord0_125_(
             P_c0,
             arg0
           )
  elif fn0[0] == 65634: # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord1}1}
    aux1 = idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord1_125_(
             arg0
           )
  elif fn0[0] == 65635: # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord2}1}
    P_c0, = fn0[1:]
    aux1 = idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord2_125_(
             P_c0,
             arg0
           )
  elif fn0[0] == 65636: # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord3}1}
    aux1 = idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord3_125_(
             arg0
           )
  elif fn0[0] == 65637: # {U_Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=0}1}
    P_c0, P_c1, = fn0[1:]
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_0_125_(
             P_c0,
             P_c1,
             arg0
           )
  elif fn0[0] == 65638: # {U_Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=1}1}
    P_c0, = fn0[1:]
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_1_125_(
             P_c0,
             arg0
           )
  elif fn0[0] == 65639: # {U_Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=2}1}
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_2_125_(
             arg0
           )
  elif fn0[0] == 65640: # {U_Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=3}1}
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_3_125_(
             arg0
           )
  elif fn0[0] == 65641: # {U_Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=4}1}
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_4_125_(
             arg0
           )
  elif fn0[0] == 65642: # {U_Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=5}1}
    P_c0, = fn0[1:]
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_5_125_(
             P_c0,
             arg0
           )
  elif fn0[0] == 65643: # {U_Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=6}1}
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_6_125_(
             arg0
           )
  elif fn0[0] == 65644: # {U_Prelude.{putStr0}1}
    P_c0, = fn0[1:]
    aux1 = idris_Prelude_46__123_putStr0_125_(
             P_c0,
             arg0
           )
  elif fn0[0] == 65645: # {U_Prelude.{putStr1}1}
    aux1 = idris_Prelude_46__123_putStr1_125_(
             arg0
           )
  elif fn0[0] == 65646: # {U_io_bind1}
    P_c0, P_c1, P_c2, P_c3, P_c4, = fn0[1:]
    aux1 = idris_io_95_bind(
             P_c0,
             P_c1,
             P_c2,
             P_c3,
             P_c4,
             arg0
           )
  elif fn0[0] == 65647: # {U_io_return1}
    P_c0, P_c1, P_c2, = fn0[1:]
    aux1 = idris_io_95_return(
             P_c0,
             P_c1,
             P_c2,
             arg0
           )
  elif fn0[0] == 65648: # {U_{PE_List a instance of Prelude.Show_f5d3ac2c0}1}
    aux1 = idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c0_125_(
             arg0
           )
  elif fn0[0] == 65649: # {U_{PE_List a instance of Prelude.Show_f5d3ac2c1}1}
    aux1 = idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c1_125_(
             arg0
           )
  elif fn0[0] == 65650: # {U_{PE_List a instance of Prelude.Show_f5d3ac2c2}1}
    aux1 = idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c2_125_(
             arg0
           )
  elif fn0[0] == 65651: # {U_{PE_List a instance of Prelude.Show_f5d3ac2c3}1}
    aux1 = idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c3_125_(
             arg0
           )
  elif fn0[0] == 65652: # {U_{PE_List a instance of Prelude.Show_f5d3ac2c4}1}
    aux1 = idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c4_125_(
             arg0
           )
  elif fn0[0] == 65653: # {U_{io_bind1}1}
    P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, = fn0[1:]
    aux1 = io_bind1(
             P_c0,
             P_c1,
             P_c2,
             P_c3,
             P_c4,
             P_c5,
             arg0
           )
  else:
    aux1 = None
  return aux1

# {EVAL0}
def EVAL0(arg0):
  return arg0

# Prelude.Classes.{Int instance of Prelude.Classes.Ord0}
def idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord0_125_(in0, in1):
  return idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33_compare_58_0(
           in0,
           in1
         )

# {PE_List a instance of Prelude.Show_f5d3ac2c0}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c0_125_(in1):
  return str(in1)

# Prelude.Classes.{Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=0}
def idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__60__61_0_125_(e0, e1):
  aux1 = e0 == e1
  if aux1 == 0:
    aux2 = (0,)
  else:
    aux2 = (1,)
  return aux2

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=0}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_0_125_(in2, in3, in4):
  return idris_Prelude_46_List_46_foldrImpl(
           None,
           None,
           in2,
           in3,
           (65632, # {U_Prelude.Basics.id1}
             None
           ),
           in4
         )

# {io_bind0}
def io_bind0(e0, e1, e2, e3, e4, idris_w, in0):
  return APPLY0(
           e4,
           in0
         )

# Main.{main0}
def idris_Main_46__123_main0_125_(in0):
  return idris_Prelude_46_putStr(
           None,
           "\n"
         )

# Prelude.{putStr0}
def idris_Prelude_46__123_putStr0_125_(e1, in0):
  return sys.stdout.write(e1)

# Main.{pythag0}
def idris_Main_46__123_pythag0_125_(in4):
  return (1, # Prelude.List.::
           in4,
           (0,)
         )

# {runMain0}
def runMain0():
  return EVAL0(
           APPLY0(
             idris_Main_46_main(
             ),
             None
           )
         )

# Prelude.Classes.{Int instance of Prelude.Classes.Ord1}
def idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord1_125_(in0):
  return (65633, # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord0}1}
           in0
         )

# {PE_List a instance of Prelude.Show_f5d3ac2c1}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c1_125_(in3):
  return str(in3)

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=1}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_1_125_(in2, in3):
  return (65637, # {U_Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=0}1}
           in2,
           in3
         )

# {io_bind1}
def io_bind1(e0, e1, e2, e3, e4, idris_w, in0):
  return APPLY0(
           io_bind0(
             e0,
             e1,
             e2,
             e3,
             e4,
             idris_w,
             in0
           ),
           idris_w
         )

# Prelude.{putStr1}
def idris_Prelude_46__123_putStr1_125_(in1):
  return (65647, # {U_io_return1}
           None,
           None,
           (0,)
         )

# Main.{pythag1}
def idris_Main_46__123_pythag1_125_(in3):
  return (65623,)

# Prelude.Classes.{Int instance of Prelude.Classes.Ord2}
def idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord2_125_(in2, in3):
  aux1 = APPLY0(
           APPLY0(
             idris_Prelude_46_Classes_46_compare(
               None,
               idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int(
               )
             ),
             in2
           ),
           in3
         )
  if aux1[0] == 0: # Prelude.Classes.LT
    aux2 = (1,)
  else:
    aux2 = (0,)
  return aux2

# {PE_List a instance of Prelude.Show_f5d3ac2c2}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c2_125_(in4):
  return str(in4)

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=2}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_2_125_(in2):
  return (65638, # {U_Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=1}1}
           in2
         )

# {io_bind2}
def io_bind2(e0, e1, e2, e3, e4, idris_w):
  return (65653, # {U_{io_bind1}1}
           e0,
           e1,
           e2,
           e3,
           e4,
           idris_w
         )

# Main.{pythag2}
def idris_Main_46__123_pythag2_125_(in5):
  return (0,)

# Prelude.Classes.{Int instance of Prelude.Classes.Ord3}
def idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord3_125_(in2):
  return (65635, # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord2}1}
           in2
         )

# {PE_List a instance of Prelude.Show_f5d3ac2c3}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c3_125_(in2):
  return idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
           None,
           None,
           None,
           None,
           (65649,),
           (65650,),
           in2
         )

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=3}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_3_125_(in1):
  return (65639,)

# Main.{pythag3}
def idris_Main_46__123_pythag3_125_(in2, in1, in0, in6):
  return (1, # Prelude.List.::
           (0, # Builtins.MkPair
             in2,
             (0, # Builtins.MkPair
               in1,
               in0
             )
           ),
           (0,)
         )

# {PE_List a instance of Prelude.Show_f5d3ac2c4}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c4_125_(in0):
  return idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
           None,
           None,
           None,
           None,
           (65648,),
           (65651,),
           in0
         )

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=4}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_4_125_(in0):
  return (65640,)

# Main.{pythag4}
def idris_Main_46__123_pythag4_125_(in1, in0, in2):
  aux1 = in2 * in2 + in1 * in1 == in0 * in0
  if aux1 == 0:
    aux2 = (0,)
  else:
    aux2 = (1,)
  return idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
           None,
           None,
           idris_Prelude_46_Applicative_46_guard(
             None,
             (0, # constructor of Prelude.Applicative.Alternative
               (65624,),
               (65625,)
             ),
             aux2
           ),
           (65626, # {U_Main.{pythag3}1}
             in2,
             in1,
             in0
           )
         )

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=5}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_5_125_(in5, in6):
  return idris_Prelude_46_List_46__43__43_(
           None,
           in5,
           in6
         )

# Main.{pythag5}
def idris_Main_46__123_pythag5_125_(in0, in1):
  return idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
           None,
           None,
           idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(
             1,
             in1
           ),
           (65627, # {U_Main.{pythag4}1}
             in1,
             in0
           )
         )

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=6}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_6_125_(in5):
  return (65642, # {U_Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=5}1}
           in5
         )

# Main.{pythag6}
def idris_Main_46__123_pythag6_125_(in0):
  return idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
           None,
           None,
           idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(
             1,
             in0
           ),
           (65628, # {U_Main.{pythag5}1}
             in0
           )
         )

# Decidable.Equality.Decidable.Equality.Char instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Char_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  return None

# Decidable.Equality.Decidable.Equality.Int instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Int_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  return None

# Decidable.Equality.Decidable.Equality.Integer instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Integer_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  return None

# Decidable.Equality.Decidable.Equality.String instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_String_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  return None

# Prelude.Prelude.Int instance of Prelude.Enum, method enumFromTo, go
def idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0_58_go_58_0(e0, e1, e2, e3, e4):
  if e3 == 0:
    aux1 = (1, # Prelude.List.::
             e4,
             e2
           )
  else:
    in0 = e3 - 1
    aux1 = idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0_58_go_58_0(
             None,
             None,
             (1, # Prelude.List.::
               e4,
               e2
             ),
             in0,
             e4 - 1
           )
  return aux1

# Prelude.Prelude.List a instance of Prelude.Show, method show, show'
def idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(e0, e1, e2, e3, e4, e5):
  if e5[0] == 1: # Prelude.List.::
    in0, in1, = e5[1:]
    if in1[0] == 0: # Prelude.List.Nil
      aux2 = e4 + APPLY0(
                    idris_Prelude_46_show(
                      None,
                      e3
                    ),
                    in0
                  )
    else:
      aux2 = idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
               None,
               None,
               None,
               e3,
               e4 + APPLY0(
                      idris_Prelude_46_show(
                        None,
                        e3
                      ),
                      in0
                    ) + ", ",
               in1
             )
    aux1 = aux2
  elif e5[0] == 0: # Prelude.List.Nil
    aux1 = e4
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Prelude.Int instance of Prelude.Enum, method enumFromTo
def idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(e0, e1):
  aux1 = idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__61__58_0(
           e0,
           e1
         )
  if aux1[0] == 0: # Prelude.Bool.False
    aux2 = (0,)
  elif aux1[0] == 1: # Prelude.Bool.True
    aux2 = idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0_58_go_58_0(
             None,
             None,
             (0,),
             e1 - e0,
             e1
           )
  else:
    idris_error("unreachable case")
  return aux2

# Prelude.Monad.Prelude.List instance of Prelude.Monad.Monad, method >>=
def idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(e0, e1, e2, e3):
  return APPLY0(
           idris_Prelude_46_Foldable_46_concatMap(
             None,
             None,
             None,
             (65641,),
             (0, # constructor of Prelude.Algebra.Monoid
               (65643,),
               (0,)
             ),
             e3
           ),
           e2
         )

# Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=
def idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__61__58_0(e0, e1):
  aux1 = APPLY0(
           APPLY0(
             idris_Prelude_46_Classes_46__60_(
               None,
               idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int(
               )
             ),
             e0
           ),
           e1
         )
  if aux1[0] == 0: # Prelude.Bool.False
    aux2 = idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__60__61_0_125_(
             e0,
             e1
           )
  elif aux1[0] == 1: # Prelude.Bool.True
    aux2 = (1,)
  else:
    idris_error("unreachable case")
  return aux2

# Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method compare
def idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33_compare_58_0(e0, e1):
  aux2 = e0 == e1
  if aux2 == 0:
    aux3 = (0,)
  else:
    aux3 = (1,)
  aux1 = aux3
  if aux1[0] == 0: # Prelude.Bool.False
    aux6 = e0 < e1
    if aux6 == 0:
      aux7 = (0,)
    else:
      aux7 = (1,)
    aux5 = aux7
    if aux5[0] == 0: # Prelude.Bool.False
      aux8 = (2,)
    elif aux5[0] == 1: # Prelude.Bool.True
      aux8 = (0,)
    else:
      idris_error("unreachable case")
    aux4 = aux8
  elif aux1[0] == 1: # Prelude.Bool.True
    aux4 = (1,)
  else:
    idris_error("unreachable case")
  return aux4

# Prelude.Prelude.(a, b) instance of Prelude.Show, method show
def idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(e0, e1, e2, e3, e4, e5, e6):
  if e6[0] == 0: # Builtins.MkPair
    in0, in1, = e6[1:]
    aux1 = "(" + APPLY0(
                   idris_Prelude_46_show(
                     None,
                     e4
                   ),
                   in0
                 ) + ", " + APPLY0(
                              idris_Prelude_46_show(
                                None,
                                e5
                              ),
                              in1
                            ) + ")"
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Prelude.List a instance of Prelude.Show, method show
def idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0(e0, e1, e2, e3):
  return "[" + idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
                 None,
                 None,
                 None,
                 e2,
                 "",
                 e3
               ) + "]"

# with block in Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <
def idris__95_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__58_0_95_with_95_85(e0, e1, e2):
  if e0[0] == 0: # Prelude.Classes.LT
    aux1 = (1,)
  else:
    aux1 = (0,)
  return aux1

# Prelude.Classes.Int instance of Prelude.Classes.Ord
def idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int():
  return (0, # constructor of Prelude.Classes.Ord
           (65634,),
           (65636,)
         )

# case block in Void
def idris_Void_95_case():
  return None

# case block in io_bind
def idris_io_95_bind_95_case(e0, e1, e2, e3, e4, e5, e6, e7):
  return APPLY0(
           e7,
           e5
         )

# <<Void eliminator>>
def idris_Void_95_elim():
  return None

if __name__ == '__main__':
  runMain0()