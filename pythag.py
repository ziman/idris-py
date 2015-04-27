#!/usr/bin/env python

import sys

class IdrisError(Exception):
  pass

def idris_error(msg):
  raise IdrisError(msg)

# Prelude.List.++
def idris_Prelude_46_List_46__43__43_(loc0, loc1, loc2):
  if idris__123_e1_125_()[0] == -1:
    aux2, aux3, = idris__123_e1_125_()[1:]
    aux1 = (1, idris__123_in0_125_(), idris_Prelude_46_List_46__43__43_(
                                        None,
                                        idris__123_in1_125_(),
                                        idris__123_e2_125_()
                                      ))
  elif idris__123_e1_125_()[0] == -1:
    aux1 = idris__123_e2_125_()
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Basics..
def idris_Prelude_46_Basics_46__46_(loc0, loc1, loc2, loc3, loc4, loc5):
  return idris__123_APPLY0_125_(
           idris__123_e3_125_(),
           idris__123_APPLY0_125_(
             idris__123_e4_125_(),
             idris_x()
           )
         )

# Prelude.Classes.<
def idris_Prelude_46_Classes_46__60_(loc0, loc1):
  if idris__123_e1_125_()[0] == -1:
    aux2, aux3, = idris__123_e1_125_()[1:]
    aux1 = idris__123_in1_125_()
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Algebra.<+>
def idris_Prelude_46_Algebra_46__60__43__62_(loc0, loc1):
  return idris__123_e1_125_()

# @@constructor of Prelude.Algebra.Monoid#Semigroup a
def idris__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a(loc0, loc1):
  if idris__123_e1_125_()[0] == -1:
    aux2, aux3, = idris__123_e1_125_()[1:]
    aux1 = idris__123_in0_125_()
  else:
    idris_error("unreachable case")
  return aux1

# @@constructor of Prelude.Applicative.Alternative#Applicative f
def idris__64__64_constructor_32_of_32_Prelude_46_Applicative_46_Alternative_35_Applicative_32_f(loc0, loc1):
  if idris__123_e1_125_()[0] == -1:
    aux2, aux3, = idris__123_e1_125_()[1:]
    aux1 = idris__123_in0_125_()
  else:
    idris_error("unreachable case")
  return aux1

# Force
def idris_Force(loc0, loc1, loc2):
  idris__123_in0_125_ = idris__123_EVAL0_125_(
                          idris__123_e2_125_()
                        )
  return idris__123_in0_125_()

# PE_List a instance of Prelude.Show_f5d3ac2c
def idris_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c(loc0):
  return idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0(
           None,
           None,
           idris__123_U_95__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c4_125_1_125_(
           ),
           idris__123_meth0_125_()
         )

# Prelude.Bool.boolElim
def idris_Prelude_46_Bool_46_boolElim(loc0, loc1, loc2, loc3):
  if idris__123_e1_125_()[0] == -1:
    aux1 = idris__123_EVAL0_125_(
             idris__123_e3_125_()
           )
  elif idris__123_e1_125_()[0] == -1:
    aux1 = idris__123_EVAL0_125_(
             idris__123_e2_125_()
           )
  else:
    idris_error("unreachable case")
  return aux1

# call__IO
def idris_call_95__95_IO(loc0, loc1, loc2):
  return idris__123_APPLY0_125_(
           idris__123_e2_125_(),
           None
         )

# Prelude.Classes.compare
def idris_Prelude_46_Classes_46_compare(loc0, loc1):
  if idris__123_e1_125_()[0] == -1:
    aux2, aux3, = idris__123_e1_125_()[1:]
    aux1 = idris__123_in0_125_()
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Foldable.concatMap
def idris_Prelude_46_Foldable_46_concatMap(loc0, loc1, loc2, loc3, loc4, loc5):
  if idris__123_e4_125_()[0] == -1:
    aux2, aux3, = idris__123_e4_125_()[1:]
    aux1 = idris__123_in0_125_()
  else:
    idris_error("unreachable case")
  if idris__123_e4_125_()[0] == -1:
    aux5, aux6, = idris__123_e4_125_()[1:]
    aux4 = idris__123_in3_125_()
  else:
    idris_error("unreachable case")
  return idris__123_APPLY0_125_(
           idris__123_APPLY0_125_(
             idris_Prelude_46_Foldable_46_foldr(
               None,
               None,
               None,
               idris__123_e3_125_()
             ),
             idris__123_U_95_Prelude_46_Basics_46__46_1_125_(
               None,
               None,
               None,
               aux1,
               idris__123_e5_125_()
             )
           ),
           aux4
         )

# Prelude.Applicative.empty
def idris_Prelude_46_Applicative_46_empty(loc0, loc1, loc2):
  if idris__123_e2_125_()[0] == -1:
    aux2, aux3, = idris__123_e2_125_()[1:]
    aux1 = idris__123_APPLY0_125_(
             idris__123_in1_125_(),
             idris__123_e1_125_()
           )
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Foldable.foldr
def idris_Prelude_46_Foldable_46_foldr(loc0, loc1, loc2, loc3):
  return idris__123_APPLY0_125_(
           idris__123_APPLY0_125_(
             idris__123_e3_125_(),
             idris__123_e1_125_()
           ),
           idris__123_e2_125_()
         )

# Prelude.List.foldrImpl
def idris_Prelude_46_List_46_foldrImpl(loc0, loc1, loc2, loc3, loc4, loc5):
  if idris__123_e5_125_()[0] == -1:
    aux2, aux3, = idris__123_e5_125_()[1:]
    aux1 = idris_Prelude_46_List_46_foldrImpl(
             None,
             None,
             idris__123_e2_125_(),
             idris__123_e3_125_(),
             idris__123_U_95_Prelude_46_Basics_46__46_1_125_(
               None,
               None,
               None,
               idris__123_e4_125_(),
               idris__123_APPLY0_125_(
                 idris__123_e2_125_(),
                 idris__123_in0_125_()
               )
             ),
             idris__123_in1_125_()
           )
  elif idris__123_e5_125_()[0] == -1:
    aux1 = idris__123_APPLY0_125_(
             idris__123_e4_125_(),
             idris__123_e3_125_()
           )
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Applicative.guard
def idris_Prelude_46_Applicative_46_guard(loc0, loc1, loc2):
  if idris__123_e2_125_()[0] == 0:
    if idris__123_e1_125_()[0] == -1:
      aux3, aux4, = idris__123_e1_125_()[1:]
      aux2 = idris__123_APPLY0_125_(
               idris__123_in1_125_(),
               None
             )
    else:
      idris_error("unreachable case")
    aux1 = aux2
  elif idris__123_e2_125_()[0] == 1:
    if idris__123_e1_125_()[0] == -1:
      aux6, aux7, = idris__123_e1_125_()[1:]
      aux5 = idris__123_in2_125_()
    else:
      idris_error("unreachable case")
    aux1 = idris__123_APPLY0_125_(
             idris_Prelude_46_Applicative_46_pure(
               None,
               None,
               aux5
             ),
             (0,)
           )
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Basics.id
def idris_Prelude_46_Basics_46_id(loc0, loc1):
  return idris__123_e1_125_()

# Prelude.Classes.intToBool
def idris_Prelude_46_Classes_46_intToBool(loc0):
  if idris__123_e0_125_() == 0:
    aux1 = (0,)
  else:
    aux1 = (1,)
  return aux1

# io_bind
def idris_io_95_bind(loc0, loc1, loc2, loc3, loc4, loc5):
  return idris__123_APPLY0_125_(
           idris__123_io_95_bind2_125_(
             idris__123_e0_125_(),
             idris__123_e1_125_(),
             idris__123_e2_125_(),
             idris__123_e3_125_(),
             idris__123_e4_125_(),
             idris_w()
           ),
           idris__123_APPLY0_125_(
             idris__123_e3_125_(),
             idris_w()
           )
         )

# io_return
def idris_io_95_return(loc0, loc1, loc2, loc3):
  return idris__123_e2_125_()

# Main.main
def idris_Main_46_main():
  return idris__123_U_95_io_95_bind1_125_(
           None,
           None,
           None,
           idris_Prelude_46_putStr(
             None,
             idris__123_APPLY0_125_(
               idris_Prelude_46_show(
                 None,
                 idris__123_U_95_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c1_125_(
                 )
               ),
               idris_Main_46_pythag(
                 10
               )
             )
           ),
           idris__123_U_95_Main_46__123_main0_125_1_125_(
           )
         )

# mkForeignPrim
def idris_mkForeignPrim():
  return None

# Prelude.Algebra.neutral
def idris_Prelude_46_Algebra_46_neutral(loc0, loc1):
  if idris__123_e1_125_()[0] == -1:
    aux2, aux3, = idris__123_e1_125_()[1:]
    aux1 = idris__123_in1_125_()
  else:
    idris_error("unreachable case")
  return aux1

# prim__addInt
def idris_prim_95__95_addInt(loc0, loc1):
  return idris__123_op0_125_() + idris__123_op1_125_()

# prim__concat
def idris_prim_95__95_concat(loc0, loc1):
  return idris__123_op0_125_() + idris__123_op1_125_()

# prim__eqInt
def idris_prim_95__95_eqInt(loc0, loc1):
  return idris__123_op0_125_() == idris__123_op1_125_()

# prim__mulInt
def idris_prim_95__95_mulInt(loc0, loc1):
  return idris__123_op0_125_() * idris__123_op1_125_()

# prim__null
def idris_prim_95__95_null():
  return None

# prim__readFile
def idris_prim_95__95_readFile(loc0, loc1):
  return idris_error("unimplemented external: prim__readFile")

# prim__registerPtr
def idris_prim_95__95_registerPtr(loc0, loc1):
  return idris_error("unimplemented external: prim__registerPtr")

# prim__sextInt_BigInt
def idris_prim_95__95_sextInt_95_BigInt(loc0):
  return idris__123_op0_125_()

# prim__sltInt
def idris_prim_95__95_sltInt(loc0, loc1):
  return idris__123_op0_125_() < idris__123_op1_125_()

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
def idris_prim_95__95_subInt(loc0, loc1):
  return idris__123_op0_125_() - idris__123_op1_125_()

# prim__toStrInt
def idris_prim_95__95_toStrInt(loc0):
  return str(idris__123_op0_125_())

# prim__vm
def idris_prim_95__95_vm():
  return idris_error("unimplemented external: prim__vm")

# prim__writeFile
def idris_prim_95__95_writeFile(loc0, loc1, loc2):
  return idris_error("unimplemented external: prim__writeFile")

# prim__writeString
def idris_prim_95__95_writeString(loc0, loc1):
  return sys.stdout.write(idris__123_op1_125_())

# prim_io_bind
def idris_prim_95_io_95_bind(loc0, loc1, loc2, loc3):
  return idris__123_APPLY0_125_(
           idris__123_e3_125_(),
           idris__123_e2_125_()
         )

# Prelude.Applicative.pure
def idris_Prelude_46_Applicative_46_pure(loc0, loc1, loc2):
  return idris__123_APPLY0_125_(
           idris__123_e2_125_(),
           idris__123_e1_125_()
         )

# Prelude.putStr
def idris_Prelude_46_putStr(loc0, loc1):
  return idris__123_U_95_io_95_bind1_125_(
           None,
           None,
           None,
           idris__123_U_95_Prelude_46__123_putStr0_125_1_125_(
             idris__123_e1_125_()
           ),
           idris__123_U_95_Prelude_46__123_putStr1_125_1_125_(
           )
         )

# Main.pythag
def idris_Main_46_pythag(loc0):
  return idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
           None,
           None,
           idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(
             1,
             idris__123_e0_125_()
           ),
           idris__123_U_95_Main_46__123_pythag6_125_1_125_(
           )
         )

# run__IO
def idris_run_95__95_IO(loc0, loc1):
  return idris__123_APPLY0_125_(
           idris__123_e1_125_(),
           None
         )

# Prelude.show
def idris_Prelude_46_show(loc0, loc1):
  return idris__123_e1_125_()

# unsafePerformPrimIO
def idris_unsafePerformPrimIO():
  return None

# world
def idris_world(loc0):
  return idris__123_e0_125_()

# Prelude.Bool.||
def idris_Prelude_46_Bool_46__124__124_(loc0, loc1):
  if idris__123_e0_125_()[0] == -1:
    aux1 = idris__123_EVAL0_125_(
             idris__123_e1_125_()
           )
  elif idris__123_e0_125_()[0] == -1:
    aux1 = (1,)
  else:
    idris_error("unreachable case")
  return aux1

# {APPLY0}
def idris__123_APPLY0_125_(loc0, loc1):
  if idris__123_fn0_125_()[0] == -1:
    aux1 = idris_Main_46__123_main0_125_(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux1 = idris_Main_46__123_pythag0_125_(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux1 = idris_Main_46__123_pythag1_125_(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux1 = idris_Main_46__123_pythag2_125_(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux2, aux3, aux4, = idris__123_fn0_125_()[1:]
    aux1 = idris_Main_46__123_pythag3_125_(
             idris__123_P_95_c0_125_(),
             idris__123_P_95_c1_125_(),
             idris__123_P_95_c2_125_(),
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux5, aux6, = idris__123_fn0_125_()[1:]
    aux1 = idris_Main_46__123_pythag4_125_(
             idris__123_P_95_c0_125_(),
             idris__123_P_95_c1_125_(),
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux7, = idris__123_fn0_125_()[1:]
    aux1 = idris_Main_46__123_pythag5_125_(
             idris__123_P_95_c0_125_(),
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux1 = idris_Main_46__123_pythag6_125_(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux1 = idris_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux8, aux9, aux10, aux11, aux12, = idris__123_fn0_125_()[1:]
    aux1 = idris_Prelude_46_Basics_46__46_(
             idris__123_P_95_c0_125_(),
             idris__123_P_95_c1_125_(),
             idris__123_P_95_c2_125_(),
             idris__123_P_95_c3_125_(),
             idris__123_P_95_c4_125_(),
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux13, = idris__123_fn0_125_()[1:]
    aux1 = idris_Prelude_46_Basics_46_id(
             idris__123_P_95_c0_125_(),
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux14, = idris__123_fn0_125_()[1:]
    aux1 = idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord0_125_(
             idris__123_P_95_c0_125_(),
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux1 = idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord1_125_(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux15, = idris__123_fn0_125_()[1:]
    aux1 = idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord2_125_(
             idris__123_P_95_c0_125_(),
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux1 = idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord3_125_(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux16, aux17, = idris__123_fn0_125_()[1:]
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_0_125_(
             idris__123_P_95_c0_125_(),
             idris__123_P_95_c1_125_(),
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux18, = idris__123_fn0_125_()[1:]
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_1_125_(
             idris__123_P_95_c0_125_(),
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_2_125_(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_3_125_(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_4_125_(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux19, = idris__123_fn0_125_()[1:]
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_5_125_(
             idris__123_P_95_c0_125_(),
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_6_125_(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux20, = idris__123_fn0_125_()[1:]
    aux1 = idris_Prelude_46__123_putStr0_125_(
             idris__123_P_95_c0_125_(),
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux1 = idris_Prelude_46__123_putStr1_125_(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux21, aux22, aux23, aux24, aux25, = idris__123_fn0_125_()[1:]
    aux1 = idris_io_95_bind(
             idris__123_P_95_c0_125_(),
             idris__123_P_95_c1_125_(),
             idris__123_P_95_c2_125_(),
             idris__123_P_95_c3_125_(),
             idris__123_P_95_c4_125_(),
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux26, aux27, aux28, = idris__123_fn0_125_()[1:]
    aux1 = idris_io_95_return(
             idris__123_P_95_c0_125_(),
             idris__123_P_95_c1_125_(),
             idris__123_P_95_c2_125_(),
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux1 = idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c0_125_(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux1 = idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c1_125_(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux1 = idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c2_125_(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux1 = idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c3_125_(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux1 = idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c4_125_(
             idris__123_arg0_125_()
           )
  elif idris__123_fn0_125_()[0] == -1:
    aux29, aux30, aux31, aux32, aux33, aux34, = idris__123_fn0_125_()[1:]
    aux1 = idris__123_io_95_bind1_125_(
             idris__123_P_95_c0_125_(),
             idris__123_P_95_c1_125_(),
             idris__123_P_95_c2_125_(),
             idris__123_P_95_c3_125_(),
             idris__123_P_95_c4_125_(),
             idris__123_P_95_c5_125_(),
             idris__123_arg0_125_()
           )
  else:
    aux1 = None
  return aux1

# {EVAL0}
def idris__123_EVAL0_125_(loc0):
  return idris__123_arg0_125_()

# Prelude.Classes.{Int instance of Prelude.Classes.Ord0}
def idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord0_125_(loc0, loc1):
  return idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33_compare_58_0(
           idris__123_in0_125_(),
           idris__123_in1_125_()
         )

# {PE_List a instance of Prelude.Show_f5d3ac2c0}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c0_125_(loc0):
  return str(idris__123_in1_125_())

# Prelude.Classes.{Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=0}
def idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__60__61_0_125_(loc0, loc1):
  aux1 = idris__123_e0_125_() == idris__123_e1_125_()
  if aux1 == 0:
    aux2 = (0,)
  else:
    aux2 = (1,)
  return aux2

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=0}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_0_125_(loc0, loc1, loc2):
  return idris_Prelude_46_List_46_foldrImpl(
           None,
           None,
           idris__123_in2_125_(),
           idris__123_in3_125_(),
           idris__123_U_95_Prelude_46_Basics_46_id1_125_(
             None
           ),
           idris__123_in4_125_()
         )

# {io_bind0}
def idris__123_io_95_bind0_125_(loc0, loc1, loc2, loc3, loc4, loc5, loc6):
  return idris__123_APPLY0_125_(
           idris__123_e4_125_(),
           idris__123_in0_125_()
         )

# Main.{main0}
def idris_Main_46__123_main0_125_(loc0):
  return idris_Prelude_46_putStr(
           None,
           "\n"
         )

# Prelude.{putStr0}
def idris_Prelude_46__123_putStr0_125_(loc0, loc1):
  return sys.stdout.write(idris__123_e1_125_())

# Main.{pythag0}
def idris_Main_46__123_pythag0_125_(loc0):
  return (1, idris__123_in4_125_(), (0,))

# {runMain0}
def idris__123_runMain0_125_():
  return idris__123_EVAL0_125_(
           idris__123_APPLY0_125_(
             idris_Main_46_main(
             ),
             None
           )
         )

# Prelude.Classes.{Int instance of Prelude.Classes.Ord1}
def idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord1_125_(loc0):
  return idris__123_U_95_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord0_125_1_125_(
           idris__123_in0_125_()
         )

# {PE_List a instance of Prelude.Show_f5d3ac2c1}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c1_125_(loc0):
  return str(idris__123_in3_125_())

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=1}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_1_125_(loc0, loc1):
  return idris__123_U_95_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_0_125_1_125_(
           idris__123_in2_125_(),
           idris__123_in3_125_()
         )

# {io_bind1}
def idris__123_io_95_bind1_125_(loc0, loc1, loc2, loc3, loc4, loc5, loc6):
  return idris__123_APPLY0_125_(
           idris__123_io_95_bind0_125_(
             idris__123_e0_125_(),
             idris__123_e1_125_(),
             idris__123_e2_125_(),
             idris__123_e3_125_(),
             idris__123_e4_125_(),
             idris_w(),
             idris__123_in0_125_()
           ),
           idris_w()
         )

# Prelude.{putStr1}
def idris_Prelude_46__123_putStr1_125_(loc0):
  return idris__123_U_95_io_95_return1_125_(
           None,
           None,
           (0,)
         )

# Main.{pythag1}
def idris_Main_46__123_pythag1_125_(loc0):
  return idris__123_U_95_Main_46__123_pythag0_125_1_125_(
         )

# Prelude.Classes.{Int instance of Prelude.Classes.Ord2}
def idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord2_125_(loc0, loc1):
  aux1 = idris__123_APPLY0_125_(
           idris__123_APPLY0_125_(
             idris_Prelude_46_Classes_46_compare(
               None,
               idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int(
               )
             ),
             idris__123_in2_125_()
           ),
           idris__123_in3_125_()
         )
  if aux1[0] == -1:
    aux2 = (1,)
  else:
    aux2 = (0,)
  return aux2

# {PE_List a instance of Prelude.Show_f5d3ac2c2}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c2_125_(loc0):
  return str(idris__123_in4_125_())

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=2}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_2_125_(loc0):
  return idris__123_U_95_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_1_125_1_125_(
           idris__123_in2_125_()
         )

# {io_bind2}
def idris__123_io_95_bind2_125_(loc0, loc1, loc2, loc3, loc4, loc5):
  return idris__123_U_95__123_io_95_bind1_125_1_125_(
           idris__123_e0_125_(),
           idris__123_e1_125_(),
           idris__123_e2_125_(),
           idris__123_e3_125_(),
           idris__123_e4_125_(),
           idris_w()
         )

# Main.{pythag2}
def idris_Main_46__123_pythag2_125_(loc0):
  return (0,)

# Prelude.Classes.{Int instance of Prelude.Classes.Ord3}
def idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord3_125_(loc0):
  return idris__123_U_95_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord2_125_1_125_(
           idris__123_in2_125_()
         )

# {PE_List a instance of Prelude.Show_f5d3ac2c3}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c3_125_(loc0):
  return idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
           None,
           None,
           None,
           None,
           idris__123_U_95__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c1_125_1_125_(
           ),
           idris__123_U_95__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c2_125_1_125_(
           ),
           idris__123_in2_125_()
         )

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=3}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_3_125_(loc0):
  return idris__123_U_95_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_2_125_1_125_(
         )

# Main.{pythag3}
def idris_Main_46__123_pythag3_125_(loc0, loc1, loc2, loc3):
  return (1, (0, idris__123_in2_125_(), (0, idris__123_in1_125_(), idris__123_in0_125_())), (0,))

# {PE_List a instance of Prelude.Show_f5d3ac2c4}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c4_125_(loc0):
  return idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
           None,
           None,
           None,
           None,
           idris__123_U_95__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c0_125_1_125_(
           ),
           idris__123_U_95__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c3_125_1_125_(
           ),
           idris__123_in0_125_()
         )

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=4}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_4_125_(loc0):
  return idris__123_U_95_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_3_125_1_125_(
         )

# Main.{pythag4}
def idris_Main_46__123_pythag4_125_(loc0, loc1, loc2):
  aux1 = idris__123_in2_125_() * idris__123_in2_125_() + idris__123_in1_125_() * idris__123_in1_125_() == idris__123_in0_125_() * idris__123_in0_125_()
  if aux1 == 0:
    aux2 = (0,)
  else:
    aux2 = (1,)
  return idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
           None,
           None,
           idris_Prelude_46_Applicative_46_guard(
             None,
             (0, idris__123_U_95_Main_46__123_pythag1_125_1_125_(
                 ), idris__123_U_95_Main_46__123_pythag2_125_1_125_(
                    )),
             aux2
           ),
           idris__123_U_95_Main_46__123_pythag3_125_1_125_(
             idris__123_in2_125_(),
             idris__123_in1_125_(),
             idris__123_in0_125_()
           )
         )

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=5}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_5_125_(loc0, loc1):
  return idris_Prelude_46_List_46__43__43_(
           None,
           idris__123_in5_125_(),
           idris__123_in6_125_()
         )

# Main.{pythag5}
def idris_Main_46__123_pythag5_125_(loc0, loc1):
  return idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
           None,
           None,
           idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(
             1,
             idris__123_in1_125_()
           ),
           idris__123_U_95_Main_46__123_pythag4_125_1_125_(
             idris__123_in1_125_(),
             idris__123_in0_125_()
           )
         )

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=6}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_6_125_(loc0):
  return idris__123_U_95_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_5_125_1_125_(
           idris__123_in5_125_()
         )

# Main.{pythag6}
def idris_Main_46__123_pythag6_125_(loc0):
  return idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
           None,
           None,
           idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(
             1,
             idris__123_in0_125_()
           ),
           idris__123_U_95_Main_46__123_pythag5_125_1_125_(
             idris__123_in0_125_()
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
def idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0_58_go_58_0(loc0, loc1, loc2, loc3, loc4):
  if idris__123_e3_125_() == 0:
    aux1 = (1, idris__123_e4_125_(), idris__123_e2_125_())
  else:
    idris__123_in0_125_ = idris__123_e3_125_() - 1
    aux1 = idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0_58_go_58_0(
             None,
             None,
             (1, idris__123_e4_125_(), idris__123_e2_125_()),
             idris__123_in0_125_(),
             idris__123_e4_125_() - 1
           )
  return aux1

# Prelude.Prelude.List a instance of Prelude.Show, method show, show'
def idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(loc0, loc1, loc2, loc3, loc4, loc5):
  if idris__123_e5_125_()[0] == -1:
    aux2, aux3, = idris__123_e5_125_()[1:]
    if idris__123_in1_125_()[0] == -1:
      aux4 = idris__123_e4_125_() + idris__123_APPLY0_125_(
                                      idris_Prelude_46_show(
                                        None,
                                        idris__123_e3_125_()
                                      ),
                                      idris__123_in0_125_()
                                    )
    else:
      aux4 = idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
               None,
               None,
               None,
               idris__123_e3_125_(),
               idris__123_e4_125_() + idris__123_APPLY0_125_(
                                        idris_Prelude_46_show(
                                          None,
                                          idris__123_e3_125_()
                                        ),
                                        idris__123_in0_125_()
                                      ) + ", ",
               idris__123_in1_125_()
             )
    aux1 = aux4
  elif idris__123_e5_125_()[0] == -1:
    aux1 = idris__123_e4_125_()
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Prelude.Int instance of Prelude.Enum, method enumFromTo
def idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(loc0, loc1):
  aux1 = idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__61__58_0(
           idris__123_e0_125_(),
           idris__123_e1_125_()
         )
  if aux1[0] == 0:
    aux2 = (0,)
  elif aux1[0] == 1:
    aux2 = idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0_58_go_58_0(
             None,
             None,
             (0,),
             idris__123_e1_125_() - idris__123_e0_125_(),
             idris__123_e1_125_()
           )
  else:
    idris_error("unreachable case")
  return aux2

# Prelude.Monad.Prelude.List instance of Prelude.Monad.Monad, method >>=
def idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(loc0, loc1, loc2, loc3):
  return idris__123_APPLY0_125_(
           idris_Prelude_46_Foldable_46_concatMap(
             None,
             None,
             None,
             idris__123_U_95_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_4_125_1_125_(
             ),
             (0, idris__123_U_95_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_6_125_1_125_(
                 ), (0,)),
             idris__123_e3_125_()
           ),
           idris__123_e2_125_()
         )

# Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=
def idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__61__58_0(loc0, loc1):
  aux1 = idris__123_APPLY0_125_(
           idris__123_APPLY0_125_(
             idris_Prelude_46_Classes_46__60_(
               None,
               idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int(
               )
             ),
             idris__123_e0_125_()
           ),
           idris__123_e1_125_()
         )
  if aux1[0] == -1:
    aux2 = idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__60__61_0_125_(
             idris__123_e0_125_(),
             idris__123_e1_125_()
           )
  elif aux1[0] == -1:
    aux2 = (1,)
  else:
    idris_error("unreachable case")
  return aux2

# Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method compare
def idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33_compare_58_0(loc0, loc1):
  aux2 = idris__123_e0_125_() == idris__123_e1_125_()
  if aux2 == 0:
    aux3 = (0,)
  else:
    aux3 = (1,)
  aux1 = aux3
  if aux1[0] == 0:
    aux6 = idris__123_e0_125_() < idris__123_e1_125_()
    if aux6 == 0:
      aux7 = (0,)
    else:
      aux7 = (1,)
    aux5 = aux7
    if aux5[0] == 0:
      aux8 = (2,)
    elif aux5[0] == 1:
      aux8 = (0,)
    else:
      idris_error("unreachable case")
    aux4 = aux8
  elif aux1[0] == 1:
    aux4 = (1,)
  else:
    idris_error("unreachable case")
  return aux4

# Prelude.Prelude.(a, b) instance of Prelude.Show, method show
def idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(loc0, loc1, loc2, loc3, loc4, loc5, loc6):
  if idris__123_e6_125_()[0] == -1:
    aux2, aux3, = idris__123_e6_125_()[1:]
    aux1 = "(" + idris__123_APPLY0_125_(
                   idris_Prelude_46_show(
                     None,
                     idris__123_e4_125_()
                   ),
                   idris__123_in0_125_()
                 ) + ", " + idris__123_APPLY0_125_(
                              idris_Prelude_46_show(
                                None,
                                idris__123_e5_125_()
                              ),
                              idris__123_in1_125_()
                            ) + ")"
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Prelude.List a instance of Prelude.Show, method show
def idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0(loc0, loc1, loc2, loc3):
  return "[" + idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
                 None,
                 None,
                 None,
                 idris__123_e2_125_(),
                 "",
                 idris__123_e3_125_()
               ) + "]"

# with block in Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <
def idris__95_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__58_0_95_with_95_85(loc0, loc1, loc2):
  if idris__123_e0_125_()[0] == -1:
    aux1 = (1,)
  else:
    aux1 = (0,)
  return aux1

# Prelude.Classes.Int instance of Prelude.Classes.Ord
def idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int():
  return (0, idris__123_U_95_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord1_125_1_125_(
             ), idris__123_U_95_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord3_125_1_125_(
                ))

# case block in Void
def idris_Void_95_case():
  return None

# case block in io_bind
def idris_io_95_bind_95_case(loc0, loc1, loc2, loc3, loc4, loc5, loc6, loc7):
  return idris__123_APPLY0_125_(
           idris__123_e7_125_(),
           idris__123_e5_125_()
         )

# <<Void eliminator>>
def idris_Void_95_elim():
  return None

if __name__ == '__main__':
  idris__123_runMain0_125_()