#!/usr/bin/env python

import sys

class IdrisError(Exception):
  pass

def idris_error(msg):
  raise IdrisError(msg)

# Prelude.List.++
def idris_Prelude_46_List_46__43__43_(e0, e1, e2):
  if e1[0] == -1: # Prelude.List.::
    aux2, aux3, = e1[1:]
    aux1 = (1,
             idris__123_in0_125_,
             idris_Prelude_46_List_46__43__43_(
               None,
               idris__123_in1_125_,
               e2
             )
           )
  elif e1[0] == -1: # Prelude.List.Nil
    aux1 = e2
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Basics..
def idris_Prelude_46_Basics_46__46_(e0, e1, e2, e3, e4, idris_x):
  return idris__123_APPLY0_125_(
           e3,
           idris__123_APPLY0_125_(
             e4,
             idris_x
           )
         )

# Prelude.Classes.<
def idris_Prelude_46_Classes_46__60_(e0, e1):
  if e1[0] == -1: # constructor of Prelude.Classes.Ord
    aux2, aux3, = e1[1:]
    aux1 = idris__123_in1_125_
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Algebra.<+>
def idris_Prelude_46_Algebra_46__60__43__62_(e0, e1):
  return e1

# @@constructor of Prelude.Algebra.Monoid#Semigroup a
def idris__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a(e0, e1):
  if e1[0] == -1: # constructor of Prelude.Algebra.Monoid
    aux2, aux3, = e1[1:]
    aux1 = idris__123_in0_125_
  else:
    idris_error("unreachable case")
  return aux1

# @@constructor of Prelude.Applicative.Alternative#Applicative f
def idris__64__64_constructor_32_of_32_Prelude_46_Applicative_46_Alternative_35_Applicative_32_f(e0, e1):
  if e1[0] == -1: # constructor of Prelude.Applicative.Alternative
    aux2, aux3, = e1[1:]
    aux1 = idris__123_in0_125_
  else:
    idris_error("unreachable case")
  return aux1

# Force
def idris_Force(e0, e1, e2):
  idris__123_in0_125_ = idris__123_EVAL0_125_(
                          e2
                        )
  return idris__123_in0_125_

# PE_List a instance of Prelude.Show_f5d3ac2c
def idris_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c(idris__123_meth0_125_):
  return idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0(
           None,
           None,
           (65652,),
           idris__123_meth0_125_
         )

# Prelude.Bool.boolElim
def idris_Prelude_46_Bool_46_boolElim(e0, e1, e2, e3):
  if e1[0] == -1: # Prelude.Bool.False
    aux1 = idris__123_EVAL0_125_(
             e3
           )
  elif e1[0] == -1: # Prelude.Bool.True
    aux1 = idris__123_EVAL0_125_(
             e2
           )
  else:
    idris_error("unreachable case")
  return aux1

# call__IO
def idris_call_95__95_IO(e0, e1, e2):
  return idris__123_APPLY0_125_(
           e2,
           None
         )

# Prelude.Classes.compare
def idris_Prelude_46_Classes_46_compare(e0, e1):
  if e1[0] == -1: # constructor of Prelude.Classes.Ord
    aux2, aux3, = e1[1:]
    aux1 = idris__123_in0_125_
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Foldable.concatMap
def idris_Prelude_46_Foldable_46_concatMap(e0, e1, e2, e3, e4, e5):
  if e4[0] == -1: # constructor of Prelude.Algebra.Monoid
    aux2, aux3, = e4[1:]
    aux1 = idris__123_in0_125_
  else:
    idris_error("unreachable case")
  if e4[0] == -1: # constructor of Prelude.Algebra.Monoid
    aux5, aux6, = e4[1:]
    aux4 = idris__123_in3_125_
  else:
    idris_error("unreachable case")
  return idris__123_APPLY0_125_(
           idris__123_APPLY0_125_(
             idris_Prelude_46_Foldable_46_foldr(
               None,
               None,
               None,
               e3
             ),
             (65631,
               None,
               None,
               None,
               aux1,
               e5
             )
           ),
           aux4
         )

# Prelude.Applicative.empty
def idris_Prelude_46_Applicative_46_empty(e0, e1, e2):
  if e2[0] == -1: # constructor of Prelude.Applicative.Alternative
    aux2, aux3, = e2[1:]
    aux1 = idris__123_APPLY0_125_(
             idris__123_in1_125_,
             e1
           )
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Foldable.foldr
def idris_Prelude_46_Foldable_46_foldr(e0, e1, e2, e3):
  return idris__123_APPLY0_125_(
           idris__123_APPLY0_125_(
             e3,
             e1
           ),
           e2
         )

# Prelude.List.foldrImpl
def idris_Prelude_46_List_46_foldrImpl(e0, e1, e2, e3, e4, e5):
  if e5[0] == -1: # Prelude.List.::
    aux2, aux3, = e5[1:]
    aux1 = idris_Prelude_46_List_46_foldrImpl(
             None,
             None,
             e2,
             e3,
             (65631,
               None,
               None,
               None,
               e4,
               idris__123_APPLY0_125_(
                 e2,
                 idris__123_in0_125_
               )
             ),
             idris__123_in1_125_
           )
  elif e5[0] == -1: # Prelude.List.Nil
    aux1 = idris__123_APPLY0_125_(
             e4,
             e3
           )
  else:
    idris_error("unreachable case")
  return aux1

# Prelude.Applicative.guard
def idris_Prelude_46_Applicative_46_guard(e0, e1, e2):
  if e2[0] == 0: # Prelude.Bool.False
    if e1[0] == -1: # constructor of Prelude.Applicative.Alternative
      aux3, aux4, = e1[1:]
      aux2 = idris__123_APPLY0_125_(
               idris__123_in1_125_,
               None
             )
    else:
      idris_error("unreachable case")
    aux1 = aux2
  elif e2[0] == 1: # Prelude.Bool.True
    if e1[0] == -1: # constructor of Prelude.Applicative.Alternative
      aux6, aux7, = e1[1:]
      aux5 = idris__123_in2_125_
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
  return idris__123_APPLY0_125_(
           idris__123_io_95_bind2_125_(
             e0,
             e1,
             e2,
             e3,
             e4,
             idris_w
           ),
           idris__123_APPLY0_125_(
             e3,
             idris_w
           )
         )

# io_return
def idris_io_95_return(e0, e1, e2, idris_w):
  return e2

# Main.main
def idris_Main_46_main():
  return (65646,
           None,
           None,
           None,
           idris_Prelude_46_putStr(
             None,
             idris__123_APPLY0_125_(
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
  if e1[0] == -1: # constructor of Prelude.Algebra.Monoid
    aux2, aux3, = e1[1:]
    aux1 = idris__123_in1_125_
  else:
    idris_error("unreachable case")
  return aux1

# prim__addInt
def idris_prim_95__95_addInt(idris__123_op0_125_, idris__123_op1_125_):
  return idris__123_op0_125_ + idris__123_op1_125_

# prim__concat
def idris_prim_95__95_concat(idris__123_op0_125_, idris__123_op1_125_):
  return idris__123_op0_125_ + idris__123_op1_125_

# prim__eqInt
def idris_prim_95__95_eqInt(idris__123_op0_125_, idris__123_op1_125_):
  return idris__123_op0_125_ == idris__123_op1_125_

# prim__mulInt
def idris_prim_95__95_mulInt(idris__123_op0_125_, idris__123_op1_125_):
  return idris__123_op0_125_ * idris__123_op1_125_

# prim__null
def idris_prim_95__95_null():
  return None

# prim__readFile
def idris_prim_95__95_readFile(idris__123_op0_125_, idris__123_op1_125_):
  return idris_error("unimplemented external: prim__readFile")

# prim__registerPtr
def idris_prim_95__95_registerPtr(idris__123_op0_125_, idris__123_op1_125_):
  return idris_error("unimplemented external: prim__registerPtr")

# prim__sextInt_BigInt
def idris_prim_95__95_sextInt_95_BigInt(idris__123_op0_125_):
  return idris__123_op0_125_

# prim__sltInt
def idris_prim_95__95_sltInt(idris__123_op0_125_, idris__123_op1_125_):
  return idris__123_op0_125_ < idris__123_op1_125_

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
def idris_prim_95__95_subInt(idris__123_op0_125_, idris__123_op1_125_):
  return idris__123_op0_125_ - idris__123_op1_125_

# prim__toStrInt
def idris_prim_95__95_toStrInt(idris__123_op0_125_):
  return str(idris__123_op0_125_)

# prim__vm
def idris_prim_95__95_vm():
  return idris_error("unimplemented external: prim__vm")

# prim__writeFile
def idris_prim_95__95_writeFile(idris__123_op0_125_, idris__123_op1_125_, idris__123_op2_125_):
  return idris_error("unimplemented external: prim__writeFile")

# prim__writeString
def idris_prim_95__95_writeString(idris__123_op0_125_, idris__123_op1_125_):
  return sys.stdout.write(idris__123_op1_125_)

# prim_io_bind
def idris_prim_95_io_95_bind(e0, e1, e2, e3):
  return idris__123_APPLY0_125_(
           e3,
           e2
         )

# Prelude.Applicative.pure
def idris_Prelude_46_Applicative_46_pure(e0, e1, e2):
  return idris__123_APPLY0_125_(
           e2,
           e1
         )

# Prelude.putStr
def idris_Prelude_46_putStr(e0, e1):
  return (65646,
           None,
           None,
           None,
           (65644,
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
  return idris__123_APPLY0_125_(
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
  if e0[0] == -1: # Prelude.Bool.False
    aux1 = idris__123_EVAL0_125_(
             e1
           )
  elif e0[0] == -1: # Prelude.Bool.True
    aux1 = (1,)
  else:
    idris_error("unreachable case")
  return aux1

# {APPLY0}
def idris__123_APPLY0_125_(idris__123_fn0_125_, idris__123_arg0_125_):
  if idris__123_fn0_125_[0] == -1: # {U_Main.{main0}1}
    aux1 = idris_Main_46__123_main0_125_(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Main.{pythag0}1}
    aux1 = idris_Main_46__123_pythag0_125_(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Main.{pythag1}1}
    aux1 = idris_Main_46__123_pythag1_125_(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Main.{pythag2}1}
    aux1 = idris_Main_46__123_pythag2_125_(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Main.{pythag3}1}
    aux2, aux3, aux4, = idris__123_fn0_125_[1:]
    aux1 = idris_Main_46__123_pythag3_125_(
             idris__123_P_95_c0_125_,
             idris__123_P_95_c1_125_,
             idris__123_P_95_c2_125_,
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Main.{pythag4}1}
    aux5, aux6, = idris__123_fn0_125_[1:]
    aux1 = idris_Main_46__123_pythag4_125_(
             idris__123_P_95_c0_125_,
             idris__123_P_95_c1_125_,
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Main.{pythag5}1}
    aux7, = idris__123_fn0_125_[1:]
    aux1 = idris_Main_46__123_pythag5_125_(
             idris__123_P_95_c0_125_,
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Main.{pythag6}1}
    aux1 = idris_Main_46__123_pythag6_125_(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_PE_List a instance of Prelude.Show_f5d3ac2c1}
    aux1 = idris_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Prelude.Basics..1}
    aux8, aux9, aux10, aux11, aux12, = idris__123_fn0_125_[1:]
    aux1 = idris_Prelude_46_Basics_46__46_(
             idris__123_P_95_c0_125_,
             idris__123_P_95_c1_125_,
             idris__123_P_95_c2_125_,
             idris__123_P_95_c3_125_,
             idris__123_P_95_c4_125_,
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Prelude.Basics.id1}
    aux13, = idris__123_fn0_125_[1:]
    aux1 = idris_Prelude_46_Basics_46_id(
             idris__123_P_95_c0_125_,
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord0}1}
    aux14, = idris__123_fn0_125_[1:]
    aux1 = idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord0_125_(
             idris__123_P_95_c0_125_,
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord1}1}
    aux1 = idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord1_125_(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord2}1}
    aux15, = idris__123_fn0_125_[1:]
    aux1 = idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord2_125_(
             idris__123_P_95_c0_125_,
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord3}1}
    aux1 = idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord3_125_(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=0}1}
    aux16, aux17, = idris__123_fn0_125_[1:]
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_0_125_(
             idris__123_P_95_c0_125_,
             idris__123_P_95_c1_125_,
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=1}1}
    aux18, = idris__123_fn0_125_[1:]
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_1_125_(
             idris__123_P_95_c0_125_,
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=2}1}
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_2_125_(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=3}1}
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_3_125_(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=4}1}
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_4_125_(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=5}1}
    aux19, = idris__123_fn0_125_[1:]
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_5_125_(
             idris__123_P_95_c0_125_,
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=6}1}
    aux1 = idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_6_125_(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Prelude.{putStr0}1}
    aux20, = idris__123_fn0_125_[1:]
    aux1 = idris_Prelude_46__123_putStr0_125_(
             idris__123_P_95_c0_125_,
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_Prelude.{putStr1}1}
    aux1 = idris_Prelude_46__123_putStr1_125_(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_io_bind1}
    aux21, aux22, aux23, aux24, aux25, = idris__123_fn0_125_[1:]
    aux1 = idris_io_95_bind(
             idris__123_P_95_c0_125_,
             idris__123_P_95_c1_125_,
             idris__123_P_95_c2_125_,
             idris__123_P_95_c3_125_,
             idris__123_P_95_c4_125_,
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_io_return1}
    aux26, aux27, aux28, = idris__123_fn0_125_[1:]
    aux1 = idris_io_95_return(
             idris__123_P_95_c0_125_,
             idris__123_P_95_c1_125_,
             idris__123_P_95_c2_125_,
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_{PE_List a instance of Prelude.Show_f5d3ac2c0}1}
    aux1 = idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c0_125_(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_{PE_List a instance of Prelude.Show_f5d3ac2c1}1}
    aux1 = idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c1_125_(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_{PE_List a instance of Prelude.Show_f5d3ac2c2}1}
    aux1 = idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c2_125_(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_{PE_List a instance of Prelude.Show_f5d3ac2c3}1}
    aux1 = idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c3_125_(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_{PE_List a instance of Prelude.Show_f5d3ac2c4}1}
    aux1 = idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c4_125_(
             idris__123_arg0_125_
           )
  elif idris__123_fn0_125_[0] == -1: # {U_{io_bind1}1}
    aux29, aux30, aux31, aux32, aux33, aux34, = idris__123_fn0_125_[1:]
    aux1 = idris__123_io_95_bind1_125_(
             idris__123_P_95_c0_125_,
             idris__123_P_95_c1_125_,
             idris__123_P_95_c2_125_,
             idris__123_P_95_c3_125_,
             idris__123_P_95_c4_125_,
             idris__123_P_95_c5_125_,
             idris__123_arg0_125_
           )
  else:
    aux1 = None
  return aux1

# {EVAL0}
def idris__123_EVAL0_125_(idris__123_arg0_125_):
  return idris__123_arg0_125_

# Prelude.Classes.{Int instance of Prelude.Classes.Ord0}
def idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord0_125_(idris__123_in0_125_, idris__123_in1_125_):
  return idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33_compare_58_0(
           idris__123_in0_125_,
           idris__123_in1_125_
         )

# {PE_List a instance of Prelude.Show_f5d3ac2c0}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c0_125_(idris__123_in1_125_):
  return str(idris__123_in1_125_)

# Prelude.Classes.{Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=0}
def idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__60__61_0_125_(e0, e1):
  aux1 = e0 == e1
  if aux1 == 0:
    aux2 = (0,)
  else:
    aux2 = (1,)
  return aux2

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=0}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_0_125_(idris__123_in2_125_, idris__123_in3_125_, idris__123_in4_125_):
  return idris_Prelude_46_List_46_foldrImpl(
           None,
           None,
           idris__123_in2_125_,
           idris__123_in3_125_,
           (65632,
             None
           ),
           idris__123_in4_125_
         )

# {io_bind0}
def idris__123_io_95_bind0_125_(e0, e1, e2, e3, e4, idris_w, idris__123_in0_125_):
  return idris__123_APPLY0_125_(
           e4,
           idris__123_in0_125_
         )

# Main.{main0}
def idris_Main_46__123_main0_125_(idris__123_in0_125_):
  return idris_Prelude_46_putStr(
           None,
           "\n"
         )

# Prelude.{putStr0}
def idris_Prelude_46__123_putStr0_125_(e1, idris__123_in0_125_):
  return sys.stdout.write(e1)

# Main.{pythag0}
def idris_Main_46__123_pythag0_125_(idris__123_in4_125_):
  return (1,
           idris__123_in4_125_,
           (0,)
         )

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
def idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord1_125_(idris__123_in0_125_):
  return (65633,
           idris__123_in0_125_
         )

# {PE_List a instance of Prelude.Show_f5d3ac2c1}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c1_125_(idris__123_in3_125_):
  return str(idris__123_in3_125_)

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=1}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_1_125_(idris__123_in2_125_, idris__123_in3_125_):
  return (65637,
           idris__123_in2_125_,
           idris__123_in3_125_
         )

# {io_bind1}
def idris__123_io_95_bind1_125_(e0, e1, e2, e3, e4, idris_w, idris__123_in0_125_):
  return idris__123_APPLY0_125_(
           idris__123_io_95_bind0_125_(
             e0,
             e1,
             e2,
             e3,
             e4,
             idris_w,
             idris__123_in0_125_
           ),
           idris_w
         )

# Prelude.{putStr1}
def idris_Prelude_46__123_putStr1_125_(idris__123_in1_125_):
  return (65647,
           None,
           None,
           (0,)
         )

# Main.{pythag1}
def idris_Main_46__123_pythag1_125_(idris__123_in3_125_):
  return (65623,)

# Prelude.Classes.{Int instance of Prelude.Classes.Ord2}
def idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord2_125_(idris__123_in2_125_, idris__123_in3_125_):
  aux1 = idris__123_APPLY0_125_(
           idris__123_APPLY0_125_(
             idris_Prelude_46_Classes_46_compare(
               None,
               idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int(
               )
             ),
             idris__123_in2_125_
           ),
           idris__123_in3_125_
         )
  if aux1[0] == -1: # Prelude.Classes.LT
    aux2 = (1,)
  else:
    aux2 = (0,)
  return aux2

# {PE_List a instance of Prelude.Show_f5d3ac2c2}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c2_125_(idris__123_in4_125_):
  return str(idris__123_in4_125_)

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=2}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_2_125_(idris__123_in2_125_):
  return (65638,
           idris__123_in2_125_
         )

# {io_bind2}
def idris__123_io_95_bind2_125_(e0, e1, e2, e3, e4, idris_w):
  return (65653,
           e0,
           e1,
           e2,
           e3,
           e4,
           idris_w
         )

# Main.{pythag2}
def idris_Main_46__123_pythag2_125_(idris__123_in5_125_):
  return (0,)

# Prelude.Classes.{Int instance of Prelude.Classes.Ord3}
def idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord3_125_(idris__123_in2_125_):
  return (65635,
           idris__123_in2_125_
         )

# {PE_List a instance of Prelude.Show_f5d3ac2c3}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c3_125_(idris__123_in2_125_):
  return idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
           None,
           None,
           None,
           None,
           (65649,),
           (65650,),
           idris__123_in2_125_
         )

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=3}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_3_125_(idris__123_in1_125_):
  return (65639,)

# Main.{pythag3}
def idris_Main_46__123_pythag3_125_(idris__123_in2_125_, idris__123_in1_125_, idris__123_in0_125_, idris__123_in6_125_):
  return (1,
           (0,
             idris__123_in2_125_,
             (0,
               idris__123_in1_125_,
               idris__123_in0_125_
             )
           ),
           (0,)
         )

# {PE_List a instance of Prelude.Show_f5d3ac2c4}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c4_125_(idris__123_in0_125_):
  return idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
           None,
           None,
           None,
           None,
           (65648,),
           (65651,),
           idris__123_in0_125_
         )

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=4}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_4_125_(idris__123_in0_125_):
  return (65640,)

# Main.{pythag4}
def idris_Main_46__123_pythag4_125_(idris__123_in1_125_, idris__123_in0_125_, idris__123_in2_125_):
  aux1 = idris__123_in2_125_ * idris__123_in2_125_ + idris__123_in1_125_ * idris__123_in1_125_ == idris__123_in0_125_ * idris__123_in0_125_
  if aux1 == 0:
    aux2 = (0,)
  else:
    aux2 = (1,)
  return idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
           None,
           None,
           idris_Prelude_46_Applicative_46_guard(
             None,
             (0,
               (65624,),
               (65625,)
             ),
             aux2
           ),
           (65626,
             idris__123_in2_125_,
             idris__123_in1_125_,
             idris__123_in0_125_
           )
         )

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=5}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_5_125_(idris__123_in5_125_, idris__123_in6_125_):
  return idris_Prelude_46_List_46__43__43_(
           None,
           idris__123_in5_125_,
           idris__123_in6_125_
         )

# Main.{pythag5}
def idris_Main_46__123_pythag5_125_(idris__123_in0_125_, idris__123_in1_125_):
  return idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
           None,
           None,
           idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(
             1,
             idris__123_in1_125_
           ),
           (65627,
             idris__123_in1_125_,
             idris__123_in0_125_
           )
         )

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=6}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_6_125_(idris__123_in5_125_):
  return (65642,
           idris__123_in5_125_
         )

# Main.{pythag6}
def idris_Main_46__123_pythag6_125_(idris__123_in0_125_):
  return idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
           None,
           None,
           idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(
             1,
             idris__123_in0_125_
           ),
           (65628,
             idris__123_in0_125_
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
    aux1 = (1,
             e4,
             e2
           )
  else:
    idris__123_in0_125_ = e3 - 1
    aux1 = idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0_58_go_58_0(
             None,
             None,
             (1,
               e4,
               e2
             ),
             idris__123_in0_125_,
             e4 - 1
           )
  return aux1

# Prelude.Prelude.List a instance of Prelude.Show, method show, show'
def idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(e0, e1, e2, e3, e4, e5):
  if e5[0] == -1: # Prelude.List.::
    aux2, aux3, = e5[1:]
    if idris__123_in1_125_[0] == -1: # Prelude.List.Nil
      aux4 = e4 + idris__123_APPLY0_125_(
                    idris_Prelude_46_show(
                      None,
                      e3
                    ),
                    idris__123_in0_125_
                  )
    else:
      aux4 = idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
               None,
               None,
               None,
               e3,
               e4 + idris__123_APPLY0_125_(
                      idris_Prelude_46_show(
                        None,
                        e3
                      ),
                      idris__123_in0_125_
                    ) + ", ",
               idris__123_in1_125_
             )
    aux1 = aux4
  elif e5[0] == -1: # Prelude.List.Nil
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
  return idris__123_APPLY0_125_(
           idris_Prelude_46_Foldable_46_concatMap(
             None,
             None,
             None,
             (65641,),
             (0,
               (65643,),
               (0,)
             ),
             e3
           ),
           e2
         )

# Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=
def idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__61__58_0(e0, e1):
  aux1 = idris__123_APPLY0_125_(
           idris__123_APPLY0_125_(
             idris_Prelude_46_Classes_46__60_(
               None,
               idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int(
               )
             ),
             e0
           ),
           e1
         )
  if aux1[0] == -1: # Prelude.Bool.False
    aux2 = idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__60__61_0_125_(
             e0,
             e1
           )
  elif aux1[0] == -1: # Prelude.Bool.True
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
  if e6[0] == -1: # Builtins.MkPair
    aux2, aux3, = e6[1:]
    aux1 = "(" + idris__123_APPLY0_125_(
                   idris_Prelude_46_show(
                     None,
                     e4
                   ),
                   idris__123_in0_125_
                 ) + ", " + idris__123_APPLY0_125_(
                              idris_Prelude_46_show(
                                None,
                                e5
                              ),
                              idris__123_in1_125_
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
  if e0[0] == -1: # Prelude.Classes.LT
    aux1 = (1,)
  else:
    aux1 = (0,)
  return aux1

# Prelude.Classes.Int instance of Prelude.Classes.Ord
def idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int():
  return (0,
           (65634,),
           (65636,)
         )

# case block in Void
def idris_Void_95_case():
  return None

# case block in io_bind
def idris_io_95_bind_95_case(e0, e1, e2, e3, e4, e5, e6, e7):
  return idris__123_APPLY0_125_(
           e7,
           e5
         )

# <<Void eliminator>>
def idris_Void_95_elim():
  return None

if __name__ == '__main__':
  idris__123_runMain0_125_()