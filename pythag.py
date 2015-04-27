#!/usr/bin/env python

import sys

class IdrisError(Exception):
  pass

def idris_error(msg):
  raise IdrisError(msg)

# Prelude.List.++
def idris_Prelude_46_List_46__43__43_(loc0, loc1, loc2):
  if loc1[0] == 1:
    loc3, loc4, = loc1[1:]
    loc5 = None
    loc5 = idris_Prelude_46_List_46__43__43_(loc5, loc4, loc2)
    return (1, loc3, loc5)
  elif loc1[0] == 0:
    return loc2
  else:
    return idris_error("unreachable case")

# Prelude.Basics..
def idris_Prelude_46_Basics_46__46_(loc0, loc1, loc2, loc3, loc4, loc5):
  loc6 = idris__123_APPLY0_125_(loc4, loc5)
  return idris__123_APPLY0_125_(loc3, loc6)

# Prelude.Classes.<
def idris_Prelude_46_Classes_46__60_(loc0, loc1):
  if loc1[0] == 0:
    loc2, loc3, = loc1[1:]
    return loc3
  else:
    return idris_error("unreachable case")

# Prelude.Algebra.<+>
def idris_Prelude_46_Algebra_46__60__43__62_(loc0, loc1):
  return loc1

# @@constructor of Prelude.Algebra.Monoid#Semigroup a
def idris__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a(loc0, loc1):
  if loc1[0] == 0:
    loc2, loc3, = loc1[1:]
    return loc2
  else:
    return idris_error("unreachable case")

# @@constructor of Prelude.Applicative.Alternative#Applicative f
def idris__64__64_constructor_32_of_32_Prelude_46_Applicative_46_Alternative_35_Applicative_32_f(loc0, loc1):
  if loc1[0] == 0:
    loc2, loc3, = loc1[1:]
    return loc2
  else:
    return idris_error("unreachable case")

# Force
def idris_Force(loc0, loc1, loc2):
  loc3 = idris__123_EVAL0_125_(loc2)
  return loc3

# PE_List a instance of Prelude.Show_f5d3ac2c
def idris_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c(loc0):
  loc1 = None
  loc2 = None
  loc3 = (65651,)
  return idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0(loc1, loc2, loc3, loc0)

# Prelude.Bool.boolElim
def idris_Prelude_46_Bool_46_boolElim(loc0, loc1, loc2, loc3):
  if loc1[0] == 0:
    return idris__123_EVAL0_125_(loc3)
  elif loc1[0] == 1:
    return idris__123_EVAL0_125_(loc2)
  else:
    return idris_error("unreachable case")

# call__IO
def idris_call_95__95_IO(loc0, loc1, loc2):
  loc3 = None
  return idris__123_APPLY0_125_(loc2, loc3)

# Prelude.Classes.compare
def idris_Prelude_46_Classes_46_compare(loc0, loc1):
  if loc1[0] == 0:
    loc2, loc3, = loc1[1:]
    return loc2
  else:
    return idris_error("unreachable case")

# Prelude.Foldable.concatMap
def idris_Prelude_46_Foldable_46_concatMap(loc0, loc1, loc2, loc3, loc4, loc5):
  loc6 = None
  loc7 = None
  loc8 = None
  loc6 = idris_Prelude_46_Foldable_46_foldr(loc6, loc7, loc8, loc3)
  loc7 = None
  loc8 = None
  loc9 = None
  if loc4[0] == 0:
    loc10, loc11, = loc4[1:]
    loc10 = loc10
  else:
    loc10 = idris_error("unreachable case")
  loc7 = (65630, loc7, loc8, loc9, loc10, loc5)
  loc6 = idris__123_APPLY0_125_(loc6, loc7)
  if loc4[0] == 0:
    loc7, loc8, = loc4[1:]
    loc7 = loc8
  else:
    loc7 = idris_error("unreachable case")
  return idris__123_APPLY0_125_(loc6, loc7)

# Prelude.Applicative.empty
def idris_Prelude_46_Applicative_46_empty(loc0, loc1, loc2):
  if loc2[0] == 0:
    loc3, loc4, = loc2[1:]
    return idris__123_APPLY0_125_(loc4, loc1)
  else:
    return idris_error("unreachable case")

# Prelude.Foldable.foldr
def idris_Prelude_46_Foldable_46_foldr(loc0, loc1, loc2, loc3):
  loc4 = idris__123_APPLY0_125_(loc3, loc1)
  return idris__123_APPLY0_125_(loc4, loc2)

# Prelude.List.foldrImpl
def idris_Prelude_46_List_46_foldrImpl(loc0, loc1, loc2, loc3, loc4, loc5):
  if loc5[0] == 1:
    loc6, loc7, = loc5[1:]
    loc8 = None
    loc9 = None
    loc10 = None
    loc11 = None
    loc12 = None
    loc13 = idris__123_APPLY0_125_(loc2, loc6)
    loc10 = (65630, loc10, loc11, loc12, loc4, loc13)
    return idris_Prelude_46_List_46_foldrImpl(loc8, loc9, loc2, loc3, loc10, loc7)
  elif loc5[0] == 0:
    return idris__123_APPLY0_125_(loc4, loc3)
  else:
    return idris_error("unreachable case")

# Prelude.Applicative.guard
def idris_Prelude_46_Applicative_46_guard(loc0, loc1, loc2):
  if loc2[0] == 0:
    if loc1[0] == 0:
      loc3, loc4, = loc1[1:]
      loc5 = None
      return idris__123_APPLY0_125_(loc4, loc5)
    else:
      return idris_error("unreachable case")
  elif loc2[0] == 1:
    loc3 = None
    loc4 = None
    if loc1[0] == 0:
      loc5, loc6, = loc1[1:]
      loc5 = loc5
    else:
      loc5 = idris_error("unreachable case")
    loc3 = idris_Prelude_46_Applicative_46_pure(loc3, loc4, loc5)
    loc4 = (0,)
    return idris__123_APPLY0_125_(loc3, loc4)
  else:
    return idris_error("unreachable case")

# Prelude.Basics.id
def idris_Prelude_46_Basics_46_id(loc0, loc1):
  return loc1

# Prelude.Classes.intToBool
def idris_Prelude_46_Classes_46_intToBool(loc0):
  if loc0 == 0:
    return (0,)
  else:
    return (1,)

# io_bind
def idris_io_95_bind(loc0, loc1, loc2, loc3, loc4, loc5):
  loc6 = idris__123_io_95_bind2_125_(loc0, loc1, loc2, loc3, loc4, loc5)
  loc7 = idris__123_APPLY0_125_(loc3, loc5)
  return idris__123_APPLY0_125_(loc6, loc7)

# io_return
def idris_io_95_return(loc0, loc1, loc2, loc3):
  return loc2

# Main.main
def idris_Main_46_main():
  loc0 = None
  loc1 = None
  loc2 = (65629,)
  loc1 = idris_Prelude_46_show(loc1, loc2)
  loc2 = 100
  loc2 = idris_Main_46_pythag(loc2)
  loc1 = idris__123_APPLY0_125_(loc1, loc2)
  return idris_Prelude_46_putStr(loc0, loc1)

# mkForeignPrim
def idris_mkForeignPrim():
  return None

# Prelude.Algebra.neutral
def idris_Prelude_46_Algebra_46_neutral(loc0, loc1):
  if loc1[0] == 0:
    loc2, loc3, = loc1[1:]
    return loc3
  else:
    return idris_error("unreachable case")

# prim__addInt
def idris_prim_95__95_addInt(loc0, loc1):
  return loc0 + loc1

# prim__concat
def idris_prim_95__95_concat(loc0, loc1):
  return loc0 + loc1

# prim__eqInt
def idris_prim_95__95_eqInt(loc0, loc1):
  return loc0 == loc1

# prim__mulInt
def idris_prim_95__95_mulInt(loc0, loc1):
  return loc0 * loc1

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
  return loc0

# prim__sltInt
def idris_prim_95__95_sltInt(loc0, loc1):
  return loc0 < loc1

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
  return loc0 - loc1

# prim__toStrInt
def idris_prim_95__95_toStrInt(loc0):
  return str(loc0)

# prim__vm
def idris_prim_95__95_vm():
  return idris_error("unimplemented external: prim__vm")

# prim__writeFile
def idris_prim_95__95_writeFile(loc0, loc1, loc2):
  return idris_error("unimplemented external: prim__writeFile")

# prim__writeString
def idris_prim_95__95_writeString(loc0, loc1):
  return sys.stdout.write(loc1)

# prim_io_bind
def idris_prim_95_io_95_bind(loc0, loc1, loc2, loc3):
  return idris__123_APPLY0_125_(loc3, loc2)

# Prelude.Applicative.pure
def idris_Prelude_46_Applicative_46_pure(loc0, loc1, loc2):
  return idris__123_APPLY0_125_(loc2, loc1)

# Prelude.putStr
def idris_Prelude_46_putStr(loc0, loc1):
  loc2 = None
  loc3 = None
  loc4 = None
  loc5 = (65643, loc1)
  loc6 = (65644,)
  return (65645, loc2, loc3, loc4, loc5, loc6)

# Main.pythag
def idris_Main_46_pythag(loc0):
  loc1 = None
  loc2 = None
  loc3 = 1
  loc3 = idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(loc3, loc0)
  loc4 = (65628,)
  return idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(loc1, loc2, loc3, loc4)

# run__IO
def idris_run_95__95_IO(loc0, loc1):
  loc2 = None
  return idris__123_APPLY0_125_(loc1, loc2)

# Prelude.show
def idris_Prelude_46_show(loc0, loc1):
  return loc1

# unsafePerformPrimIO
def idris_unsafePerformPrimIO():
  return None

# world
def idris_world(loc0):
  return loc0

# Prelude.Bool.||
def idris_Prelude_46_Bool_46__124__124_(loc0, loc1):
  if loc0[0] == 0:
    return idris__123_EVAL0_125_(loc1)
  elif loc0[0] == 1:
    return (1,)
  else:
    return idris_error("unreachable case")

# {APPLY0}
def idris__123_APPLY0_125_(loc0, loc1):
  if loc0[0] == 65622:
    return idris_Main_46__123_pythag0_125_(loc1)
  elif loc0[0] == 65623:
    return idris_Main_46__123_pythag1_125_(loc1)
  elif loc0[0] == 65624:
    return idris_Main_46__123_pythag2_125_(loc1)
  elif loc0[0] == 65625:
    loc2, loc3, loc4, = loc0[1:]
    return idris_Main_46__123_pythag3_125_(loc2, loc3, loc4, loc1)
  elif loc0[0] == 65626:
    loc2, loc3, = loc0[1:]
    return idris_Main_46__123_pythag4_125_(loc2, loc3, loc1)
  elif loc0[0] == 65627:
    loc2, = loc0[1:]
    return idris_Main_46__123_pythag5_125_(loc2, loc1)
  elif loc0[0] == 65628:
    return idris_Main_46__123_pythag6_125_(loc1)
  elif loc0[0] == 65629:
    return idris_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c(loc1)
  elif loc0[0] == 65630:
    loc2, loc3, loc4, loc5, loc6, = loc0[1:]
    return idris_Prelude_46_Basics_46__46_(loc2, loc3, loc4, loc5, loc6, loc1)
  elif loc0[0] == 65631:
    loc2, = loc0[1:]
    return idris_Prelude_46_Basics_46_id(loc2, loc1)
  elif loc0[0] == 65632:
    loc2, = loc0[1:]
    return idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord0_125_(loc2, loc1)
  elif loc0[0] == 65633:
    return idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord1_125_(loc1)
  elif loc0[0] == 65634:
    loc2, = loc0[1:]
    return idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord2_125_(loc2, loc1)
  elif loc0[0] == 65635:
    return idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord3_125_(loc1)
  elif loc0[0] == 65636:
    loc2, loc3, = loc0[1:]
    return idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_0_125_(loc2, loc3, loc1)
  elif loc0[0] == 65637:
    loc2, = loc0[1:]
    return idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_1_125_(loc2, loc1)
  elif loc0[0] == 65638:
    return idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_2_125_(loc1)
  elif loc0[0] == 65639:
    return idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_3_125_(loc1)
  elif loc0[0] == 65640:
    return idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_4_125_(loc1)
  elif loc0[0] == 65641:
    loc2, = loc0[1:]
    return idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_5_125_(loc2, loc1)
  elif loc0[0] == 65642:
    return idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_6_125_(loc1)
  elif loc0[0] == 65643:
    loc2, = loc0[1:]
    return idris_Prelude_46__123_putStr0_125_(loc2, loc1)
  elif loc0[0] == 65644:
    return idris_Prelude_46__123_putStr1_125_(loc1)
  elif loc0[0] == 65645:
    loc2, loc3, loc4, loc5, loc6, = loc0[1:]
    return idris_io_95_bind(loc2, loc3, loc4, loc5, loc6, loc1)
  elif loc0[0] == 65646:
    loc2, loc3, loc4, = loc0[1:]
    return idris_io_95_return(loc2, loc3, loc4, loc1)
  elif loc0[0] == 65647:
    return idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c0_125_(loc1)
  elif loc0[0] == 65648:
    return idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c1_125_(loc1)
  elif loc0[0] == 65649:
    return idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c2_125_(loc1)
  elif loc0[0] == 65650:
    return idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c3_125_(loc1)
  elif loc0[0] == 65651:
    return idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c4_125_(loc1)
  elif loc0[0] == 65652:
    loc2, loc3, loc4, loc5, loc6, loc7, = loc0[1:]
    return idris__123_io_95_bind1_125_(loc2, loc3, loc4, loc5, loc6, loc7, loc1)
  else:
    return None

# {EVAL0}
def idris__123_EVAL0_125_(loc0):
  return loc0

# Prelude.Classes.{Int instance of Prelude.Classes.Ord0}
def idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord0_125_(loc0, loc1):
  return idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33_compare_58_0(loc0, loc1)

# {PE_List a instance of Prelude.Show_f5d3ac2c0}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c0_125_(loc0):
  return str(loc0)

# Prelude.Classes.{Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=0}
def idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__60__61_0_125_(loc0, loc1):
  loc2 = loc0 == loc1
  if loc2 == 0:
    return (0,)
  else:
    return (1,)

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=0}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_0_125_(loc0, loc1, loc2):
  loc3 = None
  loc4 = None
  loc5 = None
  loc5 = (65631, loc5)
  return idris_Prelude_46_List_46_foldrImpl(loc3, loc4, loc0, loc1, loc5, loc2)

# {io_bind0}
def idris__123_io_95_bind0_125_(loc0, loc1, loc2, loc3, loc4, loc5, loc6):
  return idris__123_APPLY0_125_(loc4, loc6)

# Prelude.{putStr0}
def idris_Prelude_46__123_putStr0_125_(loc0, loc1):
  return sys.stdout.write(loc0)

# Main.{pythag0}
def idris_Main_46__123_pythag0_125_(loc0):
  loc1 = (0,)
  return (1, loc0, loc1)

# {runMain0}
def idris__123_runMain0_125_():
  loc0 = idris_Main_46_main()
  loc1 = None
  loc0 = idris__123_APPLY0_125_(loc0, loc1)
  return idris__123_EVAL0_125_(loc0)

# Prelude.Classes.{Int instance of Prelude.Classes.Ord1}
def idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord1_125_(loc0):
  return (65632, loc0)

# {PE_List a instance of Prelude.Show_f5d3ac2c1}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c1_125_(loc0):
  return str(loc0)

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=1}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_1_125_(loc0, loc1):
  return (65636, loc0, loc1)

# {io_bind1}
def idris__123_io_95_bind1_125_(loc0, loc1, loc2, loc3, loc4, loc5, loc6):
  loc7 = idris__123_io_95_bind0_125_(loc0, loc1, loc2, loc3, loc4, loc5, loc6)
  return idris__123_APPLY0_125_(loc7, loc5)

# Prelude.{putStr1}
def idris_Prelude_46__123_putStr1_125_(loc0):
  loc1 = None
  loc2 = None
  loc3 = (0,)
  return (65646, loc1, loc2, loc3)

# Main.{pythag1}
def idris_Main_46__123_pythag1_125_(loc0):
  return (65622,)

# Prelude.Classes.{Int instance of Prelude.Classes.Ord2}
def idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord2_125_(loc0, loc1):
  loc2 = None
  loc3 = idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int()
  loc2 = idris_Prelude_46_Classes_46_compare(loc2, loc3)
  loc2 = idris__123_APPLY0_125_(loc2, loc0)
  loc2 = idris__123_APPLY0_125_(loc2, loc1)
  if loc2[0] == 0:
    return (1,)
  else:
    return (0,)

# {PE_List a instance of Prelude.Show_f5d3ac2c2}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c2_125_(loc0):
  return str(loc0)

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=2}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_2_125_(loc0):
  return (65637, loc0)

# {io_bind2}
def idris__123_io_95_bind2_125_(loc0, loc1, loc2, loc3, loc4, loc5):
  return (65652, loc0, loc1, loc2, loc3, loc4, loc5)

# Main.{pythag2}
def idris_Main_46__123_pythag2_125_(loc0):
  return (0,)

# Prelude.Classes.{Int instance of Prelude.Classes.Ord3}
def idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord3_125_(loc0):
  return (65634, loc0)

# {PE_List a instance of Prelude.Show_f5d3ac2c3}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c3_125_(loc0):
  loc1 = None
  loc2 = None
  loc3 = None
  loc4 = None
  loc5 = (65648,)
  loc6 = (65649,)
  return idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(loc1, loc2, loc3, loc4, loc5, loc6, loc0)

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=3}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_3_125_(loc0):
  return (65638,)

# Main.{pythag3}
def idris_Main_46__123_pythag3_125_(loc0, loc1, loc2, loc3):
  loc4 = (0, loc1, loc2)
  loc4 = (0, loc0, loc4)
  loc5 = (0,)
  return (1, loc4, loc5)

# {PE_List a instance of Prelude.Show_f5d3ac2c4}
def idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c4_125_(loc0):
  loc1 = None
  loc2 = None
  loc3 = None
  loc4 = None
  loc5 = (65647,)
  loc6 = (65650,)
  return idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(loc1, loc2, loc3, loc4, loc5, loc6, loc0)

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=4}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_4_125_(loc0):
  return (65639,)

# Main.{pythag4}
def idris_Main_46__123_pythag4_125_(loc0, loc1, loc2):
  loc3 = None
  loc4 = None
  loc5 = None
  loc6 = (65623,)
  loc7 = (65624,)
  loc6 = (0, loc6, loc7)
  loc7 = loc2 * loc2
  loc8 = loc0 * loc0
  loc7 = loc7 + loc8
  loc8 = loc1 * loc1
  loc7 = loc7 == loc8
  if loc7 == 0:
    loc7 = (0,)
  else:
    loc7 = (1,)
  loc5 = idris_Prelude_46_Applicative_46_guard(loc5, loc6, loc7)
  loc6 = (65625, loc2, loc0, loc1)
  return idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(loc3, loc4, loc5, loc6)

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=5}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_5_125_(loc0, loc1):
  loc2 = None
  return idris_Prelude_46_List_46__43__43_(loc2, loc0, loc1)

# Main.{pythag5}
def idris_Main_46__123_pythag5_125_(loc0, loc1):
  loc2 = None
  loc3 = None
  loc4 = 1
  loc4 = idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(loc4, loc1)
  loc5 = (65626, loc1, loc0)
  return idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(loc2, loc3, loc4, loc5)

# Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=6}
def idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_6_125_(loc0):
  return (65641, loc0)

# Main.{pythag6}
def idris_Main_46__123_pythag6_125_(loc0):
  loc1 = None
  loc2 = None
  loc3 = 1
  loc3 = idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(loc3, loc0)
  loc4 = (65627, loc0)
  return idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(loc1, loc2, loc3, loc4)

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
  if loc3 == 0:
    return (1, loc4, loc2)
  else:
    loc5 = 1
    loc5 = loc3 - loc5
    loc6 = None
    loc7 = None
    loc8 = (1, loc4, loc2)
    loc9 = 1
    loc9 = loc4 - loc9
    return idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0_58_go_58_0(loc6, loc7, loc8, loc5, loc9)

# Prelude.Prelude.List a instance of Prelude.Show, method show, show'
def idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(loc0, loc1, loc2, loc3, loc4, loc5):
  if loc5[0] == 1:
    loc6, loc7, = loc5[1:]
    if loc7[0] == 0:
      loc8 = None
      loc8 = idris_Prelude_46_show(loc8, loc3)
      loc8 = idris__123_APPLY0_125_(loc8, loc6)
      return loc4 + loc8
    else:
      loc8 = None
      loc9 = None
      loc10 = None
      loc11 = None
      loc11 = idris_Prelude_46_show(loc11, loc3)
      loc11 = idris__123_APPLY0_125_(loc11, loc6)
      loc12 = ", "
      loc11 = loc11 + loc12
      loc11 = loc4 + loc11
      return idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(loc8, loc9, loc10, loc3, loc11, loc7)
  elif loc5[0] == 0:
    return loc4
  else:
    return idris_error("unreachable case")

# Prelude.Prelude.Int instance of Prelude.Enum, method enumFromTo
def idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(loc0, loc1):
  loc2 = idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__61__58_0(loc0, loc1)
  if loc2[0] == 0:
    return (0,)
  elif loc2[0] == 1:
    loc3 = None
    loc4 = None
    loc5 = (0,)
    loc6 = loc1 - loc0
    loc6 = loc6
    return idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0_58_go_58_0(loc3, loc4, loc5, loc6, loc1)
  else:
    return idris_error("unreachable case")

# Prelude.Monad.Prelude.List instance of Prelude.Monad.Monad, method >>=
def idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(loc0, loc1, loc2, loc3):
  loc4 = None
  loc5 = None
  loc6 = None
  loc7 = (65640,)
  loc8 = (65642,)
  loc9 = (0,)
  loc8 = (0, loc8, loc9)
  loc4 = idris_Prelude_46_Foldable_46_concatMap(loc4, loc5, loc6, loc7, loc8, loc3)
  return idris__123_APPLY0_125_(loc4, loc2)

# Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=
def idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__61__58_0(loc0, loc1):
  loc2 = None
  loc3 = idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int()
  loc2 = idris_Prelude_46_Classes_46__60_(loc2, loc3)
  loc2 = idris__123_APPLY0_125_(loc2, loc0)
  loc2 = idris__123_APPLY0_125_(loc2, loc1)
  if loc2[0] == 0:
    return idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__60__61_0_125_(loc0, loc1)
  elif loc2[0] == 1:
    return (1,)
  else:
    return idris_error("unreachable case")

# Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method compare
def idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33_compare_58_0(loc0, loc1):
  loc2 = loc0 == loc1
  if loc2 == 0:
    loc2 = (0,)
  else:
    loc2 = (1,)
  if loc2[0] == 0:
    loc3 = loc0 < loc1
    if loc3 == 0:
      loc3 = (0,)
    else:
      loc3 = (1,)
    if loc3[0] == 0:
      return (2,)
    elif loc3[0] == 1:
      return (0,)
    else:
      return idris_error("unreachable case")
  elif loc2[0] == 1:
    return (1,)
  else:
    return idris_error("unreachable case")

# Prelude.Prelude.(a, b) instance of Prelude.Show, method show
def idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(loc0, loc1, loc2, loc3, loc4, loc5, loc6):
  if loc6[0] == 0:
    loc7, loc8, = loc6[1:]
    loc9 = "("
    loc10 = None
    loc10 = idris_Prelude_46_show(loc10, loc4)
    loc10 = idris__123_APPLY0_125_(loc10, loc7)
    loc11 = ", "
    loc12 = None
    loc12 = idris_Prelude_46_show(loc12, loc5)
    loc12 = idris__123_APPLY0_125_(loc12, loc8)
    loc13 = ")"
    loc12 = loc12 + loc13
    loc11 = loc11 + loc12
    loc10 = loc10 + loc11
    return loc9 + loc10
  else:
    return idris_error("unreachable case")

# Prelude.Prelude.List a instance of Prelude.Show, method show
def idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0(loc0, loc1, loc2, loc3):
  loc4 = "["
  loc5 = None
  loc6 = None
  loc7 = None
  loc8 = ""
  loc5 = idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(loc5, loc6, loc7, loc2, loc8, loc3)
  loc6 = "]"
  loc5 = loc5 + loc6
  return loc4 + loc5

# with block in Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <
def idris__95_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__58_0_95_with_95_85(loc0, loc1, loc2):
  if loc0[0] == 0:
    return (1,)
  else:
    return (0,)

# Prelude.Classes.Int instance of Prelude.Classes.Ord
def idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int():
  loc0 = (65633,)
  loc1 = (65635,)
  return (0, loc0, loc1)

# case block in Void
def idris_Void_95_case():
  return None

# case block in io_bind
def idris_io_95_bind_95_case(loc0, loc1, loc2, loc3, loc4, loc5, loc6, loc7):
  return idris__123_APPLY0_125_(loc7, loc5)

# <<Void eliminator>>
def idris_Void_95_elim():
  return None

if __name__ == '__main__':
  idris__123_runMain0_125_()