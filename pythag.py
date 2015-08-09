#!/usr/bin/env python

import sys
import importlib

class UnitType:
  pass

class WorldType:
  pass

Unit = UnitType()
World = WorldType()

class IdrisError(Exception):
  pass

def _idris_error(msg):
  raise IdrisError(msg)

def _idris_pymodule(name):
  return importlib.import_module(name)

def _idris_call(f, args):
  return f(*list(args))

def _idris_foreach(it, st, f):
  for x in it:
    # Apply st, x, world
    st = APPLY0(APPLY0(APPLY0(f, st), x), World)
  return st

def _idris_try(f, fail, succ):
  try:
    result = APPLY0(f, World)  # apply to world
    return APPLY0(succ, result)
  except Exception as e:
    return APPLY0(fail, e)

def _idris_raise(e):
  raise e

def _idris_marshal_PIO(action):
  return lambda: APPLY0(action, World)  # delayed apply-to-world

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

# Prelude.List.++
def _idris_Prelude_46_List_46__43__43_(e0, e1, e2):
  while True:
    if e1:  # Prelude.List.::
      in0, in1 = e1.head, e1.tail
      return _idris_Prelude_46_List_46__43__43_(None, in1, e2).cons(in0)
    else:  # Prelude.List.Nil
      return e2
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.<
def _idris_Prelude_46_Classes_46__60_(e0, e1):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Classes.Ord
    in0, in1 = e1[1:]
    return in1
    return _idris_error("unreachable due to case in tail position")

# Prelude.Algebra.<+>
def _idris_Prelude_46_Algebra_46__60__43__62_(e0, e1):
  while True:
    return e1

# Force
def _idris_Force(e0, e1, e2):
  while True:
    in0 = EVAL0(e2)
    return in0

# PE_(a, b) instance of Prelude.Show.Show_da00e731
def _idris_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_da00e731(
  meth0
):
  while True:
    return _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
      None,
      None,
      None,
      None,
      (65642,),  # {U_{PE_(a, b) instance of Prelude.Show.Show_da00e7310}1}
      (65643,),  # {U_{PE_(a, b) instance of Prelude.Show.Show_da00e7311}1}
      meth0
    )

# PE_@@constructor of Prelude.Algebra.Monoid#Semigroup a_42111bf0
def _idris_PE_95__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a_95_42111bf0(
  e0, meth0, meth1
):
  while True:
    return _idris_Prelude_46_List_46__43__43_(None, meth0, meth1)

# PE_@@constructor of Prelude.Applicative.Alternative#Applicative f_5102bba8
def _idris_PE_95__64__64_constructor_32_of_32_Prelude_46_Applicative_46_Alternative_35_Applicative_32_f_95_5102bba8(
  meth0, meth1
):
  while True:
    return ConsList().cons(meth1)

# PE_@@constructor of Prelude.Monad.Monad#Applicative m_d05ad59e
def _idris_PE_95__64__64_constructor_32_of_32_Prelude_46_Monad_46_Monad_35_Applicative_32_m_95_d05ad59e(
  meth0, meth1
):
  while True:
    return ConsList().cons(meth1)

# PE_Prelude.Show.(a, b) instance of Prelude.Show.Show, method show_cfed4029
def _idris_PE_95_Prelude_46_Show_46__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_cfed4029(
  e0
):
  while True:
    assert True  # Builtins.MkPair
    in0, in1 = e0
    return ("(" + (str(in0) + (", " + (str(in1) + ")"))))
    return _idris_error("unreachable due to case in tail position")

# PE_Prelude.Show.List a instance of Prelude.Show.Show, method show_6d67ca7a
def _idris_PE_95_Prelude_46_Show_46_List_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_6d67ca7a(
  e0
):
  while True:
    return ("[" + (_idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
      None,
      None,
      None,
      (65629,),  # {U_PE_(a, b) instance of Prelude.Show.Show_da00e7311}
      "",
      e0
    ) + "]"))

# PE_concatMap_fdbcf7d2
def _idris_PE_95_concatMap_95_fdbcf7d2(e0, e1, e2, e3):
  while True:
    return _idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
      None,
      None,
      (65644, e2),  # {U_{PE_concatMap_fdbcf7d20}1}
      _idris_PE_95_neutral_95_42111bf0(None),
      e3
    )

# PE_empty_8ff8f7b3
def _idris_PE_95_empty_95_8ff8f7b3(e0):
  while True:
    return ConsList()

# PE_neutral_42111bf0
def _idris_PE_95_neutral_95_42111bf0(e0):
  while True:
    return ConsList()

# PE_print_c5315c5e
def _idris_PE_95_print_95_c5315c5e(e0, e1):
  while True:
    return (65640, None, None, None, (65645, e1), (65646,))  # {U_io_bind1}, {U_{PE_print_c5315c5e0}1}, {U_{PE_print_c5315c5e1}1}

# PE_show_78b4bfbe
def _idris_PE_95_show_95_78b4bfbe(e0):
  while True:
    return _idris_PE_95_Prelude_46_Show_46_List_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_6d67ca7a(
      e0
    )

# call__IO
def _idris_call_95__95_IO(e0, e1, e2):
  while True:
    return APPLY0(e2, None)

# Prelude.Classes.compare
def _idris_Prelude_46_Classes_46_compare(e0, e1):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Classes.Ord
    in0, in1 = e1[1:]
    return in0
    return _idris_error("unreachable due to case in tail position")

# Prelude.Foldable.foldr
def _idris_Prelude_46_Foldable_46_foldr(e0, e1, e2, e3):
  while True:
    return APPLY0(APPLY0(e3, e1), e2)

# Prelude.Bool.ifThenElse
def _idris_Prelude_46_Bool_46_ifThenElse(e0, e1, e2, e3):
  while True:
    if not e1:  # Prelude.Bool.False
      return EVAL0(e3)
    else:  # Prelude.Bool.True
      return EVAL0(e2)
    return _idris_error("unreachable due to case in tail position")

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
    return APPLY0(io_bind2(e0, e1, e2, e3, e4, _idris_w), APPLY0(e3, _idris_w))

# io_return
def _idris_io_95_return(e0, e1, e2, _idris_w):
  while True:
    return e2

# Main.main
def _idris_Main_46_main():
  while True:
    return (
      65640,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_PE_95_print_95_c5315c5e(None, _idris_Main_46_pythag(200)),
      (65624,)  # {U_Main.{main0}1}
    )

# mkForeignPrim
def _idris_mkForeignPrim():
  while True:
    return None

# prim__addInt
def _idris_prim_95__95_addInt(op0, op1):
  while True:
    return (op0 + op1)

# prim__concat
def _idris_prim_95__95_concat(op0, op1):
  while True:
    return (op0 + op1)

# prim__eqInt
def _idris_prim_95__95_eqInt(op0, op1):
  while True:
    return (op0 == op1)

# prim__mulInt
def _idris_prim_95__95_mulInt(op0, op1):
  while True:
    return (op0 * op1)

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

# prim__sextInt_BigInt
def _idris_prim_95__95_sextInt_95_BigInt(op0):
  while True:
    return op0

# prim__sltInt
def _idris_prim_95__95_sltInt(op0, op1):
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

# prim__subInt
def _idris_prim_95__95_subInt(op0, op1):
  while True:
    return (op0 - op1)

# prim__toStrInt
def _idris_prim_95__95_toStrInt(op0):
  while True:
    return str(op0)

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
    return APPLY0(e3, e2)

# Prelude.Applicative.pure
def _idris_Prelude_46_Applicative_46_pure(e0, e1, e2):
  while True:
    return APPLY0(e2, e1)

# Prelude.putStr
def _idris_Prelude_46_putStr(e0, e1):
  while True:
    return (65640, None, None, None, (65638, e1), (65639,))  # {U_io_bind1}, {U_Prelude.{putStr0}1}, {U_Prelude.{putStr1}1}

# Main.pythag
def _idris_Main_46_pythag(e0):
  while True:
    return _idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
      None,
      None,
      _idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(
        1, e0
      ),
      (65628,)  # {U_Main.{pythag3}1}
    )

# run__IO
def _idris_run_95__95_IO(e0, e1):
  while True:
    return APPLY0(e1, None)

# Prelude.Show.show
def _idris_Prelude_46_Show_46_show(e0, e1):
  while True:
    return e1

# unsafePerformPrimIO
def _idris_unsafePerformPrimIO():
  while True:
    return None

# world
def _idris_world(e0):
  while True:
    return e0

# Prelude.Bool.||
def _idris_Prelude_46_Bool_46__124__124_(e0, e1):
  while True:
    if not e0:  # Prelude.Bool.False
      return EVAL0(e1)
    else:  # Prelude.Bool.True
      return True
    return _idris_error("unreachable due to case in tail position")

# {APPLY0}
def APPLY0(fn0, arg0):
  while True:
    if fn0[0] < 65640:
      if fn0[0] < 65632:
        if fn0[0] < 65628:
          if fn0[0] < 65626:
            if fn0[0] == 65624:  # {U_Main.{main0}1}
              return _idris_Main_46__123_main0_125_(arg0)
            else:  # {U_Main.{pythag0}1}
              P_c0, P_c1, P_c2 = fn0[1:]
              return _idris_Main_46__123_pythag0_125_(P_c0, P_c1, P_c2, arg0)
          else:
            if fn0[0] == 65626:  # {U_Main.{pythag1}1}
              P_c0, P_c1 = fn0[1:]
              return _idris_Main_46__123_pythag1_125_(P_c0, P_c1, arg0)
            else:  # {U_Main.{pythag2}1}
              P_c0 = fn0[1]
              return _idris_Main_46__123_pythag2_125_(P_c0, arg0)
        else:
          if fn0[0] < 65630:
            if fn0[0] == 65628:  # {U_Main.{pythag3}1}
              return _idris_Main_46__123_pythag3_125_(arg0)
            else:  # {U_PE_(a, b) instance of Prelude.Show.Show_da00e7311}
              return _idris_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_da00e731(
                arg0
              )
          else:
            if fn0[0] == 65630:  # {U_PE_@@constructor of Prelude.Algebra.Monoid#Semigroup a_42111bf01}
              P_c0, P_c1 = fn0[1:]
              return _idris_PE_95__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a_95_42111bf0(
                P_c0, P_c1, arg0
              )
            else:  # {U_PE_@@constructor of Prelude.Applicative.Alternative#Applicative f_5102bba81}
              P_c0 = fn0[1]
              return _idris_PE_95__64__64_constructor_32_of_32_Prelude_46_Applicative_46_Alternative_35_Applicative_32_f_95_5102bba8(
                P_c0, arg0
              )
      else:
        if fn0[0] < 65636:
          if fn0[0] < 65634:
            if fn0[0] == 65632:  # {U_PE_@@constructor of Prelude.Monad.Monad#Applicative m_d05ad59e1}
              P_c0 = fn0[1]
              return _idris_PE_95__64__64_constructor_32_of_32_Prelude_46_Monad_46_Monad_35_Applicative_32_m_95_d05ad59e(
                P_c0, arg0
              )
            else:  # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam0}1}
              P_c0 = fn0[1]
              return _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
                P_c0, arg0
              )
          else:
            if fn0[0] == 65634:  # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam1}1}
              return _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
                arg0
              )
            else:  # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam2}1}
              P_c0 = fn0[1]
              return _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
                P_c0, arg0
              )
        else:
          if fn0[0] < 65638:
            if fn0[0] == 65636:  # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam3}1}
              return _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
                arg0
              )
            else:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable1}
              P_c0, P_c1, P_c2, P_c3 = fn0[1:]
              return _idris_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List(
                P_c0, P_c1, P_c2, P_c3, arg0
              )
          else:
            if fn0[0] == 65638:  # {U_Prelude.{putStr0}1}
              P_c0 = fn0[1]
              return _idris_Prelude_46__123_putStr0_125_(P_c0, arg0)
            else:  # {U_Prelude.{putStr1}1}
              return _idris_Prelude_46__123_putStr1_125_(arg0)
    else:
      if fn0[0] < 65648:
        if fn0[0] < 65644:
          if fn0[0] < 65642:
            if fn0[0] == 65640:  # {U_io_bind1}
              P_c0, P_c1, P_c2, P_c3, P_c4 = fn0[1:]
              return _idris_io_95_bind(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
            else:  # {U_io_return1}
              P_c0, P_c1, P_c2 = fn0[1:]
              return _idris_io_95_return(P_c0, P_c1, P_c2, arg0)
          else:
            if fn0[0] == 65642:  # {U_{PE_(a, b) instance of Prelude.Show.Show_da00e7310}1}
              return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_da00e7310_125_(
                arg0
              )
            else:  # {U_{PE_(a, b) instance of Prelude.Show.Show_da00e7311}1}
              return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_da00e7311_125_(
                arg0
              )
        else:
          if fn0[0] < 65646:
            if fn0[0] == 65644:  # {U_{PE_concatMap_fdbcf7d20}1}
              P_c0 = fn0[1]
              return _idris__123_PE_95_concatMap_95_fdbcf7d20_125_(P_c0, arg0)
            else:  # {U_{PE_print_c5315c5e0}1}
              P_c0 = fn0[1]
              return _idris__123_PE_95_print_95_c5315c5e0_125_(P_c0, arg0)
          else:
            if fn0[0] == 65646:  # {U_{PE_print_c5315c5e1}1}
              return _idris__123_PE_95_print_95_c5315c5e1_125_(arg0)
            else:  # {U_{io_bind1}1}
              P_c0, P_c1, P_c2, P_c3, P_c4, P_c5 = fn0[1:]
              return io_bind1(P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, arg0)
      else:
        if fn0[0] < 65652:
          if fn0[0] < 65650:
            if fn0[0] == 65648:  # {U_PE_@@constructor of Prelude.Algebra.Monoid#Semigroup a_42111bf02}
              P_c0 = fn0[1]
              return (65630, P_c0, arg0)  # {U_PE_@@constructor of Prelude.Algebra.Monoid#Semigroup a_42111bf01}
            else:  # {U_PE_@@constructor of Prelude.Applicative.Alternative#Applicative f_5102bba82}
              return (65631, arg0)  # {U_PE_@@constructor of Prelude.Applicative.Alternative#Applicative f_5102bba81}
          else:
            if fn0[0] == 65650:  # {U_PE_@@constructor of Prelude.Monad.Monad#Applicative m_d05ad59e2}
              return (65632, arg0)  # {U_PE_@@constructor of Prelude.Monad.Monad#Applicative m_d05ad59e1}
            else:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable2}
              P_c0, P_c1, P_c2 = fn0[1:]
              return (65637, P_c0, P_c1, P_c2, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable1}
        else:
          if fn0[0] < 65654:
            if fn0[0] == 65652:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable3}
              P_c0, P_c1 = fn0[1:]
              return (65651, P_c0, P_c1, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable2}
            else:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable4}
              P_c0 = fn0[1]
              return (65652, P_c0, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable3}
          else:
            assert fn0[0] == 65654  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable5}
            return (65653, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable4}
    return _idris_error("unreachable due to case in tail position")

# {EVAL0}
def EVAL0(arg0):
  while True:
    return arg0

# Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam0}
def _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
  in0, in1
):
  while True:
    return _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33_compare_58_0(
      in0, in1
    )

# {PE_(a, b) instance of Prelude.Show.Show_da00e7310}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_da00e7310_125_(
  in0
):
  while True:
    return str(in0)

# {PE_concatMap_fdbcf7d20}
def _idris__123_PE_95_concatMap_95_fdbcf7d20_125_(e2, in0):
  while True:
    return APPLY0(
      _idris_Prelude_46_Algebra_46__60__43__62_(None, (65648, None)),  # {U_PE_@@constructor of Prelude.Algebra.Monoid#Semigroup a_42111bf02}
      APPLY0(e2, in0)
    )

# {PE_print_c5315c5e0}
def _idris__123_PE_95_print_95_c5315c5e0_125_(e1, in0):
  while True:
    return sys.stdout.write(_idris_PE_95_show_95_78b4bfbe(e1))

# Prelude.Classes.{Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=_lam0}
def _idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__60__61__95_lam0_125_(
  e0, e1
):
  while True:
    aux1 = (e0 == e1)
    if aux1 == 0:
      return False
    else:
      return True
    return _idris_error("unreachable due to case in tail position")

# {io_bind0}
def io_bind0(e0, e1, e2, e3, e4, _idris_w, in0):
  while True:
    return APPLY0(e4, in0)

# Main.{main0}
def _idris_Main_46__123_main0_125_(in0):
  while True:
    return _idris_Prelude_46_putStr(None, "\n")

# Prelude.{putStr0}
def _idris_Prelude_46__123_putStr0_125_(e1, in0):
  while True:
    return sys.stdout.write(e1)

# Main.{pythag0}
def _idris_Main_46__123_pythag0_125_(in2, in1, in0, in3):
  while True:
    return APPLY0(
      _idris_Prelude_46_Applicative_46_pure(None, None, (65650,)),  # {U_PE_@@constructor of Prelude.Monad.Monad#Applicative m_d05ad59e2}
      (in2, (in1, in0))
    )

# {runMain0}
def runMain0():
  while True:
    return EVAL0(APPLY0(_idris_Main_46_main(), None))

# Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam1}
def _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
  in0
):
  while True:
    return (65633, in0)  # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam0}1}

# {PE_(a, b) instance of Prelude.Show.Show_da00e7311}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_da00e7311_125_(
  in1
):
  while True:
    return _idris_PE_95_Prelude_46_Show_46__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_cfed4029(
      in1
    )

# {PE_print_c5315c5e1}
def _idris__123_PE_95_print_95_c5315c5e1_125_(in1):
  while True:
    return (65641, None, None, Unit)  # {U_io_return1}

# {io_bind1}
def io_bind1(e0, e1, e2, e3, e4, _idris_w, in0):
  while True:
    return APPLY0(io_bind0(e0, e1, e2, e3, e4, _idris_w, in0), _idris_w)

# Prelude.{putStr1}
def _idris_Prelude_46__123_putStr1_125_(in1):
  while True:
    return (65641, None, None, Unit)  # {U_io_return1}

# Main.{pythag1}
def _idris_Main_46__123_pythag1_125_(in1, in0, in2):
  while True:
    aux2 = (((in2 * in2) + (in1 * in1)) == (in0 * in0))
    if aux2 == 0:
      aux3 = False
    else:
      aux3 = True
    aux1 = aux3
    if not aux1:  # Prelude.Bool.False
      aux4 = _idris_PE_95_empty_95_8ff8f7b3(None)
    else:  # Prelude.Bool.True
      aux4 = APPLY0(_idris_Prelude_46_Applicative_46_pure(None, None, (65649,)), Unit)  # {U_PE_@@constructor of Prelude.Applicative.Alternative#Applicative f_5102bba82}
    return _idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
      None,
      None,
      aux4,
      (65625, in2, in1, in0)  # {U_Main.{pythag0}1}
    )

# Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam2}
def _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
  in2, in3
):
  while True:
    aux1 = APPLY0(
      APPLY0(
        _idris_Prelude_46_Classes_46_compare(
          None,
          _idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int()
        ),
        in2
      ),
      in3
    )
    if aux1[0] == 0:  # Prelude.Classes.LT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# {io_bind2}
def io_bind2(e0, e1, e2, e3, e4, _idris_w):
  while True:
    return (65647, e0, e1, e2, e3, e4, _idris_w)  # {U_{io_bind1}1}

# Main.{pythag2}
def _idris_Main_46__123_pythag2_125_(in0, in1):
  while True:
    return _idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
      None,
      None,
      _idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(
        1, in1
      ),
      (65626, in1, in0)  # {U_Main.{pythag1}1}
    )

# Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam3}
def _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
  in2
):
  while True:
    return (65635, in2)  # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam2}1}

# Main.{pythag3}
def _idris_Main_46__123_pythag3_125_(in0):
  while True:
    return _idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
      None,
      None,
      _idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(
        1, in0
      ),
      (65627, in0)  # {U_Main.{pythag2}1}
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

# Decidable.Equality.Decidable.Equality.String instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_String_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Prelude.Prelude.Int instance of Prelude.Enum, method enumFromTo, go
def _idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0_58_go_58_0(
  e0, e1, e2, e3, e4
):
  while True:
    if e3 == 0:
      return e2.cons(e4)
    else:
      in0 = (e3 - 1)
      e0, e1, e2, e3, e4, = None, None, e2.cons(e4), in0, (e4 - 1),
      continue
      return _idris_error("unreachable due to tail call")
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.Prelude.Show.List a instance of Prelude.Show.Show, method show, show'
def _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
  e0, e1, e2, e3, e4, e5
):
  while True:
    if e5:  # Prelude.List.::
      in0, in1 = e5.head, e5.tail
      if not in1:  # Prelude.List.Nil
        return (e4 + APPLY0(_idris_Prelude_46_Show_46_show(None, e3), in0))
      else:
        e0, e1, e2, e3, e4, e5, = None, None, None, e3, (e4 + (APPLY0(_idris_Prelude_46_Show_46_show(None, e3), in0) + ", ")), in1,
        continue
        return _idris_error("unreachable due to tail call")
      return _idris_error("unreachable due to case in tail position")
    else:  # Prelude.List.Nil
      return e4
    return _idris_error("unreachable due to case in tail position")

# Prelude.Prelude.Int instance of Prelude.Enum, method enumFromTo
def _idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(
  e0, e1
):
  while True:
    aux1 = _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__61__58_0(
      e0, e1
    )
    if not aux1:  # Prelude.Bool.False
      return ConsList()
    else:  # Prelude.Bool.True
      return _idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0_58_go_58_0(
        None,
        None,
        ConsList(),
        (e1 - e0),
        e1
      )
    return _idris_error("unreachable due to case in tail position")

# Prelude.Foldable.Prelude.List.List instance of Prelude.Foldable.Foldable, method foldr
def _idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
  e0, e1, e2, e3, e4
):
  while True:
    if e4:  # Prelude.List.::
      in0, in1 = e4.head, e4.tail
      return APPLY0(
        APPLY0(e2, in0),
        APPLY0(
          APPLY0(
            APPLY0(_idris_Prelude_46_Foldable_46_foldr(None, None, None, (65654,)), e2),  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable5}
            e3
          ),
          in1
        )
      )
    else:  # Prelude.List.Nil
      return e3
    return _idris_error("unreachable due to case in tail position")

# Prelude.Monad.Prelude.List instance of Prelude.Monad.Monad, method >>=
def _idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
  e0, e1, e2, e3
):
  while True:
    return _idris_PE_95_concatMap_95_fdbcf7d2(None, None, e3, e2)

# Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=
def _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__61__58_0(
  e0, e1
):
  while True:
    aux1 = APPLY0(
      APPLY0(
        _idris_Prelude_46_Classes_46__60_(
          None,
          _idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int()
        ),
        e0
      ),
      e1
    )
    if not aux1:  # Prelude.Bool.False
      return _idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__60__61__95_lam0_125_(
        e0, e1
      )
    else:  # Prelude.Bool.True
      return True
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method compare
def _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33_compare_58_0(
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

# Prelude.Show.Prelude.Show.(a, b) instance of Prelude.Show.Show, method show
def _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
  e0, e1, e2, e3, e4, e5, e6
):
  while True:
    assert True  # Builtins.MkPair
    in0, in1 = e6
    return ("(" + (APPLY0(_idris_Prelude_46_Show_46_show(None, e4), in0) + (", " + (APPLY0(_idris_Prelude_46_Show_46_show(None, e5), in1) + ")"))))
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <
def _idris__95_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__58_0_95_with_95_87(
  e0, e1, e2
):
  while True:
    if e0[0] == 0:  # Prelude.Classes.LT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# Prelude.List.List instance of Prelude.Foldable.Foldable
def _idris_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List(
  meth0, meth1, meth2, meth3, meth4
):
  while True:
    return _idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
      None, None, meth2, meth3, meth4
    )

# Prelude.Classes.Int instance of Prelude.Classes.Ord
def _idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int():
  while True:
    return (0, (65634,), (65636,))  # constructor of Prelude.Classes.Ord, {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam1}1}, {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam3}1}

# case block in Void
def _idris_Void_95_case():
  while True:
    return None

# case block in io_bind
def _idris_io_95_bind_95_case(e0, e1, e2, e3, e4, e5, e6, e7):
  while True:
    return APPLY0(e7, e5)

# <<Void eliminator>>
def _idris_Void_95_elim():
  while True:
    return None

if __name__ == '__main__':
  runMain0()
