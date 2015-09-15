#!/usr/bin/env python

import sys
import importlib

Unit = object()
World = object()

class IdrisError(Exception):
  pass

def _idris_error(msg):
  raise IdrisError(msg)

def _idris_call(f, args):
  return f(*list(args))

def _idris_foreach(it, st, f):
  for x in it:
    # Apply st, x, world
    st = APPLY0(APPLY0(APPLY0(f, st), x), World)
  return st

def _idris_try(f, succ, fail):
  try:
    result = APPLY0(f, World)  # apply to world
    return APPLY0(succ, result)
  except Exception as e:
    # provide the exception class name + the exception itself
    return APPLY0(APPLY0(fail, e.__class__.__name__), e)

def _idris_raise(e):
  raise e

def _idris_marshal_IO(action):
  return lambda: APPLY0(action, World)  # delayed apply-to-world

def _idris_get_global(name):
  return globals()[name]

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

# Prelude.Bool.&&
def _idris_Prelude_46_Bool_46__38__38_(e0, e1):
  while True:
    if not e0:  # Prelude.Bool.False
      return False
    else:  # Prelude.Bool.True
      return EVAL0(e1)
    return _idris_error("unreachable due to case in tail position")

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
    in0, in1, in2 = e1[1:]
    return in1
    return _idris_error("unreachable due to case in tail position")

# Prelude.Algebra.<+>
def _idris_Prelude_46_Algebra_46__60__43__62_(e0, e1):
  while True:
    return e1

# Prelude.Classes.==
def _idris_Prelude_46_Classes_46__61__61_(e0, e1):
  while True:
    return e1

# Prelude.Classes.>
def _idris_Prelude_46_Classes_46__62_(e0, e1):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Classes.Ord
    in0, in1, in2 = e1[1:]
    return in2
    return _idris_error("unreachable due to case in tail position")

# Force
def _idris_Force(e0, e1, e2):
  while True:
    in0 = EVAL0(e2)
    return in0

# PE_@@constructor of Prelude.Algebra.Monoid#Semigroup a_0
def _idris_PE_95__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a_95_0(
  e0, meth0, meth1
):
  while True:
    return _idris_Prelude_46_List_46__43__43_(None, meth0, meth1)

# PE_@@constructor of Prelude.Applicative.Alternative#Applicative f_0
def _idris_PE_95__64__64_constructor_32_of_32_Prelude_46_Applicative_46_Alternative_35_Applicative_32_f_95_0(
  meth0, meth1
):
  while True:
    return ConsList().cons(meth1)

# PE_concatMap_0
def _idris_PE_95_concatMap_95_0(e0, e1, e2, e3):
  while True:
    return _idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
      None,
      None,
      (_idris__123_U_95__123_PE_95_concatMap_95_00_125_1_125_, e2,),
      _idris_PE_95_neutral_95_0(None),
      e3
    )

# PE_empty_0
def _idris_PE_95_empty_95_0(e0):
  while True:
    return ConsList()

# PE_neutral_0
def _idris_PE_95_neutral_95_0(e0):
  while True:
    return ConsList()

# PE_print_0
def _idris_PE_95_print_95_0(e0, e1):
  while True:
    return (U_io_bind1, None, None, None, (_idris__123_U_95__123_PE_95_print_95_039_125_1_125_, e1,), (_idris__123_U_95__123_PE_95_print_95_040_125_1_125_,),)

# call__IO
def _idris_call_95__95_IO(e0, e1, e2):
  while True:
    aux1 = e2
    return aux1[0](aux1, None)  # APPLY

# Prelude.Classes.compare
def _idris_Prelude_46_Classes_46_compare(e0, e1):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Classes.Ord
    in0, in1, in2 = e1[1:]
    return in0
    return _idris_error("unreachable due to case in tail position")

# Prelude.Foldable.foldr
def _idris_Prelude_46_Foldable_46_foldr(e0, e1, e2, e3):
  while True:
    aux2 = e3
    aux1 = aux2[0](aux2, e1)  # APPLY
    return aux1[0](aux1, e2)  # APPLY

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
    aux1 = io_bind2(e0, e1, e2, e3, e4, _idris_w)
    aux2 = e3
    return aux1[0](aux1, aux2[0](aux2, _idris_w))  # APPLY, APPLY

# io_return
def _idris_io_95_return(e0, e1, e2, _idris_w):
  while True:
    return e2

# Main.main
def _idris_Main_46_main():
  while True:
    return (U_io_bind1, None, None, None, _idris_PE_95_print_95_0(None, _idris_Main_46_pythag(75)), (_idris__123_U_95_Main_46__123_main0_125_1_125_,),)

# mkForeignPrim
def _idris_mkForeignPrim():
  while True:
    return None

# Prelude.Bool.not
def _idris_Prelude_46_Bool_46_not(e0):
  while True:
    if not e0:  # Prelude.Bool.False
      return True
    else:  # Prelude.Bool.True
      return False
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.precCon
def _idris_Prelude_46_Show_46_precCon(e0):
  while True:
    if e0[0] == 6:  # Prelude.Show.App
      return 6
    elif e0[0] == 3:  # Prelude.Show.Backtick
      return 3
    elif e0[0] == 2:  # Prelude.Show.Dollar
      return 2
    elif e0[0] == 1:  # Prelude.Show.Eq
      return 1
    elif e0[0] == 0:  # Prelude.Show.Open
      return 0
    elif e0[0] == 5:  # Prelude.Show.PrefixMinus
      return 5
    else:  # Prelude.Show.User
      in0 = e0[1]
      return 4
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.primNumShow
def _idris_Prelude_46_Show_46_primNumShow(e0, e1, e2, e3):
  while True:
    aux1 = e1
    in0 = aux1[0](aux1, e3)  # APPLY
    aux3 = _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33__62__61__58_0(
      e2, (5,)  # Prelude.Show.PrefixMinus
    )
    if not aux3:  # Prelude.Bool.False
      aux4 = False
    else:  # Prelude.Bool.True
      aux4 = _idris_Prelude_46_Show_46__123_primNumShow2_125_(in0, e0, e1, e2, e3)
    aux2 = aux4
    if not aux2:  # Prelude.Bool.False
      return in0
    else:  # Prelude.Bool.True
      return ("(" + (in0 + ")"))
    return _idris_error("unreachable due to case in tail position")

# prim__addInt
def _idris_prim_95__95_addInt(op0, op1):
  while True:
    return (op0 + op1)

# prim__concat
def _idris_prim_95__95_concat(op0, op1):
  while True:
    return (op0 + op1)

# prim__eqBigInt
def _idris_prim_95__95_eqBigInt(op0, op1):
  while True:
    return (op0 == op1)

# prim__eqChar
def _idris_prim_95__95_eqChar(op0, op1):
  while True:
    return (op0 == op1)

# prim__eqInt
def _idris_prim_95__95_eqInt(op0, op1):
  while True:
    return (op0 == op1)

# prim__eqManagedPtr
def _idris_prim_95__95_eqManagedPtr(op0, op1):
  while True:
    return _idris_error("unimplemented external: prim__eqManagedPtr")

# prim__eqPtr
def _idris_prim_95__95_eqPtr(op0, op1):
  while True:
    return _idris_error("unimplemented external: prim__eqPtr")

# prim__eqString
def _idris_prim_95__95_eqString(op0, op1):
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

# prim__sltBigInt
def _idris_prim_95__95_sltBigInt(op0, op1):
  while True:
    return (op0 < op1)

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

# prim__strHead
def _idris_prim_95__95_strHead(op0):
  while True:
    return op0[0]

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
    aux1 = e3
    return aux1[0](aux1, e2)  # APPLY

# Prelude.Applicative.pure
def _idris_Prelude_46_Applicative_46_pure(e0, e1, e2):
  while True:
    aux1 = e2
    return aux1[0](aux1, e1)  # APPLY

# Prelude.Interactive.putStr
def _idris_Prelude_46_Interactive_46_putStr(e0, e1):
  while True:
    return (U_io_bind1, None, None, None, (_idris__123_U_95_Prelude_46_Interactive_46__123_putStr0_125_1_125_, e1,), (_idris__123_U_95_Prelude_46_Interactive_46__123_putStr1_125_1_125_,),)

# Main.pythag
def _idris_Main_46_pythag(e0):
  while True:
    return _idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
      None,
      None,
      _idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(
        1, e0
      ),
      (_idris__123_U_95_Main_46__123_pythag3_125_1_125_,)
    )

# really_believe_me
def _idris_really_95_believe_95_me(e0, e1, e2):
  while True:
    return e2

# run__IO
def _idris_run_95__95_IO(e0, e1):
  while True:
    aux1 = e1
    return aux1[0](aux1, None)  # APPLY

# Prelude.Show.show
def _idris_Prelude_46_Show_46_show(e0, e1):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Show.Show
    in0, in1 = e1[1:]
    return in0
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.showParens
def _idris_Prelude_46_Show_46_showParens(e0, e1):
  while True:
    if not e0:  # Prelude.Bool.False
      return e1
    else:  # Prelude.Bool.True
      return ("(" + (e1 + ")"))
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.showPrec
def _idris_Prelude_46_Show_46_showPrec(e0, e1):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Show.Show
    in0, in1 = e1[1:]
    return in1
    return _idris_error("unreachable due to case in tail position")

# Prelude.Strings.strM
def _idris_Prelude_46_Strings_46_strM(e0):
  while True:
    aux3 = (e0 == "")
    if aux3 == 0:
      aux4 = False
    else:
      aux4 = True
    aux2 = aux4
    if not aux2:  # Prelude.Bool.False
      aux5 = True
    else:  # Prelude.Bool.True
      aux5 = False
    aux1 = _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Bool_58__33_decEq_58_0(
      aux5, True
    )
    if aux1[0] == 1:  # Prelude.Basics.No
      return _idris_really_95_believe_95_me(None, None, (0,))  # Prelude.Strings.StrNil
    else:  # Prelude.Basics.Yes
      return _idris_really_95_believe_95_me(None, None, (1, e0[0]))  # Prelude.Strings.StrCons
    return _idris_error("unreachable due to case in tail position")

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

# Prelude.Show.{(a, b) instance of Prelude.Show.Show_lam0}
def _idris_Prelude_46_Show_46__123__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam0_125_(
  e2, e3, in0
):
  while True:
    return _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
      None, None, None, None, e2, e3, in0
    )

def _idris__123_U_95_Main_46__123_main0_125_1_125_(_idris_arg, arg0):
  return _idris_Main_46__123_main0_125_(arg0)

def _idris__123_U_95_Main_46__123_pythag0_125_1_125_(_idris_arg, arg0):
  P_c0, P_c1, P_c2 = _idris_arg[1:]
  return _idris_Main_46__123_pythag0_125_(P_c0, P_c1, P_c2, arg0)

def _idris__123_U_95_Main_46__123_pythag1_125_1_125_(_idris_arg, arg0):
  P_c0, P_c1 = _idris_arg[1:]
  return _idris_Main_46__123_pythag1_125_(P_c0, P_c1, arg0)

def _idris__123_U_95_Main_46__123_pythag2_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris_Main_46__123_pythag2_125_(P_c0, arg0)

def _idris__123_U_95_Main_46__123_pythag3_125_1_125_(_idris_arg, arg0):
  return _idris_Main_46__123_pythag3_125_(arg0)

def _idris__123_U_95_PE_95__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a_95_01_125_(_idris_arg, arg0):
  P_c0, P_c1 = _idris_arg[1:]
  return _idris_PE_95__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a_95_0(
    P_c0, P_c1, arg0
  )

def _idris__123_U_95_PE_95__64__64_constructor_32_of_32_Prelude_46_Applicative_46_Alternative_35_Applicative_32_f_95_01_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris_PE_95__64__64_constructor_32_of_32_Prelude_46_Applicative_46_Alternative_35_Applicative_32_f_95_0(
    P_c0, arg0
  )

def _idris__123_U_95_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
    P_c0, arg0
  )

def _idris__123_U_95_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_1_125_(_idris_arg, arg0):
  return _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
    arg0
  )

def _idris__123_U_95_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
    P_c0, arg0
  )

def _idris__123_U_95_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_1_125_(_idris_arg, arg0):
  return _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
    arg0
  )

def _idris__123_U_95_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_(
    P_c0, arg0
  )

def _idris__123_U_95_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_1_125_(_idris_arg, arg0):
  return _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_(
    arg0
  )

def _idris__123_U_95_Prelude_46_Interactive_46__123_putStr0_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Interactive_46__123_putStr0_125_(P_c0, arg0)

def _idris__123_U_95_Prelude_46_Interactive_46__123_putStr1_125_1_125_(_idris_arg, arg0):
  return _idris_Prelude_46_Interactive_46__123_putStr1_125_(arg0)

def _idris__123_U_95_Prelude_46_List_46_List_32_instance_32_of_32_Prelude_46_Foldable_46_Foldable1_125_(_idris_arg, arg0):
  P_c0, P_c1, P_c2, P_c3 = _idris_arg[1:]
  return _idris_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List(
    P_c0, P_c1, P_c2, P_c3, arg0
  )

def _idris__123_U_95_Prelude_46_Nat_46_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Eq1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Eq_36_Nat(P_c0, arg0)

def _idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
    P_c0, arg0
  )

def _idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_1_125_(_idris_arg, arg0):
  return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
    arg0
  )

def _idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
    P_c0, arg0
  )

def _idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_1_125_(_idris_arg, arg0):
  return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
    arg0
  )

def _idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_(
    P_c0, arg0
  )

def _idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_1_125_(_idris_arg, arg0):
  return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_(
    arg0
  )

def _idris__123_U_95_Prelude_46_Show_46__123__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam0_125_1_125_(_idris_arg, arg0):
  P_c0, P_c1 = _idris_arg[1:]
  return _idris_Prelude_46_Show_46__123__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam0_125_(
    P_c0, P_c1, arg0
  )

def _idris__123_U_95_Prelude_46_Show_46__123__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam1_125_1_125_(_idris_arg, arg0):
  P_c0, P_c1 = _idris_arg[1:]
  return _idris_Prelude_46_Show_46__123__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam1_125_(
    P_c0, P_c1, arg0
  )

def _idris__123_U_95_Prelude_46_Show_46__123__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam2_125_1_125_(_idris_arg, arg0):
  P_c0, P_c1 = _idris_arg[1:]
  return _idris_Prelude_46_Show_46__123__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam2_125_(
    P_c0, P_c1, arg0
  )

def _idris__123_U_95_Prelude_46_Show_46__123_Int_32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam0_125_1_125_(_idris_arg, arg0):
  return _idris_Prelude_46_Show_46__123_Int_32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam0_125_(
    arg0
  )

def _idris__123_U_95_Prelude_46_Show_46__123_Int_32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam1_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Show_46__123_Int_32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam1_125_(
    P_c0, arg0
  )

def _idris__123_U_95_Prelude_46_Show_46__123_Int_32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam2_125_1_125_(_idris_arg, arg0):
  return _idris_Prelude_46_Show_46__123_Int_32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam2_125_(
    arg0
  )

def _idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
    P_c0, arg0
  )

def _idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_1_125_(_idris_arg, arg0):
  return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
    arg0
  )

def _idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
    P_c0, arg0
  )

def _idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_1_125_(_idris_arg, arg0):
  return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
    arg0
  )

def _idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_(
    P_c0, arg0
  )

def _idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_1_125_(_idris_arg, arg0):
  return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_(
    arg0
  )

def _idris__123_U_95_Prelude_46_Show_46__123_primNumShow0_125_1_125_(_idris_arg, arg0):
  return _idris_Prelude_46_Show_46__123_primNumShow0_125_(arg0)

def U_io_bind1(_idris_arg, arg0):
  P_c0, P_c1, P_c2, P_c3, P_c4 = _idris_arg[1:]
  return _idris_io_95_bind(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)

def U_io_return1(_idris_arg, arg0):
  P_c0, P_c1, P_c2 = _idris_arg[1:]
  return _idris_io_95_return(P_c0, P_c1, P_c2, arg0)

def U_prim__toStrInt1(_idris_arg, arg0):
  return _idris_prim_95__95_toStrInt(arg0)

def _idris__123_U_95__123_PE_95_concatMap_95_00_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris__123_PE_95_concatMap_95_00_125_(P_c0, arg0)

def _idris__123_U_95__123_PE_95_print_95_00_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_00_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_010_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_010_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_011_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris__123_PE_95_print_95_011_125_(P_c0, arg0)

def _idris__123_U_95__123_PE_95_print_95_012_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_012_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_013_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_013_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_014_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris__123_PE_95_print_95_014_125_(P_c0, arg0)

def _idris__123_U_95__123_PE_95_print_95_015_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_015_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_016_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_016_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_017_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_017_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_018_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_018_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_019_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_019_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_01_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris__123_PE_95_print_95_01_125_(P_c0, arg0)

def _idris__123_U_95__123_PE_95_print_95_020_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris__123_PE_95_print_95_020_125_(P_c0, arg0)

def _idris__123_U_95__123_PE_95_print_95_021_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_021_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_022_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_022_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_023_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris__123_PE_95_print_95_023_125_(P_c0, arg0)

def _idris__123_U_95__123_PE_95_print_95_024_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_024_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_025_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_025_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_026_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris__123_PE_95_print_95_026_125_(P_c0, arg0)

def _idris__123_U_95__123_PE_95_print_95_027_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_027_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_028_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_028_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_029_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_029_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_02_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_02_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_030_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris__123_PE_95_print_95_030_125_(P_c0, arg0)

def _idris__123_U_95__123_PE_95_print_95_031_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_031_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_032_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_032_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_033_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris__123_PE_95_print_95_033_125_(P_c0, arg0)

def _idris__123_U_95__123_PE_95_print_95_034_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_034_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_035_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_035_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_036_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_036_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_037_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_037_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_038_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_038_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_039_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris__123_PE_95_print_95_039_125_(P_c0, arg0)

def _idris__123_U_95__123_PE_95_print_95_03_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_03_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_040_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_040_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_04_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris__123_PE_95_print_95_04_125_(P_c0, arg0)

def _idris__123_U_95__123_PE_95_print_95_05_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_05_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_06_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_06_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_07_125_1_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return _idris__123_PE_95_print_95_07_125_(P_c0, arg0)

def _idris__123_U_95__123_PE_95_print_95_08_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_08_125_(arg0)

def _idris__123_U_95__123_PE_95_print_95_09_125_1_125_(_idris_arg, arg0):
  return _idris__123_PE_95_print_95_09_125_(arg0)

def _idris__123_U_95__123_io_95_bind1_125_1_125_(_idris_arg, arg0):
  P_c0, P_c1, P_c2, P_c3, P_c4, P_c5 = _idris_arg[1:]
  return io_bind1(P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, arg0)

def _idris__123_U_95_PE_95__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a_95_02_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return (_idris__123_U_95_PE_95__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a_95_01_125_, P_c0, arg0,)

def _idris__123_U_95_PE_95__64__64_constructor_32_of_32_Prelude_46_Applicative_46_Alternative_35_Applicative_32_f_95_02_125_(_idris_arg, arg0):
  return (_idris__123_U_95_PE_95__64__64_constructor_32_of_32_Prelude_46_Applicative_46_Alternative_35_Applicative_32_f_95_01_125_, arg0,)

def _idris__123_U_95_Prelude_46_List_46_List_32_instance_32_of_32_Prelude_46_Foldable_46_Foldable2_125_(_idris_arg, arg0):
  P_c0, P_c1, P_c2 = _idris_arg[1:]
  return (_idris__123_U_95_Prelude_46_List_46_List_32_instance_32_of_32_Prelude_46_Foldable_46_Foldable1_125_, P_c0, P_c1, P_c2, arg0,)

def _idris__123_U_95_Prelude_46_Nat_46_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Eq2_125_(_idris_arg, arg0):
  return (_idris__123_U_95_Prelude_46_Nat_46_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Eq1_125_, arg0,)

def _idris__123_U_95_Prelude_46_List_46_List_32_instance_32_of_32_Prelude_46_Foldable_46_Foldable3_125_(_idris_arg, arg0):
  P_c0, P_c1 = _idris_arg[1:]
  return (_idris__123_U_95_Prelude_46_List_46_List_32_instance_32_of_32_Prelude_46_Foldable_46_Foldable2_125_, P_c0, P_c1, arg0,)

def _idris__123_U_95_Prelude_46_List_46_List_32_instance_32_of_32_Prelude_46_Foldable_46_Foldable4_125_(_idris_arg, arg0):
  P_c0 = _idris_arg[1]
  return (_idris__123_U_95_Prelude_46_List_46_List_32_instance_32_of_32_Prelude_46_Foldable_46_Foldable3_125_, P_c0, arg0,)

def _idris__123_U_95_Prelude_46_List_46_List_32_instance_32_of_32_Prelude_46_Foldable_46_Foldable5_125_(_idris_arg, arg0):
  return (_idris__123_U_95_Prelude_46_List_46_List_32_instance_32_of_32_Prelude_46_Foldable_46_Foldable4_125_, arg0,)

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

# Prelude.Show.{Int instance of Prelude.Show.Show_lam0}
def _idris_Prelude_46_Show_46__123_Int_32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam0_125_(
  in0
):
  while True:
    aux2 = _idris_Prelude_46_Show_46_showPrec(
      None,
      _idris_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_Int()
    )
    aux1 = aux2[0](aux2, (0,))  # APPLY, Prelude.Show.Open
    return aux1[0](aux1, in0)  # APPLY

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam0}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
  in0, in1
):
  while True:
    return _idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat_58__33_compare_58_0(
      in0, in1
    )

# {PE_concatMap_00}
def _idris__123_PE_95_concatMap_95_00_125_(e2, in0):
  while True:
    aux1 = _idris_Prelude_46_Algebra_46__60__43__62_(
      None,
      (_idris__123_U_95_PE_95__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a_95_02_125_, None,)
    )
    aux2 = e2
    return aux1[0](aux1, aux2[0](aux2, in0))  # APPLY, APPLY

# {PE_print_00}
def _idris__123_PE_95_print_95_00_125_(in2):
  while True:
    aux2 = _idris_Prelude_46_Show_46_showPrec(
      None,
      _idris_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_Int()
    )
    aux1 = aux2[0](aux2, (0,))  # APPLY, Prelude.Show.Open
    return aux1[0](aux1, in2)  # APPLY

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam0}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
  in0, in1
):
  while True:
    return _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33_compare_58_0(
      in0, in1
    )

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

# Prelude.Classes.{Prelude.Show.Prec instance of Prelude.Classes.Ord, method >=_lam0}
def _idris_Prelude_46_Classes_46__123_Prelude_46_Show_46_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__62__61__95_lam0_125_(
  e0, e1
):
  while True:
    return _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Eq_36_Prec_58__33__61__61__58_0(
      e0, e1
    )

# {io_bind0}
def io_bind0(e0, e1, e2, e3, e4, _idris_w, in0):
  while True:
    aux1 = e4
    return aux1[0](aux1, in0)  # APPLY

# Main.{main0}
def _idris_Main_46__123_main0_125_(in0):
  while True:
    return _idris_Prelude_46_Interactive_46_putStr(None, "\n")

# Prelude.Show.{primNumShow0}
def _idris_Prelude_46_Show_46__123_primNumShow0_125_(in1):
  while True:
    aux1 = (in1 == '-')
    if aux1 == 0:
      return False
    else:
      return True
    return _idris_error("unreachable due to case in tail position")

# Prelude.Interactive.{putStr0}
def _idris_Prelude_46_Interactive_46__123_putStr0_125_(e1, in0):
  while True:
    return sys.stdout.write(e1)

# Main.{pythag0}
def _idris_Main_46__123_pythag0_125_(in2, in1, in0, in3):
  while True:
    return ConsList().cons((in2, (in1, in0)))

# {runMain0}
def runMain0():
  while True:
    aux1 = _idris_Main_46_main()
    return EVAL0(aux1[0](aux1, None))  # APPLY

# Prelude.Show.{(a, b) instance of Prelude.Show.Show_lam1}
def _idris_Prelude_46_Show_46__123__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam1_125_(
  e2, e3, in2
):
  while True:
    aux1 = _idris_Prelude_46_Show_46_show(
      None,
      _idris_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36__40_a_44__32_b_41_(
        None, None, e2, e3
      )
    )
    return aux1[0](aux1, in2)  # APPLY

# Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam1}
def _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
  in0
):
  while True:
    return (_idris__123_U_95_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_1_125_, in0,)

# Prelude.Show.{Int instance of Prelude.Show.Show_lam1}
def _idris_Prelude_46_Show_46__123_Int_32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam1_125_(
  in1, in2
):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), in1, in2)

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam1}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
  in0
):
  while True:
    return (_idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_1_125_, in0,)

# {PE_print_01}
def _idris__123_PE_95_print_95_01_125_(in3, in4):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), in3, in4)

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam1}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
  in0
):
  while True:
    return (_idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_1_125_, in0,)

# {io_bind1}
def io_bind1(e0, e1, e2, e3, e4, _idris_w, in0):
  while True:
    aux1 = io_bind0(e0, e1, e2, e3, e4, _idris_w, in0)
    return aux1[0](aux1, _idris_w)  # APPLY

# Prelude.Show.{primNumShow1}
def _idris_Prelude_46_Show_46__123_primNumShow1_125_(e0, e1, e2, e3, in0, in2):
  while True:
    return (_idris__123_U_95_Prelude_46_Show_46__123_primNumShow0_125_1_125_,)

# Prelude.Interactive.{putStr1}
def _idris_Prelude_46_Interactive_46__123_putStr1_125_(in1):
  while True:
    return (U_io_return1, None, None, Unit,)

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
      aux4 = _idris_PE_95_empty_95_0(None)
    else:  # Prelude.Bool.True
      aux5 = _idris_Prelude_46_Applicative_46_pure(
        None,
        None,
        (_idris__123_U_95_PE_95__64__64_constructor_32_of_32_Prelude_46_Applicative_46_Alternative_35_Applicative_32_f_95_02_125_,)
      )
      aux4 = aux5[0](aux5, Unit)  # APPLY
    return _idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
      None,
      None,
      aux4,
      (_idris__123_U_95_Main_46__123_pythag0_125_1_125_, in2, in1, in0,)
    )

# Prelude.Show.{(a, b) instance of Prelude.Show.Show_lam2}
def _idris_Prelude_46_Show_46__123__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam2_125_(
  e2, e3, in1
):
  while True:
    return (_idris__123_U_95_Prelude_46_Show_46__123__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam1_125_1_125_, e2, e3,)

# Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam2}
def _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
  in2, in3
):
  while True:
    aux3 = _idris_Prelude_46_Classes_46_compare(
      None,
      _idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int()
    )
    aux2 = aux3[0](aux3, in2)  # APPLY
    aux1 = aux2[0](aux2, in3)  # APPLY
    if aux1[0] == 0:  # Prelude.Classes.LT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.{Int instance of Prelude.Show.Show_lam2}
def _idris_Prelude_46_Show_46__123_Int_32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam2_125_(
  in1
):
  while True:
    return (_idris__123_U_95_Prelude_46_Show_46__123_Int_32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam1_125_1_125_, in1,)

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam2}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
  in2, in3
):
  while True:
    aux3 = _idris_Prelude_46_Classes_46_compare(
      None,
      _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat()
    )
    aux2 = aux3[0](aux3, in2)  # APPLY
    aux1 = aux2[0](aux2, in3)  # APPLY
    if aux1[0] == 0:  # Prelude.Classes.LT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# {PE_print_02}
def _idris__123_PE_95_print_95_02_125_(in3):
  while True:
    return (_idris__123_U_95__123_PE_95_print_95_01_125_1_125_, in3,)

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam2}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
  in2, in3
):
  while True:
    aux3 = _idris_Prelude_46_Classes_46_compare(
      None,
      _idris_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec()
    )
    aux2 = aux3[0](aux3, in2)  # APPLY
    aux1 = aux2[0](aux2, in3)  # APPLY
    if aux1[0] == 0:  # Prelude.Classes.LT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# {io_bind2}
def io_bind2(e0, e1, e2, e3, e4, _idris_w):
  while True:
    return (_idris__123_U_95__123_io_95_bind1_125_1_125_, e0, e1, e2, e3, e4, _idris_w,)

# Prelude.Show.{primNumShow2}
def _idris_Prelude_46_Show_46__123_primNumShow2_125_(in0, e0, e1, e2, e3):
  while True:
    aux1 = _idris_Prelude_46_Strings_46_strM(in0)
    if aux1[0] == 1:  # Prelude.Strings.StrCons
      in2 = aux1[1]
      aux2 = _idris_Prelude_46_Show_46__123_primNumShow1_125_(e0, e1, e2, e3, in0, in2)
      return aux2[0](aux2, in2)  # APPLY
    else:  # Prelude.Strings.StrNil
      return False
    return _idris_error("unreachable due to case in tail position")

# Main.{pythag2}
def _idris_Main_46__123_pythag2_125_(in0, in1):
  while True:
    return _idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
      None,
      None,
      _idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(
        1, in1
      ),
      (_idris__123_U_95_Main_46__123_pythag1_125_1_125_, in1, in0,)
    )

# Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam3}
def _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
  in2
):
  while True:
    return (_idris__123_U_95_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_1_125_, in2,)

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam3}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
  in2
):
  while True:
    return (_idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_1_125_, in2,)

# {PE_print_03}
def _idris__123_PE_95_print_95_03_125_(in6):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), (0,), in6)  # Prelude.Show.Open

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam3}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
  in2
):
  while True:
    return (_idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_1_125_, in2,)

# Main.{pythag3}
def _idris_Main_46__123_pythag3_125_(in0):
  while True:
    return _idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
      None,
      None,
      _idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(
        1, in0
      ),
      (_idris__123_U_95_Main_46__123_pythag2_125_1_125_, in0,)
    )

# Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam4}
def _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_(
  in4, in5
):
  while True:
    aux3 = _idris_Prelude_46_Classes_46_compare(
      None,
      _idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int()
    )
    aux2 = aux3[0](aux3, in4)  # APPLY
    aux1 = aux2[0](aux2, in5)  # APPLY
    if aux1[0] == 2:  # Prelude.Classes.GT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam4}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_(
  in4, in5
):
  while True:
    aux3 = _idris_Prelude_46_Classes_46_compare(
      None,
      _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat()
    )
    aux2 = aux3[0](aux3, in4)  # APPLY
    aux1 = aux2[0](aux2, in5)  # APPLY
    if aux1[0] == 2:  # Prelude.Classes.GT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# {PE_print_04}
def _idris__123_PE_95_print_95_04_125_(in7, in8):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), in7, in8)

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam4}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_(
  in4, in5
):
  while True:
    aux3 = _idris_Prelude_46_Classes_46_compare(
      None,
      _idris_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec()
    )
    aux2 = aux3[0](aux3, in4)  # APPLY
    aux1 = aux2[0](aux2, in5)  # APPLY
    if aux1[0] == 2:  # Prelude.Classes.GT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam5}
def _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_(
  in4
):
  while True:
    return (_idris__123_U_95_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_1_125_, in4,)

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam5}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_(
  in4
):
  while True:
    return (_idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_1_125_, in4,)

# {PE_print_05}
def _idris__123_PE_95_print_95_05_125_(in7):
  while True:
    return (_idris__123_U_95__123_PE_95_print_95_04_125_1_125_, in7,)

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam5}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_(
  in4
):
  while True:
    return (_idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_1_125_, in4,)

# {PE_print_06}
def _idris__123_PE_95_print_95_06_125_(in9):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), (0,), in9)  # Prelude.Show.Open

# {PE_print_07}
def _idris__123_PE_95_print_95_07_125_(in10, in11):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), in10, in11)

# {PE_print_08}
def _idris__123_PE_95_print_95_08_125_(in10):
  while True:
    return (_idris__123_U_95__123_PE_95_print_95_07_125_1_125_, in10,)

# {PE_print_09}
def _idris__123_PE_95_print_95_09_125_(in5):
  while True:
    return _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
      None,
      None,
      None,
      None,
      (
        0,  # constructor of Prelude.Show.Show
        (_idris__123_U_95__123_PE_95_print_95_03_125_1_125_,),
        (_idris__123_U_95__123_PE_95_print_95_05_125_1_125_,)
      ),
      (
        0,  # constructor of Prelude.Show.Show
        (_idris__123_U_95__123_PE_95_print_95_06_125_1_125_,),
        (_idris__123_U_95__123_PE_95_print_95_08_125_1_125_,)
      ),
      in5
    )

# {PE_print_010}
def _idris__123_PE_95_print_95_010_125_(in14):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), (0,), in14)  # Prelude.Show.Open

# {PE_print_011}
def _idris__123_PE_95_print_95_011_125_(in15, in16):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), in15, in16)

# {PE_print_012}
def _idris__123_PE_95_print_95_012_125_(in15):
  while True:
    return (_idris__123_U_95__123_PE_95_print_95_011_125_1_125_, in15,)

# {PE_print_013}
def _idris__123_PE_95_print_95_013_125_(in17):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), (0,), in17)  # Prelude.Show.Open

# {PE_print_014}
def _idris__123_PE_95_print_95_014_125_(in18, in19):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), in18, in19)

# {PE_print_015}
def _idris__123_PE_95_print_95_015_125_(in18):
  while True:
    return (_idris__123_U_95__123_PE_95_print_95_014_125_1_125_, in18,)

# {PE_print_016}
def _idris__123_PE_95_print_95_016_125_(in13):
  while True:
    return _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
      None,
      None,
      None,
      None,
      (
        0,  # constructor of Prelude.Show.Show
        (_idris__123_U_95__123_PE_95_print_95_010_125_1_125_,),
        (_idris__123_U_95__123_PE_95_print_95_012_125_1_125_,)
      ),
      (
        0,  # constructor of Prelude.Show.Show
        (_idris__123_U_95__123_PE_95_print_95_013_125_1_125_,),
        (_idris__123_U_95__123_PE_95_print_95_015_125_1_125_,)
      ),
      in13
    )

# {PE_print_017}
def _idris__123_PE_95_print_95_017_125_(in12):
  while True:
    return (_idris__123_U_95__123_PE_95_print_95_016_125_1_125_,)

# {PE_print_018}
def _idris__123_PE_95_print_95_018_125_(in1):
  while True:
    return _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
      None,
      None,
      None,
      None,
      (
        0,  # constructor of Prelude.Show.Show
        (_idris__123_U_95__123_PE_95_print_95_00_125_1_125_,),
        (_idris__123_U_95__123_PE_95_print_95_02_125_1_125_,)
      ),
      (
        0,  # constructor of Prelude.Show.Show
        (_idris__123_U_95__123_PE_95_print_95_09_125_1_125_,),
        (_idris__123_U_95__123_PE_95_print_95_017_125_1_125_,)
      ),
      in1
    )

# {PE_print_019}
def _idris__123_PE_95_print_95_019_125_(in22):
  while True:
    aux2 = _idris_Prelude_46_Show_46_showPrec(
      None,
      _idris_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_Int()
    )
    aux1 = aux2[0](aux2, (0,))  # APPLY, Prelude.Show.Open
    return aux1[0](aux1, in22)  # APPLY

# {PE_print_020}
def _idris__123_PE_95_print_95_020_125_(in23, in24):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), in23, in24)

# {PE_print_021}
def _idris__123_PE_95_print_95_021_125_(in23):
  while True:
    return (_idris__123_U_95__123_PE_95_print_95_020_125_1_125_, in23,)

# {PE_print_022}
def _idris__123_PE_95_print_95_022_125_(in26):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), (0,), in26)  # Prelude.Show.Open

# {PE_print_023}
def _idris__123_PE_95_print_95_023_125_(in27, in28):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), in27, in28)

# {PE_print_024}
def _idris__123_PE_95_print_95_024_125_(in27):
  while True:
    return (_idris__123_U_95__123_PE_95_print_95_023_125_1_125_, in27,)

# {PE_print_025}
def _idris__123_PE_95_print_95_025_125_(in29):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), (0,), in29)  # Prelude.Show.Open

# {PE_print_026}
def _idris__123_PE_95_print_95_026_125_(in30, in31):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), in30, in31)

# {PE_print_027}
def _idris__123_PE_95_print_95_027_125_(in30):
  while True:
    return (_idris__123_U_95__123_PE_95_print_95_026_125_1_125_, in30,)

# {PE_print_028}
def _idris__123_PE_95_print_95_028_125_(in25):
  while True:
    return _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
      None,
      None,
      None,
      None,
      (
        0,  # constructor of Prelude.Show.Show
        (_idris__123_U_95__123_PE_95_print_95_022_125_1_125_,),
        (_idris__123_U_95__123_PE_95_print_95_024_125_1_125_,)
      ),
      (
        0,  # constructor of Prelude.Show.Show
        (_idris__123_U_95__123_PE_95_print_95_025_125_1_125_,),
        (_idris__123_U_95__123_PE_95_print_95_027_125_1_125_,)
      ),
      in25
    )

# {PE_print_029}
def _idris__123_PE_95_print_95_029_125_(in34):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), (0,), in34)  # Prelude.Show.Open

# {PE_print_030}
def _idris__123_PE_95_print_95_030_125_(in35, in36):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), in35, in36)

# {PE_print_031}
def _idris__123_PE_95_print_95_031_125_(in35):
  while True:
    return (_idris__123_U_95__123_PE_95_print_95_030_125_1_125_, in35,)

# {PE_print_032}
def _idris__123_PE_95_print_95_032_125_(in37):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), (0,), in37)  # Prelude.Show.Open

# {PE_print_033}
def _idris__123_PE_95_print_95_033_125_(in38, in39):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (U_prim__toStrInt1,), in38, in39)

# {PE_print_034}
def _idris__123_PE_95_print_95_034_125_(in38):
  while True:
    return (_idris__123_U_95__123_PE_95_print_95_033_125_1_125_, in38,)

# {PE_print_035}
def _idris__123_PE_95_print_95_035_125_(in33):
  while True:
    return _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
      None,
      None,
      None,
      None,
      (
        0,  # constructor of Prelude.Show.Show
        (_idris__123_U_95__123_PE_95_print_95_029_125_1_125_,),
        (_idris__123_U_95__123_PE_95_print_95_031_125_1_125_,)
      ),
      (
        0,  # constructor of Prelude.Show.Show
        (_idris__123_U_95__123_PE_95_print_95_032_125_1_125_,),
        (_idris__123_U_95__123_PE_95_print_95_034_125_1_125_,)
      ),
      in33
    )

# {PE_print_036}
def _idris__123_PE_95_print_95_036_125_(in32):
  while True:
    return (_idris__123_U_95__123_PE_95_print_95_035_125_1_125_,)

# {PE_print_037}
def _idris__123_PE_95_print_95_037_125_(in21):
  while True:
    aux1 = _idris_Prelude_46_Show_46_show(
      None,
      _idris_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36__40_a_44__32_b_41_(
        None,
        None,
        (
          0,  # constructor of Prelude.Show.Show
          (_idris__123_U_95__123_PE_95_print_95_019_125_1_125_,),
          (_idris__123_U_95__123_PE_95_print_95_021_125_1_125_,)
        ),
        (
          0,  # constructor of Prelude.Show.Show
          (_idris__123_U_95__123_PE_95_print_95_028_125_1_125_,),
          (_idris__123_U_95__123_PE_95_print_95_036_125_1_125_,)
        )
      )
    )
    return aux1[0](aux1, in21)  # APPLY

# {PE_print_038}
def _idris__123_PE_95_print_95_038_125_(in20):
  while True:
    return (_idris__123_U_95__123_PE_95_print_95_037_125_1_125_,)

# {PE_print_039}
def _idris__123_PE_95_print_95_039_125_(e1, in0):
  while True:
    return sys.stdout.write(_idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0(
      None,
      None,
      (
        0,  # constructor of Prelude.Show.Show
        (_idris__123_U_95__123_PE_95_print_95_018_125_1_125_,),
        (_idris__123_U_95__123_PE_95_print_95_038_125_1_125_,)
      ),
      e1
    ))

# {PE_print_040}
def _idris__123_PE_95_print_95_040_125_(in40):
  while True:
    return (U_io_return1, None, None, Unit,)

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

# Decidable.Equality.Decidable.Equality.ManagedPtr instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_ManagedPtr_58__33_decEq_58_0_58_primitiveNotEq_58_0():
  while True:
    return None

# Decidable.Equality.Decidable.Equality.Ptr instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Ptr_58__33_decEq_58_0_58_primitiveNotEq_58_0():
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
        aux1 = _idris_Prelude_46_Show_46_show(None, e3)
        return (e4 + aux1[0](aux1, in0))  # APPLY
      else:
        aux2 = _idris_Prelude_46_Show_46_show(None, e3)
        e0, e1, e2, e3, e4, e5, = None, None, None, e3, (e4 + (aux2[0](aux2, in0) + ", ")), in1,  # APPLY
        continue
        return _idris_error("unreachable due to tail call")
      return _idris_error("unreachable due to case in tail position")
    else:  # Prelude.List.Nil
      return e4
    return _idris_error("unreachable due to case in tail position")

# Decidable.Equality.Decidable.Equality.Bool instance of Decidable.Equality.DecEq, method decEq
def _idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Bool_58__33_decEq_58_0(
  e0, e1
):
  while True:
    if not e1:  # Prelude.Bool.False
      if not e0:  # Prelude.Bool.False
        return (0,)  # Prelude.Basics.Yes
      else:  # Prelude.Bool.True
        return (1,)  # Prelude.Basics.No
      return _idris_error("unreachable due to case in tail position")
    else:  # Prelude.Bool.True
      if not e0:  # Prelude.Bool.False
        return (1,)  # Prelude.Basics.No
      else:  # Prelude.Bool.True
        return (0,)  # Prelude.Basics.Yes
      return _idris_error("unreachable due to case in tail position")
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

# Prelude.Classes.Prelude.Nat.Nat instance of Prelude.Classes.Eq, method ==
def _idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Eq_36_Nat_58__33__61__61__58_0(
  e0, e1
):
  while True:
    if e1 == 0:
      if e0 == 0:
        return True
      else:
        return False
      return _idris_error("unreachable due to case in tail position")
    elif True:
      in0 = (e1 - 1)
      if e0 == 0:
        return False
      else:
        in1 = (e0 - 1)
        aux2 = _idris_Prelude_46_Classes_46__61__61_(
          None,
          (_idris__123_U_95_Prelude_46_Nat_46_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Eq2_125_,)
        )
        aux1 = aux2[0](aux2, in1)  # APPLY
        return aux1[0](aux1, in0)  # APPLY
      return _idris_error("unreachable due to case in tail position")
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.Prelude.Show.Prec instance of Prelude.Classes.Eq, method ==
def _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Eq_36_Prec_58__33__61__61__58_0(
  e0, e1
):
  while True:
    if e1[0] == 4:  # Prelude.Show.User
      in0 = e1[1]
      if e0[0] == 4:  # Prelude.Show.User
        in1 = e0[1]
        return _idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Eq_36_Nat_58__33__61__61__58_0(
          in1, in0
        )
      else:
        aux1 = (_idris_Prelude_46_Show_46_precCon(e0) == _idris_Prelude_46_Show_46_precCon(e1))
        if aux1 == 0:
          return False
        else:
          return True
        return _idris_error("unreachable due to case in tail position")
      return _idris_error("unreachable due to case in tail position")
    else:
      aux2 = (_idris_Prelude_46_Show_46_precCon(e0) == _idris_Prelude_46_Show_46_precCon(e1))
      if aux2 == 0:
        return False
      else:
        return True
      return _idris_error("unreachable due to case in tail position")
    return _idris_error("unreachable due to case in tail position")

# Prelude.Foldable.Prelude.List.List instance of Prelude.Foldable.Foldable, method foldr
def _idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
  e0, e1, e2, e3, e4
):
  while True:
    if e4:  # Prelude.List.::
      in0, in1 = e4.head, e4.tail
      aux2 = e2
      aux1 = aux2[0](aux2, in0)  # APPLY
      aux5 = _idris_Prelude_46_Foldable_46_foldr(
        None,
        None,
        None,
        (_idris__123_U_95_Prelude_46_List_46_List_32_instance_32_of_32_Prelude_46_Foldable_46_Foldable5_125_,)
      )
      aux4 = aux5[0](aux5, e2)  # APPLY
      aux3 = aux4[0](aux4, e3)  # APPLY
      return aux1[0](aux1, aux3[0](aux3, in1))  # APPLY, APPLY
    else:  # Prelude.List.Nil
      return e3
    return _idris_error("unreachable due to case in tail position")

# Prelude.Monad.Prelude.List instance of Prelude.Monad.Monad, method >>=
def _idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
  e0, e1, e2, e3
):
  while True:
    return _idris_PE_95_concatMap_95_0(None, None, e3, e2)

# Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=
def _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__61__58_0(
  e0, e1
):
  while True:
    aux3 = _idris_Prelude_46_Classes_46__60_(
      None,
      _idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int()
    )
    aux2 = aux3[0](aux3, e0)  # APPLY
    aux1 = aux2[0](aux2, e1)  # APPLY
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

# Prelude.Classes.Prelude.Classes.Integer instance of Prelude.Classes.Ord, method compare
def _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Integer_58__33_compare_58_0(
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

# Prelude.Classes.Prelude.Nat.Nat instance of Prelude.Classes.Ord, method compare
def _idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat_58__33_compare_58_0(
  e0, e1
):
  while True:
    if e1 == 0:
      if e0 == 0:
        return (1,)  # Prelude.Classes.EQ
      else:
        in0 = (e0 - 1)
        return (2,)  # Prelude.Classes.GT
      return _idris_error("unreachable due to case in tail position")
    else:
      in1 = (e1 - 1)
      if e0 == 0:
        return (0,)  # Prelude.Classes.LT
      else:
        in2 = (e0 - 1)
        aux2 = _idris_Prelude_46_Classes_46_compare(
          None,
          _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat()
        )
        aux1 = aux2[0](aux2, in2)  # APPLY
        return aux1[0](aux1, in1)  # APPLY
      return _idris_error("unreachable due to case in tail position")
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.Prelude.Show.Prec instance of Prelude.Classes.Ord, method >=
def _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33__62__61__58_0(
  e0, e1
):
  while True:
    aux3 = _idris_Prelude_46_Classes_46__62_(
      None,
      _idris_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec()
    )
    aux2 = aux3[0](aux3, e0)  # APPLY
    aux1 = aux2[0](aux2, e1)  # APPLY
    if not aux1:  # Prelude.Bool.False
      return _idris_Prelude_46_Classes_46__123_Prelude_46_Show_46_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__62__61__95_lam0_125_(
        e0, e1
      )
    else:  # Prelude.Bool.True
      return True
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.Prelude.Show.Prec instance of Prelude.Classes.Ord, method compare
def _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33_compare_58_0(
  e0, e1
):
  while True:
    if e1[0] == 4:  # Prelude.Show.User
      in0 = e1[1]
      if e0[0] == 4:  # Prelude.Show.User
        in1 = e0[1]
        return _idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat_58__33_compare_58_0(
          in1, in0
        )
      else:
        return _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Integer_58__33_compare_58_0(
          _idris_Prelude_46_Show_46_precCon(e0),
          _idris_Prelude_46_Show_46_precCon(e1)
        )
      return _idris_error("unreachable due to case in tail position")
    else:
      return _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Integer_58__33_compare_58_0(
        _idris_Prelude_46_Show_46_precCon(e0),
        _idris_Prelude_46_Show_46_precCon(e1)
      )
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.Prelude.Show.(a, b) instance of Prelude.Show.Show, method show
def _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
  e0, e1, e2, e3, e4, e5, e6
):
  while True:
    assert True  # Builtins.MkPair
    in0, in1 = e6
    aux1 = _idris_Prelude_46_Show_46_show(None, e4)
    aux2 = _idris_Prelude_46_Show_46_show(None, e5)
    return ("(" + (aux1[0](aux1, in0) + (", " + (aux2[0](aux2, in1) + ")"))))  # APPLY, APPLY
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.Prelude.Show.List a instance of Prelude.Show.Show, method show
def _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0(
  e0, e1, e2, e3
):
  while True:
    return ("[" + (_idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
      None, None, None, e2, "", e3
    ) + "]"))

# with block in Prelude.Strings.strM
def _idris__95_Prelude_46_Strings_46_strM_95_with_95_21(e0, e1):
  while True:
    if e1[0] == 1:  # Prelude.Basics.No
      return _idris_really_95_believe_95_me(None, None, (0,))  # Prelude.Strings.StrNil
    else:  # Prelude.Basics.Yes
      return _idris_really_95_believe_95_me(None, None, (1, e0[0]))  # Prelude.Strings.StrCons
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Classes.Prelude.Show.Prec instance of Prelude.Classes.Ord, method <
def _idris__95_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33__60__58_0_95_with_95_24(
  e0, e1, e2
):
  while True:
    if e0[0] == 0:  # Prelude.Classes.LT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Classes.Prelude.Show.Prec instance of Prelude.Classes.Ord, method >
def _idris__95_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33__62__58_0_95_with_95_26(
  e0, e1, e2
):
  while True:
    if e0[0] == 2:  # Prelude.Classes.GT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Show.firstCharIs
def _idris__95_Prelude_46_Show_46_firstCharIs_95_with_95_41(e0, e1, e2):
  while True:
    if e2[0] == 1:  # Prelude.Strings.StrCons
      in0 = e2[1]
      aux1 = e0
      return aux1[0](aux1, in0)  # APPLY
    else:  # Prelude.Strings.StrNil
      return False
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Classes.Prelude.Nat.Nat instance of Prelude.Classes.Ord, method <
def _idris__95_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat_58__33__60__58_0_95_with_95_81(
  e0, e1, e2
):
  while True:
    if e0[0] == 0:  # Prelude.Classes.LT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Classes.Prelude.Nat.Nat instance of Prelude.Classes.Ord, method >
def _idris__95_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat_58__33__62__58_0_95_with_95_83(
  e0, e1, e2
):
  while True:
    if e0[0] == 2:  # Prelude.Classes.GT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <
def _idris__95_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__58_0_95_with_95_92(
  e0, e1, e2
):
  while True:
    if e0[0] == 0:  # Prelude.Classes.LT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method >
def _idris__95_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__62__58_0_95_with_95_94(
  e0, e1, e2
):
  while True:
    if e0[0] == 2:  # Prelude.Classes.GT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# Prelude.Nat.Nat instance of Prelude.Classes.Eq
def _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Eq_36_Nat(meth0, meth1):
  while True:
    return _idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Eq_36_Nat_58__33__61__61__58_0(
      meth0, meth1
    )

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
    return (
      0,  # constructor of Prelude.Classes.Ord
      (_idris__123_U_95_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_1_125_,),
      (_idris__123_U_95_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_1_125_,),
      (_idris__123_U_95_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_1_125_,)
    )

# Prelude.Nat.Nat instance of Prelude.Classes.Ord
def _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat():
  while True:
    return (
      0,  # constructor of Prelude.Classes.Ord
      (_idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_1_125_,),
      (_idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_1_125_,),
      (_idris__123_U_95_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_1_125_,)
    )

# Prelude.Show.Prec instance of Prelude.Classes.Ord
def _idris_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec():
  while True:
    return (
      0,  # constructor of Prelude.Classes.Ord
      (_idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_1_125_,),
      (_idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_1_125_,),
      (_idris__123_U_95_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_1_125_,)
    )

# Prelude.Show.(a, b) instance of Prelude.Show.Show
def _idris_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36__40_a_44__32_b_41_(
  e0, e1, e2, e3
):
  while True:
    return (
      0,  # constructor of Prelude.Show.Show
      (_idris__123_U_95_Prelude_46_Show_46__123__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam0_125_1_125_, e2, e3,),
      (_idris__123_U_95_Prelude_46_Show_46__123__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam2_125_1_125_, e2, e3,)
    )

# Prelude.Show.Int instance of Prelude.Show.Show
def _idris_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_Int():
  while True:
    return (
      0,  # constructor of Prelude.Show.Show
      (_idris__123_U_95_Prelude_46_Show_46__123_Int_32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam0_125_1_125_,),
      (_idris__123_U_95_Prelude_46_Show_46__123_Int_32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam2_125_1_125_,)
    )

# case block in Void
def _idris_Void_95_case():
  while True:
    return None

# case block in io_bind
def _idris_io_95_bind_95_case(e0, e1, e2, e3, e4, e5, e6, e7):
  while True:
    aux1 = e7
    return aux1[0](aux1, e5)  # APPLY

# <<Void eliminator>>
def _idris_Void_95_elim():
  while True:
    return None

if __name__ == '__main__':
  runMain0()
