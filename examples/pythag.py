#!/usr/bin/env python

import sys
import importlib
import math

Unit = object()
World = object()

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

# Prelude.Basics..
def _idris_Prelude_46_Basics_46__46_(e0, e1, e2, e3, e4, _idris_x):
  while True:
    return APPLY0(e3, APPLY0(e4, _idris_x))

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

# PE_(a, b) instance of Prelude.Show.Show_a94d79ab
def _idris_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab():
  while True:
    return (0, (65704,), (65719,))  # constructor of Prelude.Show.Show, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab12}1}, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab26}1}

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
    return (u'(' + (_idris_Prelude_46_Show_46_primNumShow(None, (65700,), (0,), in0) + (u', ' + (_idris_Prelude_46_Show_46_primNumShow(None, (65700,), (0,), in1) + u')'))))  # {U_prim__toStrInt1}, Prelude.Show.Open, {U_prim__toStrInt1}, Prelude.Show.Open
    return _idris_error("unreachable due to case in tail position")

# PE_Prelude.Show.List a instance of Prelude.Show.Show, method show_54220539
def _idris_PE_95_Prelude_46_Show_46_List_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_54220539(
  e0
):
  while True:
    return (u'[' + (_idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
      None,
      None,
      None,
      _idris_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab(),
      u'',
      e0
    ) + u']'))

# PE_concatMap_af3155d1
def _idris_PE_95_concatMap_95_af3155d1(e0, e1, e2, e3):
  while True:
    return _idris_PE_95_foldr_95_c8d7af37(
      None,
      None,
      (65728, e2),  # {U_{PE_concatMap_af3155d10}1}
      _idris_PE_95_neutral_95_42111bf0(None),
      e3
    )

# PE_empty_8ff8f7b3
def _idris_PE_95_empty_95_8ff8f7b3(e0):
  while True:
    return ConsList()

# PE_foldr_c8d7af37
def _idris_PE_95_foldr_95_c8d7af37(e0, e1, e2, e3, e4):
  while True:
    return _idris_Prelude_46_Foldable_46_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List_58__33_foldr_58_0(
      None, None, e2, e3, e4
    )

# PE_neutral_42111bf0
def _idris_PE_95_neutral_95_42111bf0(e0):
  while True:
    return ConsList()

# PE_printLn'_1452bb16
def _idris_PE_95_printLn_39__95_1452bb16(e0, e1):
  while True:
    return _idris_Prelude_46_Interactive_46_putStr_39_(
      None,
      (_idris_PE_95_show_95_78b4bfbe(e1) + u'\u000a')
    )

# PE_printLn'_cfef5baf
def _idris_PE_95_printLn_39__95_cfef5baf(e0, e1):
  while True:
    return _idris_Prelude_46_Interactive_46_putStr_39_(
      None,
      (_idris_PE_95_show_95_24967653(e1) + u'\u000a')
    )

# PE_printLn_48f3a70d
def _idris_PE_95_printLn_95_48f3a70d(e0):
  while True:
    return _idris_PE_95_printLn_39__95_1452bb16(None, e0)

# PE_show_24967653
def _idris_PE_95_show_95_24967653(e0):
  while True:
    return (u'[' + (_idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(
      None,
      None,
      None,
      (0, (65729,), (65731,)),  # constructor of Prelude.Show.Show, {U_{PE_show_249676530}1}, {U_{PE_show_249676532}1}
      u'',
      e0
    ) + u']'))

# PE_show_78b4bfbe
def _idris_PE_95_show_95_78b4bfbe(e0):
  while True:
    return _idris_PE_95_Prelude_46_Show_46_List_32_a_32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_54220539(
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
    in0, in1, in2 = e1[1:]
    return in0
    return _idris_error("unreachable due to case in tail position")

# Prelude.Foldable.foldr
def _idris_Prelude_46_Foldable_46_foldr(e0, e1, e2, e3):
  while True:
    return APPLY0(APPLY0(e3, e1), e2)

# Prelude.Basics.id
def _idris_Prelude_46_Basics_46_id(e0, e1):
  while True:
    return e1

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

# Prelude.Chars.isDigit
def _idris_Prelude_46_Chars_46_isDigit(e0):
  while True:
    aux1 = _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Char_58__33__62__61__58_0(
      e0, u'0'
    )
    if not aux1:  # Prelude.Bool.False
      return False
    else:  # Prelude.Bool.True
      return _idris_Prelude_46_Chars_46__123_isDigit0_125_(e0)
    return _idris_error("unreachable due to case in tail position")

# Main.main
def _idris_Main_46_main():
  while True:
    return (
      65697,  # {U_io_bind1}
      None,
      None,
      None,
      _idris_PE_95_printLn_95_48f3a70d(_idris_Main_46_pythag(100)),
      (65640,)  # {U_Main.{main2}1}
    )

# Prelude.List.mergeBy
def _idris_Prelude_46_List_46_mergeBy(e0, e1, e2, e3):
  while True:
    if not e2:  # Prelude.List.Nil
      return e3
    else:
      if e3:  # Prelude.List.::
        in0, in1 = e3.head, e3.tail
        assert e2  # Prelude.List.::
        in2, in3 = e2.head, e2.tail
        aux1 = _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Eq_36_Ordering_58__33__61__61__58_0(
          APPLY0(APPLY0(e1, in2), in0),
          (0,)  # Prelude.Classes.LT
        )
        if not aux1:  # Prelude.Bool.False
          return _idris_Prelude_46_List_46_mergeBy(None, e1, in3.cons(in2), in1).cons(in0)
        else:  # Prelude.Bool.True
          return _idris_Prelude_46_List_46_mergeBy(None, e1, in3, in1.cons(in0)).cons(in2)
        return _idris_error("unreachable due to case in tail position")
        return _idris_error("unreachable due to case in tail position")
      else:  # Prelude.List.Nil
        return e2
      return _idris_error("unreachable due to case in tail position")
    return _idris_error("unreachable due to case in tail position")

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
    in0 = APPLY0(e1, e3)
    aux2 = _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33__62__61__58_0(
      e2, (5,)  # Prelude.Show.PrefixMinus
    )
    if not aux2:  # Prelude.Bool.False
      aux3 = False
    else:  # Prelude.Bool.True
      aux3 = _idris_Prelude_46_Show_46__123_primNumShow2_125_(in0, e0, e1, e2, e3)
    aux1 = aux3
    if not aux1:  # Prelude.Bool.False
      return in0
    else:  # Prelude.Bool.True
      return (u'(' + (in0 + u')'))
    return _idris_error("unreachable due to case in tail position")

# prim__addInt
def _idris_prim_95__95_addInt(op0, op1):
  while True:
    return (op0 + op1)

# prim__charToInt
def _idris_prim_95__95_charToInt(op0):
  while True:
    return ord(op0)

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

# prim__ltString
def _idris_prim_95__95_ltString(op0, op1):
  while True:
    return (op0 < op1)

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

# prim__sltChar
def _idris_prim_95__95_sltChar(op0, op1):
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

# prim__strCons
def _idris_prim_95__95_strCons(op0, op1):
  while True:
    return (op0 + op1)

# prim__strHead
def _idris_prim_95__95_strHead(op0):
  while True:
    return op0[0]

# prim__strTail
def _idris_prim_95__95_strTail(op0):
  while True:
    return op0[1:]

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

# Prelude.Show.protectEsc
def _idris_Prelude_46_Show_46_protectEsc(e0, e1, e2):
  while True:
    aux2 = _idris_Prelude_46_Strings_46_strM(e2)
    if aux2[0] == 1:  # Prelude.Strings.StrCons
      in0, in1 = aux2[1:]
      aux3 = APPLY0(e0, in0)
    else:  # Prelude.Strings.StrNil
      aux3 = False
    aux1 = aux3
    if not aux1:  # Prelude.Bool.False
      aux4 = u''
    else:  # Prelude.Bool.True
      aux4 = u'\\&'
    return (e1 + (aux4 + e2))

# Prelude.Applicative.pure
def _idris_Prelude_46_Applicative_46_pure(e0, e1, e2):
  while True:
    return APPLY0(e2, e1)

# Prelude.Interactive.putStr'
def _idris_Prelude_46_Interactive_46_putStr_39_(e0, e1):
  while True:
    return (65697, None, None, None, (65663, e1), (65664,))  # {U_io_bind1}, {U_Prelude.Interactive.{putStr'0}1}, {U_Prelude.Interactive.{putStr'1}1}

# Main.pythag
def _idris_Main_46_pythag(e0):
  while True:
    return _idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
      None,
      None,
      _idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(
        1, e0
      ),
      (65644,)  # {U_Main.{pythag3}1}
    )

# really_believe_me
def _idris_really_95_believe_95_me(e0, e1, e2):
  while True:
    return e2

# run__IO
def _idris_run_95__95_IO(e0, e1):
  while True:
    return APPLY0(e1, None)

# Prelude.Show.show
def _idris_Prelude_46_Show_46_show(e0, e1):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Show.Show
    in0, in1 = e1[1:]
    return in0
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.showLitChar
def _idris_Prelude_46_Show_46_showLitChar(e0):
  while True:
    aux1 = _idris_Prelude_46_Show_46_showLitChar_58_getAt_58_10(
      None,
      ord(e0),
      _idris_Prelude_46_Show_46_showLitChar_58_asciiTab_58_10(None)
    )
    if aux1 is not None:  # Prelude.Maybe.Just
      in10 = aux1
      aux2 = (65648, None, None, None, (65699, u'\\'), (65686, in10))  # {U_Prelude.Basics..1}, {U_prim__strCons1}, {U_Prelude.Show.{showLitChar10}1}
    else:  # Prelude.Maybe.Nothing
      aux4 = _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Char_58__33_compare_58_0(
        e0,
        u'\u007f'
      )
      if aux4[0] == 2:  # Prelude.Classes.GT
        aux5 = True
      else:
        aux5 = False
      aux3 = aux5
      if not aux3:  # Prelude.Bool.False
        aux6 = (65699, e0)  # {U_prim__strCons1}
      else:  # Prelude.Bool.True
        aux6 = (
          65648,  # {U_Prelude.Basics..1}
          None,
          None,
          None,
          (65699, u'\\'),  # {U_prim__strCons1}
          (
            65673,  # {U_Prelude.Show.protectEsc1}
            (65650,),  # {U_Prelude.Chars.isDigit1}
            _idris_Prelude_46_Show_46_primNumShow(None, (65700,), (0,), ord(e0))  # {U_prim__toStrInt1}, Prelude.Show.Open
          )
        )
      aux2 = aux6
    return {
      u'\u0007': (65685,),  # {U_Prelude.Show.{showLitChar0}1}
      u'\u0008': (65687,),  # {U_Prelude.Show.{showLitChar1}1}
      u'\u0009': (65688,),  # {U_Prelude.Show.{showLitChar2}1}
      u'\u000a': (65689,),  # {U_Prelude.Show.{showLitChar3}1}
      u'\u000b': (65690,),  # {U_Prelude.Show.{showLitChar4}1}
      u'\u000c': (65691,),  # {U_Prelude.Show.{showLitChar5}1}
      u'\u000d': (65692,),  # {U_Prelude.Show.{showLitChar6}1}
      u'\u000e': (65673, (65693,), u'\\SO'),  # {U_Prelude.Show.protectEsc1}, {U_Prelude.Show.{showLitChar7}1}
      u'\\': (65694,),  # {U_Prelude.Show.{showLitChar8}1}
      u'\u007f': (65695,)  # {U_Prelude.Show.{showLitChar9}1}
    }.get(e0, aux2)

# Prelude.Show.showLitString
def _idris_Prelude_46_Show_46_showLitString(e0):
  while True:
    if e0:  # Prelude.List.::
      in0, in1 = e0.head, e0.tail
      if in0 == u'"':
        return (
          65648,  # {U_Prelude.Basics..1}
          None,
          None,
          None,
          (65696,),  # {U_Prelude.Show.{showLitString0}1}
          _idris_Prelude_46_Show_46_showLitString(in1)
        )
      else:
        return (
          65648,  # {U_Prelude.Basics..1}
          None,
          None,
          None,
          _idris_Prelude_46_Show_46_showLitChar(in0),
          _idris_Prelude_46_Show_46_showLitString(in1)
        )
      return _idris_error("unreachable due to case in tail position")
    else:  # Prelude.List.Nil
      return (65649, None)  # {U_Prelude.Basics.id1}
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.showParens
def _idris_Prelude_46_Show_46_showParens(e0, e1):
  while True:
    if not e0:  # Prelude.Bool.False
      return e1
    else:  # Prelude.Bool.True
      return (u'(' + (e1 + u')'))
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.showPrec
def _idris_Prelude_46_Show_46_showPrec(e0, e1):
  while True:
    assert e1[0] == 0  # constructor of Prelude.Show.Show
    in0, in1 = e1[1:]
    return in1
    return _idris_error("unreachable due to case in tail position")

# Prelude.List.sortBy
def _idris_Prelude_46_List_46_sortBy(e0, e1, e2):
  while True:
    if e2:  # Prelude.List.::
      in0, in1 = e2.head, e2.tail
      if not in1:  # Prelude.List.Nil
        return ConsList().cons(in0)
      else:
        aux1 = _idris_Prelude_46_List_46_sortBy_58_splitRec_58_2(
          None,
          None,
          None,
          e2,
          e2,
          (65649, None)  # {U_Prelude.Basics.id1}
        )
        assert True  # Builtins.MkPair
        in2, in3 = aux1
        return _idris_Prelude_46_List_46_mergeBy(
          None,
          e1,
          _idris_Prelude_46_List_46_sortBy(None, e1, in2),
          _idris_Prelude_46_List_46_sortBy(None, e1, in3)
        )
        return _idris_error("unreachable due to case in tail position")
      return _idris_error("unreachable due to case in tail position")
    elif not e2:  # Prelude.List.Nil
      return ConsList()
    else:
      aux2 = _idris_Prelude_46_List_46_sortBy_58_splitRec_58_2(
        None,
        None,
        None,
        e2,
        e2,
        (65649, None)  # {U_Prelude.Basics.id1}
      )
      assert True  # Builtins.MkPair
      in4, in5 = aux2
      return _idris_Prelude_46_List_46_mergeBy(
        None,
        e1,
        _idris_Prelude_46_List_46_sortBy(None, e1, in4),
        _idris_Prelude_46_List_46_sortBy(None, e1, in5)
      )
      return _idris_error("unreachable due to case in tail position")
    return _idris_error("unreachable due to case in tail position")

# Prelude.Strings.strM
def _idris_Prelude_46_Strings_46_strM(e0):
  while True:
    aux3 = (e0 == u'')
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
      return _idris_really_95_believe_95_me(None, None, (1, e0[0], e0[1:]))  # Prelude.Strings.StrCons
    return _idris_error("unreachable due to case in tail position")

# Prelude.Strings.unpack
def _idris_Prelude_46_Strings_46_unpack(e0):
  while True:
    aux1 = _idris_Prelude_46_Strings_46_strM(e0)
    if aux1[0] == 1:  # Prelude.Strings.StrCons
      in0, in1 = aux1[1:]
      return _idris_Prelude_46_Strings_46_unpack(in1).cons(in0)
    else:  # Prelude.Strings.StrNil
      return ConsList()
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

# {APPLY0}
def APPLY0(fn0, arg0):
  while True:
    if fn0[0] < 65690:
      if fn0[0] < 65664:
        if fn0[0] < 65651:
          if fn0[0] < 65644:
            if fn0[0] < 65641:
              if fn0[0] == 65638:  # {U_Main.{main0}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_main0_125_(P_c0, arg0)
              elif fn0[0] == 65639:  # {U_Main.{main1}1}
                return _idris_Main_46__123_main1_125_(arg0)
              else:  # {U_Main.{main2}1}
                return _idris_Main_46__123_main2_125_(arg0)
            else:
              if fn0[0] == 65641:  # {U_Main.{pythag0}1}
                P_c0, P_c1, P_c2 = fn0[1:]
                return _idris_Main_46__123_pythag0_125_(P_c0, P_c1, P_c2, arg0)
              elif fn0[0] == 65642:  # {U_Main.{pythag1}1}
                P_c0, P_c1 = fn0[1:]
                return _idris_Main_46__123_pythag1_125_(P_c0, P_c1, arg0)
              else:  # {U_Main.{pythag2}1}
                P_c0 = fn0[1]
                return _idris_Main_46__123_pythag2_125_(P_c0, arg0)
          else:
            if fn0[0] < 65647:
              if fn0[0] == 65644:  # {U_Main.{pythag3}1}
                return _idris_Main_46__123_pythag3_125_(arg0)
              elif fn0[0] == 65645:  # {U_PE_@@constructor of Prelude.Algebra.Monoid#Semigroup a_42111bf01}
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
              if fn0[0] < 65649:
                if fn0[0] == 65647:  # {U_PE_@@constructor of Prelude.Monad.Monad#Applicative m_d05ad59e1}
                  P_c0 = fn0[1]
                  return _idris_PE_95__64__64_constructor_32_of_32_Prelude_46_Monad_46_Monad_35_Applicative_32_m_95_d05ad59e(
                    P_c0, arg0
                  )
                else:  # {U_Prelude.Basics..1}
                  P_c0, P_c1, P_c2, P_c3, P_c4 = fn0[1:]
                  return _idris_Prelude_46_Basics_46__46_(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
              else:
                if fn0[0] == 65649:  # {U_Prelude.Basics.id1}
                  P_c0 = fn0[1]
                  return _idris_Prelude_46_Basics_46_id(P_c0, arg0)
                else:  # {U_Prelude.Chars.isDigit1}
                  return _idris_Prelude_46_Chars_46_isDigit(arg0)
        else:
          if fn0[0] < 65657:
            if fn0[0] < 65654:
              if fn0[0] == 65651:  # {U_Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam0}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Classes_46__123_Char_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
                  P_c0, arg0
                )
              elif fn0[0] == 65652:  # {U_Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam1}1}
                return _idris_Prelude_46_Classes_46__123_Char_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
                  arg0
                )
              else:  # {U_Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam2}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Classes_46__123_Char_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
                  P_c0, arg0
                )
            else:
              if fn0[0] == 65654:  # {U_Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam3}1}
                return _idris_Prelude_46_Classes_46__123_Char_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
                  arg0
                )
              elif fn0[0] == 65655:  # {U_Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam4}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Classes_46__123_Char_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_(
                  P_c0, arg0
                )
              else:  # {U_Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam5}1}
                return _idris_Prelude_46_Classes_46__123_Char_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_(
                  arg0
                )
          else:
            if fn0[0] < 65660:
              if fn0[0] == 65657:  # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam0}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
                  P_c0, arg0
                )
              elif fn0[0] == 65658:  # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam1}1}
                return _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
                  arg0
                )
              else:  # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam2}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
                  P_c0, arg0
                )
            else:
              if fn0[0] < 65662:
                if fn0[0] == 65660:  # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam3}1}
                  return _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
                    arg0
                  )
                else:  # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam4}1}
                  P_c0 = fn0[1]
                  return _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_(
                    P_c0, arg0
                  )
              else:
                if fn0[0] == 65662:  # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam5}1}
                  return _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_(
                    arg0
                  )
                else:  # {U_Prelude.Interactive.{putStr'0}1}
                  P_c0 = fn0[1]
                  return _idris_Prelude_46_Interactive_46__123_putStr_39_0_125_(P_c0, arg0)
      else:
        if fn0[0] < 65677:
          if fn0[0] < 65670:
            if fn0[0] < 65667:
              if fn0[0] == 65664:  # {U_Prelude.Interactive.{putStr'1}1}
                return _idris_Prelude_46_Interactive_46__123_putStr_39_1_125_(arg0)
              elif fn0[0] == 65665:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable1}
                P_c0, P_c1, P_c2, P_c3 = fn0[1:]
                return _idris_Prelude_46_List_46__64_Prelude_46_Foldable_46_Foldable_36_List(
                  P_c0, P_c1, P_c2, P_c3, arg0
                )
              else:  # {U_Prelude.Nat.Nat instance of Prelude.Classes.Eq1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Eq_36_Nat(P_c0, arg0)
            else:
              if fn0[0] == 65667:  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam0}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
                  P_c0, arg0
                )
              elif fn0[0] == 65668:  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam1}1}
                return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
                  arg0
                )
              else:  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam2}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
                  P_c0, arg0
                )
          else:
            if fn0[0] < 65673:
              if fn0[0] == 65670:  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam3}1}
                return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
                  arg0
                )
              elif fn0[0] == 65671:  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam4}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_(
                  P_c0, arg0
                )
              else:  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam5}1}
                return _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_(
                  arg0
                )
            else:
              if fn0[0] < 65675:
                if fn0[0] == 65673:  # {U_Prelude.Show.protectEsc1}
                  P_c0, P_c1 = fn0[1:]
                  return _idris_Prelude_46_Show_46_protectEsc(P_c0, P_c1, arg0)
                else:  # {U_Prelude.Show.{Int instance of Prelude.Show.Show_lam0}1}
                  return _idris_Prelude_46_Show_46__123_Int_32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam0_125_(
                    arg0
                  )
              else:
                if fn0[0] == 65675:  # {U_Prelude.Show.{Int instance of Prelude.Show.Show_lam1}1}
                  P_c0 = fn0[1]
                  return _idris_Prelude_46_Show_46__123_Int_32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam1_125_(
                    P_c0, arg0
                  )
                else:  # {U_Prelude.Show.{Int instance of Prelude.Show.Show_lam2}1}
                  return _idris_Prelude_46_Show_46__123_Int_32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam2_125_(
                    arg0
                  )
        else:
          if fn0[0] < 65683:
            if fn0[0] < 65680:
              if fn0[0] == 65677:  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam0}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
                  P_c0, arg0
                )
              elif fn0[0] == 65678:  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam1}1}
                return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
                  arg0
                )
              else:  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam2}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
                  P_c0, arg0
                )
            else:
              if fn0[0] == 65680:  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam3}1}
                return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
                  arg0
                )
              elif fn0[0] == 65681:  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam4}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_(
                  P_c0, arg0
                )
              else:  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam5}1}
                return _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_(
                  arg0
                )
          else:
            if fn0[0] < 65686:
              if fn0[0] == 65683:  # {U_Prelude.Show.{case block in showLitChar at ./Prelude/Show.idr:126:27_lam0}1}
                P_c0 = fn0[1]
                return _idris_Prelude_46_Show_46__123_case_32_block_32_in_32_showLitChar_32_at_32__46__47_Prelude_47_Show_46_idr_58_126_58_27_95_lam0_125_(
                  P_c0, arg0
                )
              elif fn0[0] == 65684:  # {U_Prelude.Show.{primNumShow0}1}
                return _idris_Prelude_46_Show_46__123_primNumShow0_125_(arg0)
              else:  # {U_Prelude.Show.{showLitChar0}1}
                return _idris_Prelude_46_Show_46__123_showLitChar0_125_(arg0)
            else:
              if fn0[0] < 65688:
                if fn0[0] == 65686:  # {U_Prelude.Show.{showLitChar10}1}
                  P_c0 = fn0[1]
                  return _idris_Prelude_46_Show_46__123_showLitChar10_125_(P_c0, arg0)
                else:  # {U_Prelude.Show.{showLitChar1}1}
                  return _idris_Prelude_46_Show_46__123_showLitChar1_125_(arg0)
              else:
                if fn0[0] == 65688:  # {U_Prelude.Show.{showLitChar2}1}
                  return _idris_Prelude_46_Show_46__123_showLitChar2_125_(arg0)
                else:  # {U_Prelude.Show.{showLitChar3}1}
                  return _idris_Prelude_46_Show_46__123_showLitChar3_125_(arg0)
    else:
      if fn0[0] < 65716:
        if fn0[0] < 65703:
          if fn0[0] < 65696:
            if fn0[0] < 65693:
              if fn0[0] == 65690:  # {U_Prelude.Show.{showLitChar4}1}
                return _idris_Prelude_46_Show_46__123_showLitChar4_125_(arg0)
              elif fn0[0] == 65691:  # {U_Prelude.Show.{showLitChar5}1}
                return _idris_Prelude_46_Show_46__123_showLitChar5_125_(arg0)
              else:  # {U_Prelude.Show.{showLitChar6}1}
                return _idris_Prelude_46_Show_46__123_showLitChar6_125_(arg0)
            else:
              if fn0[0] == 65693:  # {U_Prelude.Show.{showLitChar7}1}
                return _idris_Prelude_46_Show_46__123_showLitChar7_125_(arg0)
              elif fn0[0] == 65694:  # {U_Prelude.Show.{showLitChar8}1}
                return _idris_Prelude_46_Show_46__123_showLitChar8_125_(arg0)
              else:  # {U_Prelude.Show.{showLitChar9}1}
                return _idris_Prelude_46_Show_46__123_showLitChar9_125_(arg0)
          else:
            if fn0[0] < 65699:
              if fn0[0] == 65696:  # {U_Prelude.Show.{showLitString0}1}
                return _idris_Prelude_46_Show_46__123_showLitString0_125_(arg0)
              elif fn0[0] == 65697:  # {U_io_bind1}
                P_c0, P_c1, P_c2, P_c3, P_c4 = fn0[1:]
                return _idris_io_95_bind(P_c0, P_c1, P_c2, P_c3, P_c4, arg0)
              else:  # {U_io_return1}
                P_c0, P_c1, P_c2 = fn0[1:]
                return _idris_io_95_return(P_c0, P_c1, P_c2, arg0)
            else:
              if fn0[0] < 65701:
                if fn0[0] == 65699:  # {U_prim__strCons1}
                  P_c0 = fn0[1]
                  return _idris_prim_95__95_strCons(P_c0, arg0)
                else:  # {U_prim__toStrInt1}
                  return _idris_prim_95__95_toStrInt(arg0)
              else:
                if fn0[0] == 65701:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab0}1}
                  return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab0_125_(
                    arg0
                  )
                else:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab10}1}
                  return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab10_125_(
                    arg0
                  )
        else:
          if fn0[0] < 65709:
            if fn0[0] < 65706:
              if fn0[0] == 65703:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab11}1}
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab11_125_(
                  arg0
                )
              elif fn0[0] == 65704:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab12}1}
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab12_125_(
                  arg0
                )
              else:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab13}1}
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab13_125_(
                  arg0
                )
            else:
              if fn0[0] == 65706:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab14}1}
                P_c0 = fn0[1]
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab14_125_(
                  P_c0, arg0
                )
              elif fn0[0] == 65707:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab15}1}
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab15_125_(
                  arg0
                )
              else:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab16}1}
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab16_125_(
                  arg0
                )
          else:
            if fn0[0] < 65712:
              if fn0[0] == 65709:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab17}1}
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab17_125_(
                  arg0
                )
              elif fn0[0] == 65710:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab18}1}
                P_c0 = fn0[1]
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab18_125_(
                  P_c0, arg0
                )
              else:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab19}1}
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab19_125_(
                  arg0
                )
            else:
              if fn0[0] < 65714:
                if fn0[0] == 65712:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab1}1}
                  P_c0 = fn0[1]
                  return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab1_125_(
                    P_c0, arg0
                  )
                else:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab20}1}
                  return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab20_125_(
                    arg0
                  )
              else:
                if fn0[0] == 65714:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab21}1}
                  P_c0 = fn0[1]
                  return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab21_125_(
                    P_c0, arg0
                  )
                else:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab22}1}
                  return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab22_125_(
                    arg0
                  )
      else:
        if fn0[0] < 65729:
          if fn0[0] < 65722:
            if fn0[0] < 65719:
              if fn0[0] == 65716:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab23}1}
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab23_125_(
                  arg0
                )
              elif fn0[0] == 65717:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab24}1}
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab24_125_(
                  arg0
                )
              else:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab25}1}
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab25_125_(
                  arg0
                )
            else:
              if fn0[0] == 65719:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab26}1}
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab26_125_(
                  arg0
                )
              elif fn0[0] == 65720:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab2}1}
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab2_125_(
                  arg0
                )
              else:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab3}1}
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab3_125_(
                  arg0
                )
          else:
            if fn0[0] < 65725:
              if fn0[0] == 65722:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab4}1}
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab4_125_(
                  arg0
                )
              elif fn0[0] == 65723:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab5}1}
                P_c0 = fn0[1]
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab5_125_(
                  P_c0, arg0
                )
              else:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab6}1}
                return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab6_125_(
                  arg0
                )
            else:
              if fn0[0] < 65727:
                if fn0[0] == 65725:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab7}1}
                  return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab7_125_(
                    arg0
                  )
                else:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab8}1}
                  P_c0 = fn0[1]
                  return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab8_125_(
                    P_c0, arg0
                  )
              else:
                if fn0[0] == 65727:  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab9}1}
                  return _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab9_125_(
                    arg0
                  )
                else:  # {U_{PE_concatMap_af3155d10}1}
                  P_c0 = fn0[1]
                  return _idris__123_PE_95_concatMap_95_af3155d10_125_(P_c0, arg0)
        else:
          if fn0[0] < 65736:
            if fn0[0] < 65732:
              if fn0[0] == 65729:  # {U_{PE_show_249676530}1}
                return _idris__123_PE_95_show_95_249676530_125_(arg0)
              elif fn0[0] == 65730:  # {U_{PE_show_249676531}1}
                return _idris__123_PE_95_show_95_249676531_125_(arg0)
              else:  # {U_{PE_show_249676532}1}
                return _idris__123_PE_95_show_95_249676532_125_(arg0)
            else:
              if fn0[0] < 65734:
                if fn0[0] == 65732:  # {U_{Prelude.List.sortBy, splitRec_lam0}1}
                  P_c0 = fn0[1]
                  return _idris__123_Prelude_46_List_46_sortBy_44__32_splitRec_95_lam0_125_(P_c0, arg0)
                else:  # {U_{io_bind1}1}
                  P_c0, P_c1, P_c2, P_c3, P_c4, P_c5 = fn0[1:]
                  return io_bind1(P_c0, P_c1, P_c2, P_c3, P_c4, P_c5, arg0)
              else:
                if fn0[0] == 65734:  # {U_PE_@@constructor of Prelude.Algebra.Monoid#Semigroup a_42111bf02}
                  P_c0 = fn0[1]
                  return (65645, P_c0, arg0)  # {U_PE_@@constructor of Prelude.Algebra.Monoid#Semigroup a_42111bf01}
                else:  # {U_PE_@@constructor of Prelude.Applicative.Alternative#Applicative f_5102bba82}
                  return (65646, arg0)  # {U_PE_@@constructor of Prelude.Applicative.Alternative#Applicative f_5102bba81}
          else:
            if fn0[0] < 65739:
              if fn0[0] == 65736:  # {U_PE_@@constructor of Prelude.Monad.Monad#Applicative m_d05ad59e2}
                return (65647, arg0)  # {U_PE_@@constructor of Prelude.Monad.Monad#Applicative m_d05ad59e1}
              elif fn0[0] == 65737:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable2}
                P_c0, P_c1, P_c2 = fn0[1:]
                return (65665, P_c0, P_c1, P_c2, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable1}
              else:  # {U_Prelude.Nat.Nat instance of Prelude.Classes.Eq2}
                return (65666, arg0)  # {U_Prelude.Nat.Nat instance of Prelude.Classes.Eq1}
            else:
              if fn0[0] < 65741:
                if fn0[0] == 65739:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable3}
                  P_c0, P_c1 = fn0[1:]
                  return (65737, P_c0, P_c1, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable2}
                else:  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable4}
                  P_c0 = fn0[1]
                  return (65739, P_c0, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable3}
              else:
                assert fn0[0] == 65741  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable5}
                return (65740, arg0)  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable4}
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam0}
def _idris_Prelude_46_Classes_46__123_Char_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
  in0, in1
):
  while True:
    return _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Char_58__33_compare_58_0(
      in0, in1
    )

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
    return APPLY0(
      APPLY0(
        _idris_Prelude_46_Show_46_showPrec(
          None,
          _idris_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_Int()
        ),
        (0,)  # Prelude.Show.Open
      ),
      in0
    )

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam0}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
  in0, in1
):
  while True:
    return _idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat_58__33_compare_58_0(
      in0, in1
    )

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab0}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab0_125_(
  in1
):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (65700,), (0,), in1)  # {U_prim__toStrInt1}, Prelude.Show.Open

# {PE_concatMap_af3155d10}
def _idris__123_PE_95_concatMap_95_af3155d10_125_(e2, in0):
  while True:
    return APPLY0(
      _idris_Prelude_46_Algebra_46__60__43__62_(None, (65734, None)),  # {U_PE_@@constructor of Prelude.Algebra.Monoid#Semigroup a_42111bf02}
      APPLY0(e2, in0)
    )

# {PE_show_249676530}
def _idris__123_PE_95_show_95_249676530_125_(in0):
  while True:
    return _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_String_58__33_show_58_0(
      in0
    )

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam0}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam0_125_(
  in0, in1
):
  while True:
    return _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33_compare_58_0(
      in0, in1
    )

# Prelude.Classes.{Prelude.Classes.Char instance of Prelude.Classes.Ord, method <=_lam0}
def _idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Char_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__60__61__95_lam0_125_(
  e0, e1
):
  while True:
    aux1 = (e0 == e1)
    if aux1 == 0:
      return False
    else:
      return True
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.{Prelude.Classes.Char instance of Prelude.Classes.Ord, method >=_lam0}
def _idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Char_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__62__61__95_lam0_125_(
  e0, e1
):
  while True:
    aux1 = (e0 == e1)
    if aux1 == 0:
      return False
    else:
      return True
    return _idris_error("unreachable due to case in tail position")

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

# {Prelude.List.sortBy, splitRec_lam0}
def _idris__123_Prelude_46_List_46_sortBy_44__32_splitRec_95_lam0_125_(in0, in6):
  while True:
    return in6.cons(in0)

# Prelude.Classes.{Prelude.Show.Prec instance of Prelude.Classes.Ord, method >=_lam0}
def _idris_Prelude_46_Classes_46__123_Prelude_46_Show_46_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__62__61__95_lam0_125_(
  e0, e1
):
  while True:
    return _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Eq_36_Prec_58__33__61__61__58_0(
      e0, e1
    )

# Prelude.Show.{case block in showLitChar at ./Prelude/Show.idr:126:27_lam0}
def _idris_Prelude_46_Show_46__123_case_32_block_32_in_32_showLitChar_32_at_32__46__47_Prelude_47_Show_46_idr_58_126_58_27_95_lam0_125_(
  in0, in1
):
  while True:
    return (in0 + in1)

# {io_bind0}
def io_bind0(e0, e1, e2, e3, e4, _idris_w, in0):
  while True:
    return APPLY0(e4, in0)

# Prelude.Chars.{isDigit0}
def _idris_Prelude_46_Chars_46__123_isDigit0_125_(e0):
  while True:
    return _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Char_58__33__60__61__58_0(
      e0, u'9'
    )

# Main.{main0}
def _idris_Main_46__123_main0_125_(in1, in2):
  while True:
    return _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_String_58__33_compare_58_0(
      in1, in2
    )

# Prelude.Show.{primNumShow0}
def _idris_Prelude_46_Show_46__123_primNumShow0_125_(in1):
  while True:
    aux1 = (in1 == u'-')
    if aux1 == 0:
      return False
    else:
      return True
    return _idris_error("unreachable due to case in tail position")

# Prelude.Interactive.{putStr'0}
def _idris_Prelude_46_Interactive_46__123_putStr_39_0_125_(e1, in0):
  while True:
    return sys.stdout.write(e1)

# Main.{pythag0}
def _idris_Main_46__123_pythag0_125_(in2, in1, in0, in3):
  while True:
    return APPLY0(
      _idris_Prelude_46_Applicative_46_pure(None, None, (65736,)),  # {U_PE_@@constructor of Prelude.Monad.Monad#Applicative m_d05ad59e2}
      (in2, (in1, in0))
    )

# {runMain0}
def runMain0():
  while True:
    return EVAL0(APPLY0(_idris_Main_46_main(), None))

# Prelude.Show.{showLitChar0}
def _idris_Prelude_46_Show_46__123_showLitChar0_125_(in0):
  while True:
    return (u'\\a' + in0)

# Prelude.Show.{showLitString0}
def _idris_Prelude_46_Show_46__123_showLitString0_125_(in2):
  while True:
    return (u'\\"' + in2)

# Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam1}
def _idris_Prelude_46_Classes_46__123_Char_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
  in0
):
  while True:
    return (65651, in0)  # {U_Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam0}1}

# Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam1}
def _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
  in0
):
  while True:
    return (65657, in0)  # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam0}1}

# Prelude.Show.{Int instance of Prelude.Show.Show_lam1}
def _idris_Prelude_46_Show_46__123_Int_32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam1_125_(
  in1, in2
):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (65700,), in1, in2)  # {U_prim__toStrInt1}

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam1}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
  in0
):
  while True:
    return (65667, in0)  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam0}1}

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab1}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab1_125_(
  in2, in3
):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (65700,), in2, in3)  # {U_prim__toStrInt1}

# {PE_show_249676531}
def _idris__123_PE_95_show_95_249676531_125_(in2):
  while True:
    return _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_String_58__33_show_58_0(
      in2
    )

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam1}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam1_125_(
  in0
):
  while True:
    return (65677, in0)  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam0}1}

# {io_bind1}
def io_bind1(e0, e1, e2, e3, e4, _idris_w, in0):
  while True:
    return APPLY0(io_bind0(e0, e1, e2, e3, e4, _idris_w, in0), _idris_w)

# Main.{main1}
def _idris_Main_46__123_main1_125_(in1):
  while True:
    return (65638, in1)  # {U_Main.{main0}1}

# Prelude.Show.{primNumShow1}
def _idris_Prelude_46_Show_46__123_primNumShow1_125_(e0, e1, e2, e3, in0, in2, in3):
  while True:
    return (65684,)  # {U_Prelude.Show.{primNumShow0}1}

# Prelude.Interactive.{putStr'1}
def _idris_Prelude_46_Interactive_46__123_putStr_39_1_125_(in1):
  while True:
    return (65698, None, None, Unit)  # {U_io_return1}

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
      aux4 = APPLY0(_idris_Prelude_46_Applicative_46_pure(None, None, (65735,)), Unit)  # {U_PE_@@constructor of Prelude.Applicative.Alternative#Applicative f_5102bba82}
    return _idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
      None,
      None,
      aux4,
      (65641, in2, in1, in0)  # {U_Main.{pythag0}1}
    )

# Prelude.Show.{showLitChar1}
def _idris_Prelude_46_Show_46__123_showLitChar1_125_(in1):
  while True:
    return (u'\\b' + in1)

# Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam2}
def _idris_Prelude_46_Classes_46__123_Char_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
  in2, in3
):
  while True:
    aux1 = APPLY0(
      APPLY0(
        _idris_Prelude_46_Classes_46_compare(
          None,
          _idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Char()
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

# Prelude.Show.{Int instance of Prelude.Show.Show_lam2}
def _idris_Prelude_46_Show_46__123_Int_32_instance_32_of_32_Prelude_46_Show_46_Show_95_lam2_125_(
  in1
):
  while True:
    return (65675, in1)  # {U_Prelude.Show.{Int instance of Prelude.Show.Show_lam1}1}

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam2}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
  in2, in3
):
  while True:
    aux1 = APPLY0(
      APPLY0(
        _idris_Prelude_46_Classes_46_compare(
          None,
          _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat()
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

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab2}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab2_125_(
  in2
):
  while True:
    return (65712, in2)  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab1}1}

# {PE_show_249676532}
def _idris__123_PE_95_show_95_249676532_125_(in1):
  while True:
    return (65730,)  # {U_{PE_show_249676531}1}

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam2}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(
  in2, in3
):
  while True:
    aux1 = APPLY0(
      APPLY0(
        _idris_Prelude_46_Classes_46_compare(
          None,
          _idris_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec()
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
    return (65733, e0, e1, e2, e3, e4, _idris_w)  # {U_{io_bind1}1}

# Main.{main2}
def _idris_Main_46__123_main2_125_(in0):
  while True:
    return _idris_PE_95_printLn_39__95_cfef5baf(
      None,
      _idris_Prelude_46_List_46_sortBy(
        None,
        (65639,),  # {U_Main.{main1}1}
        ConsList().cons(u'baz').cons(u'bar').cons(u'foo')
      )
    )

# Prelude.Show.{primNumShow2}
def _idris_Prelude_46_Show_46__123_primNumShow2_125_(in0, e0, e1, e2, e3):
  while True:
    aux1 = _idris_Prelude_46_Strings_46_strM(in0)
    if aux1[0] == 1:  # Prelude.Strings.StrCons
      in2, in3 = aux1[1:]
      return APPLY0(
        _idris_Prelude_46_Show_46__123_primNumShow1_125_(e0, e1, e2, e3, in0, in2, in3),
        in2
      )
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
      (65642, in1, in0)  # {U_Main.{pythag1}1}
    )

# Prelude.Show.{showLitChar2}
def _idris_Prelude_46_Show_46__123_showLitChar2_125_(in2):
  while True:
    return (u'\\t' + in2)

# Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam3}
def _idris_Prelude_46_Classes_46__123_Char_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
  in2
):
  while True:
    return (65653, in2)  # {U_Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam2}1}

# Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam3}
def _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
  in2
):
  while True:
    return (65659, in2)  # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam2}1}

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam3}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
  in2
):
  while True:
    return (65669, in2)  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam2}1}

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab3}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab3_125_(
  in4
):
  while True:
    return _idris_PE_95_Prelude_46_Show_46__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_cfed4029(
      in4
    )

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam3}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam3_125_(
  in2
):
  while True:
    return (65679, in2)  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam2}1}

# Main.{pythag3}
def _idris_Main_46__123_pythag3_125_(in0):
  while True:
    return _idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(
      None,
      None,
      _idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(
        1, in0
      ),
      (65643, in0)  # {U_Main.{pythag2}1}
    )

# Prelude.Show.{showLitChar3}
def _idris_Prelude_46_Show_46__123_showLitChar3_125_(in3):
  while True:
    return (u'\\n' + in3)

# Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam4}
def _idris_Prelude_46_Classes_46__123_Char_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_(
  in4, in5
):
  while True:
    aux1 = APPLY0(
      APPLY0(
        _idris_Prelude_46_Classes_46_compare(
          None,
          _idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Char()
        ),
        in4
      ),
      in5
    )
    if aux1[0] == 2:  # Prelude.Classes.GT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam4}
def _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_(
  in4, in5
):
  while True:
    aux1 = APPLY0(
      APPLY0(
        _idris_Prelude_46_Classes_46_compare(
          None,
          _idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int()
        ),
        in4
      ),
      in5
    )
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
    aux1 = APPLY0(
      APPLY0(
        _idris_Prelude_46_Classes_46_compare(
          None,
          _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat()
        ),
        in4
      ),
      in5
    )
    if aux1[0] == 2:  # Prelude.Classes.GT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab4}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab4_125_(
  in7
):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (65700,), (0,), in7)  # {U_prim__toStrInt1}, Prelude.Show.Open

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam4}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam4_125_(
  in4, in5
):
  while True:
    aux1 = APPLY0(
      APPLY0(
        _idris_Prelude_46_Classes_46_compare(
          None,
          _idris_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec()
        ),
        in4
      ),
      in5
    )
    if aux1[0] == 2:  # Prelude.Classes.GT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.{showLitChar4}
def _idris_Prelude_46_Show_46__123_showLitChar4_125_(in4):
  while True:
    return (u'\\v' + in4)

# Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam5}
def _idris_Prelude_46_Classes_46__123_Char_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_(
  in4
):
  while True:
    return (65655, in4)  # {U_Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam4}1}

# Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam5}
def _idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_(
  in4
):
  while True:
    return (65661, in4)  # {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam4}1}

# Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam5}
def _idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_(
  in4
):
  while True:
    return (65671, in4)  # {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam4}1}

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab5}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab5_125_(
  in8, in9
):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (65700,), in8, in9)  # {U_prim__toStrInt1}

# Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam5}
def _idris_Prelude_46_Show_46__123_Prec_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam5_125_(
  in4
):
  while True:
    return (65681, in4)  # {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam4}1}

# Prelude.Show.{showLitChar5}
def _idris_Prelude_46_Show_46__123_showLitChar5_125_(in5):
  while True:
    return (u'\\f' + in5)

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab6}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab6_125_(
  in8
):
  while True:
    return (65723, in8)  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab5}1}

# Prelude.Show.{showLitChar6}
def _idris_Prelude_46_Show_46__123_showLitChar6_125_(in6):
  while True:
    return (u'\\r' + in6)

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab7}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab7_125_(
  in10
):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (65700,), (0,), in10)  # {U_prim__toStrInt1}, Prelude.Show.Open

# Prelude.Show.{showLitChar7}
def _idris_Prelude_46_Show_46__123_showLitChar7_125_(in7):
  while True:
    aux1 = (in7 == u'H')
    if aux1 == 0:
      return False
    else:
      return True
    return _idris_error("unreachable due to case in tail position")

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab8}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab8_125_(
  in11, in12
):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (65700,), in11, in12)  # {U_prim__toStrInt1}

# Prelude.Show.{showLitChar8}
def _idris_Prelude_46_Show_46__123_showLitChar8_125_(in8):
  while True:
    return (u'\\\\' + in8)

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab9}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab9_125_(
  in11
):
  while True:
    return (65726, in11)  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab8}1}

# Prelude.Show.{showLitChar9}
def _idris_Prelude_46_Show_46__123_showLitChar9_125_(in9):
  while True:
    return (u'\\DEL' + in9)

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab10}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab10_125_(
  in6
):
  while True:
    return _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
      None,
      None,
      None,
      None,
      (0, (65722,), (65724,)),  # constructor of Prelude.Show.Show, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab4}1}, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab6}1}
      (0, (65725,), (65727,)),  # constructor of Prelude.Show.Show, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab7}1}, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab9}1}
      in6
    )

# Prelude.Show.{showLitChar10}
def _idris_Prelude_46_Show_46__123_showLitChar10_125_(in10, in11):
  while True:
    return (in10 + in11)

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab11}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab11_125_(
  in5
):
  while True:
    return (65702,)  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab10}1}

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab12}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab12_125_(
  in0
):
  while True:
    return _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
      None,
      None,
      None,
      None,
      (0, (65701,), (65720,)),  # constructor of Prelude.Show.Show, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab0}1}, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab2}1}
      (0, (65721,), (65703,)),  # constructor of Prelude.Show.Show, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab3}1}, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab11}1}
      in0
    )

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab13}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab13_125_(
  in15
):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (65700,), (0,), in15)  # {U_prim__toStrInt1}, Prelude.Show.Open

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab14}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab14_125_(
  in16, in17
):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (65700,), in16, in17)  # {U_prim__toStrInt1}

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab15}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab15_125_(
  in16
):
  while True:
    return (65706, in16)  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab14}1}

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab16}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab16_125_(
  in18
):
  while True:
    return _idris_PE_95_Prelude_46_Show_46__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_44__32_method_32_show_95_cfed4029(
      in18
    )

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab17}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab17_125_(
  in21
):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (65700,), (0,), in21)  # {U_prim__toStrInt1}, Prelude.Show.Open

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab18}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab18_125_(
  in22, in23
):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (65700,), in22, in23)  # {U_prim__toStrInt1}

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab19}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab19_125_(
  in22
):
  while True:
    return (65710, in22)  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab18}1}

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab20}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab20_125_(
  in24
):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (65700,), (0,), in24)  # {U_prim__toStrInt1}, Prelude.Show.Open

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab21}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab21_125_(
  in25, in26
):
  while True:
    return _idris_Prelude_46_Show_46_primNumShow(None, (65700,), in25, in26)  # {U_prim__toStrInt1}

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab22}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab22_125_(
  in25
):
  while True:
    return (65714, in25)  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab21}1}

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab23}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab23_125_(
  in20
):
  while True:
    return _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
      None,
      None,
      None,
      None,
      (0, (65709,), (65711,)),  # constructor of Prelude.Show.Show, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab17}1}, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab19}1}
      (0, (65713,), (65715,)),  # constructor of Prelude.Show.Show, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab20}1}, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab22}1}
      in20
    )

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab24}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab24_125_(
  in19
):
  while True:
    return (65716,)  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab23}1}

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab25}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab25_125_(
  in14
):
  while True:
    return _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(
      None,
      None,
      None,
      None,
      (0, (65705,), (65707,)),  # constructor of Prelude.Show.Show, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab13}1}, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab15}1}
      (0, (65708,), (65717,)),  # constructor of Prelude.Show.Show, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab16}1}, {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab24}1}
      in14
    )

# {PE_(a, b) instance of Prelude.Show.Show_a94d79ab26}
def _idris__123_PE_95__40_a_44__32_b_41__32_instance_32_of_32_Prelude_46_Show_46_Show_95_a94d79ab26_125_(
  in13
):
  while True:
    return (65718,)  # {U_{PE_(a, b) instance of Prelude.Show.Show_a94d79ab25}1}

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
        return (e4 + APPLY0(_idris_Prelude_46_Show_46_show(None, e3), in0))
      else:
        e0, e1, e2, e3, e4, e5, = None, None, None, e3, (e4 + (APPLY0(_idris_Prelude_46_Show_46_show(None, e3), in0) + u', ')), in1,
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
        return APPLY0(APPLY0(_idris_Prelude_46_Classes_46__61__61_(None, (65738,)), in1), in0)  # {U_Prelude.Nat.Nat instance of Prelude.Classes.Eq2}
      return _idris_error("unreachable due to case in tail position")
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.Prelude.Classes.Ordering instance of Prelude.Classes.Eq, method ==
def _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Eq_36_Ordering_58__33__61__61__58_0(
  e0, e1
):
  while True:
    if e1[0] == 1:  # Prelude.Classes.EQ
      if e0[0] == 1:  # Prelude.Classes.EQ
        return True
      else:
        return False
      return _idris_error("unreachable due to case in tail position")
    elif e1[0] == 2:  # Prelude.Classes.GT
      if e0[0] == 2:  # Prelude.Classes.GT
        return True
      else:
        return False
      return _idris_error("unreachable due to case in tail position")
    elif e1[0] == 0:  # Prelude.Classes.LT
      if e0[0] == 0:  # Prelude.Classes.LT
        return True
      else:
        return False
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
      return APPLY0(
        APPLY0(e2, in0),
        APPLY0(
          APPLY0(
            APPLY0(_idris_Prelude_46_Foldable_46_foldr(None, None, None, (65741,)), e2),  # {U_Prelude.List.List instance of Prelude.Foldable.Foldable5}
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
    return _idris_PE_95_concatMap_95_af3155d1(None, None, e3, e2)

# Prelude.Classes.Prelude.Classes.Char instance of Prelude.Classes.Ord, method <=
def _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Char_58__33__60__61__58_0(
  e0, e1
):
  while True:
    aux1 = APPLY0(
      APPLY0(
        _idris_Prelude_46_Classes_46__60_(
          None,
          _idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Char()
        ),
        e0
      ),
      e1
    )
    if not aux1:  # Prelude.Bool.False
      return _idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Char_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__60__61__95_lam0_125_(
        e0, e1
      )
    else:  # Prelude.Bool.True
      return True
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.Prelude.Classes.Char instance of Prelude.Classes.Ord, method >=
def _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Char_58__33__62__61__58_0(
  e0, e1
):
  while True:
    aux1 = APPLY0(
      APPLY0(
        _idris_Prelude_46_Classes_46__62_(
          None,
          _idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Char()
        ),
        e0
      ),
      e1
    )
    if not aux1:  # Prelude.Bool.False
      return _idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Char_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__62__61__95_lam0_125_(
        e0, e1
      )
    else:  # Prelude.Bool.True
      return True
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.Prelude.Classes.Char instance of Prelude.Classes.Ord, method compare
def _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Char_58__33_compare_58_0(
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
        return APPLY0(
          APPLY0(
            _idris_Prelude_46_Classes_46_compare(
              None,
              _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat()
            ),
            in2
          ),
          in1
        )
      return _idris_error("unreachable due to case in tail position")
    return _idris_error("unreachable due to case in tail position")

# Prelude.Classes.Prelude.Show.Prec instance of Prelude.Classes.Ord, method >=
def _idris_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33__62__61__58_0(
  e0, e1
):
  while True:
    aux1 = APPLY0(
      APPLY0(
        _idris_Prelude_46_Classes_46__62_(
          None,
          _idris_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec()
        ),
        e0
      ),
      e1
    )
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

# Prelude.Classes.Prelude.Classes.String instance of Prelude.Classes.Ord, method compare
def _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_String_58__33_compare_58_0(
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
    return (u'(' + (APPLY0(_idris_Prelude_46_Show_46_show(None, e4), in0) + (u', ' + (APPLY0(_idris_Prelude_46_Show_46_show(None, e5), in1) + u')'))))
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.Prelude.Show.String instance of Prelude.Show.Show, method show
def _idris_Prelude_46_Show_46_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_String_58__33_show_58_0(
  e0
):
  while True:
    aux1 = _idris_Prelude_46_Strings_46_strM(e0)
    if aux1[0] == 1:  # Prelude.Strings.StrCons
      in0, in1 = aux1[1:]
      aux2 = _idris__95_Prelude_46_Strings_46_unpack_95_with_95_24(
        None,
        _idris_Prelude_46_Strings_46_strM(in1)
      ).cons(in0)
    else:  # Prelude.Strings.StrNil
      aux2 = ConsList()
    return (u'"' + APPLY0(_idris_Prelude_46_Show_46_showLitString(aux2), u'"'))

# Prelude.List.sortBy, splitRec
def _idris_Prelude_46_List_46_sortBy_58_splitRec_58_2(e0, e1, e2, e3, e4, e5):
  while True:
    if e4:  # Prelude.List.::
      in0, in1 = e4.head, e4.tail
      if e3:  # Prelude.List.::
        in2, in3 = e3.head, e3.tail
        if in3:  # Prelude.List.::
          in4, in5 = in3.head, in3.tail
          e0, e1, e2, e3, e4, e5, = None, None, None, in5, in1, (65648, None, None, None, e5, (65732, in0)),  # {U_Prelude.Basics..1}, {U_{Prelude.List.sortBy, splitRec_lam0}1}
          continue
          return _idris_error("unreachable due to tail call")
        else:
          return (APPLY0(e5, ConsList()), e4)
        return _idris_error("unreachable due to case in tail position")
      else:
        return (APPLY0(e5, ConsList()), e4)
      return _idris_error("unreachable due to case in tail position")
    else:
      return (APPLY0(e5, ConsList()), e4)
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.showLitChar, asciiTab
def _idris_Prelude_46_Show_46_showLitChar_58_asciiTab_58_10(e0):
  while True:
    return ConsList().cons(u'US').cons(u'RS').cons(u'GS').cons(u'FS').cons(u'ESC').cons(u'SUB').cons(u'EM').cons(u'CAN').cons(u'ETB').cons(u'SYN').cons(u'NAK').cons(u'DC4').cons(u'DC3').cons(u'DC2').cons(u'DC1').cons(u'DLE').cons(u'SI').cons(u'SO').cons(u'CR').cons(u'FF').cons(u'VT').cons(u'LF').cons(u'HT').cons(u'BS').cons(u'BEL').cons(u'ACK').cons(u'ENQ').cons(u'EOT').cons(u'ETX').cons(u'STX').cons(u'SOH').cons(u'NUL')

# Prelude.Show.showLitChar, getAt
def _idris_Prelude_46_Show_46_showLitChar_58_getAt_58_10(e0, e1, e2):
  while True:
    if e2:  # Prelude.List.::
      in0, in1 = e2.head, e2.tail
      if e1 == 0:
        return in0
      else:
        in2 = (e1 - 1)
        e0, e1, e2, = None, in2, in1,
        continue
        return _idris_error("unreachable due to tail call")
      return _idris_error("unreachable due to case in tail position")
    else:  # Prelude.List.Nil
      return None
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Strings.strM
def _idris__95_Prelude_46_Strings_46_strM_95_with_95_21(e0, e1):
  while True:
    if e1[0] == 1:  # Prelude.Basics.No
      return _idris_really_95_believe_95_me(None, None, (0,))  # Prelude.Strings.StrNil
    else:  # Prelude.Basics.Yes
      return _idris_really_95_believe_95_me(None, None, (1, e0[0], e0[1:]))  # Prelude.Strings.StrCons
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Strings.unpack
def _idris__95_Prelude_46_Strings_46_unpack_95_with_95_24(e0, e1):
  while True:
    if e1[0] == 1:  # Prelude.Strings.StrCons
      in0, in1 = e1[1:]
      return _idris__95_Prelude_46_Strings_46_unpack_95_with_95_24(
        None,
        _idris_Prelude_46_Strings_46_strM(in1)
      ).cons(in0)
    else:  # Prelude.Strings.StrNil
      return ConsList()
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Classes.Prelude.Show.Prec instance of Prelude.Classes.Ord, method <
def _idris__95_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33__60__58_0_95_with_95_25(
  e0, e1, e2
):
  while True:
    if e0[0] == 0:  # Prelude.Classes.LT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Classes.Prelude.Show.Prec instance of Prelude.Classes.Ord, method >
def _idris__95_Prelude_46_Classes_46_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec_58__33__62__58_0_95_with_95_27(
  e0, e1, e2
):
  while True:
    if e0[0] == 2:  # Prelude.Classes.GT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Show.firstCharIs
def _idris__95_Prelude_46_Show_46_firstCharIs_95_with_95_44(e0, e1, e2):
  while True:
    if e2[0] == 1:  # Prelude.Strings.StrCons
      in0, in1 = e2[1:]
      return APPLY0(e0, in0)
    else:  # Prelude.Strings.StrNil
      return False
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Classes.Prelude.Nat.Nat instance of Prelude.Classes.Ord, method <
def _idris__95_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat_58__33__60__58_0_95_with_95_82(
  e0, e1, e2
):
  while True:
    if e0[0] == 0:  # Prelude.Classes.LT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Classes.Prelude.Nat.Nat instance of Prelude.Classes.Ord, method >
def _idris__95_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat_58__33__62__58_0_95_with_95_84(
  e0, e1, e2
):
  while True:
    if e0[0] == 2:  # Prelude.Classes.GT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <
def _idris__95_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__58_0_95_with_95_96(
  e0, e1, e2
):
  while True:
    if e0[0] == 0:  # Prelude.Classes.LT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method >
def _idris__95_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__62__58_0_95_with_95_98(
  e0, e1, e2
):
  while True:
    if e0[0] == 2:  # Prelude.Classes.GT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Classes.Prelude.Classes.Char instance of Prelude.Classes.Ord, method <
def _idris__95_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Char_58__33__60__58_0_95_with_95_129(
  e0, e1, e2
):
  while True:
    if e0[0] == 0:  # Prelude.Classes.LT
      return True
    else:
      return False
    return _idris_error("unreachable due to case in tail position")

# with block in Prelude.Classes.Prelude.Classes.Char instance of Prelude.Classes.Ord, method >
def _idris__95_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Char_58__33__62__58_0_95_with_95_131(
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

# Prelude.Classes.Char instance of Prelude.Classes.Ord
def _idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Char():
  while True:
    return (0, (65652,), (65654,), (65656,))  # constructor of Prelude.Classes.Ord, {U_Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam1}1}, {U_Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam3}1}, {U_Prelude.Classes.{Char instance of Prelude.Classes.Ord_lam5}1}

# Prelude.Classes.Int instance of Prelude.Classes.Ord
def _idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int():
  while True:
    return (0, (65658,), (65660,), (65662,))  # constructor of Prelude.Classes.Ord, {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam1}1}, {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam3}1}, {U_Prelude.Classes.{Int instance of Prelude.Classes.Ord_lam5}1}

# Prelude.Nat.Nat instance of Prelude.Classes.Ord
def _idris_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat():
  while True:
    return (0, (65668,), (65670,), (65672,))  # constructor of Prelude.Classes.Ord, {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam1}1}, {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam3}1}, {U_Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam5}1}

# Prelude.Show.Prec instance of Prelude.Classes.Ord
def _idris_Prelude_46_Show_46__64_Prelude_46_Classes_46_Ord_36_Prec():
  while True:
    return (0, (65678,), (65680,), (65682,))  # constructor of Prelude.Classes.Ord, {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam1}1}, {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam3}1}, {U_Prelude.Show.{Prec instance of Prelude.Classes.Ord_lam5}1}

# Prelude.Show.Int instance of Prelude.Show.Show
def _idris_Prelude_46_Show_46__64_Prelude_46_Show_46_Show_36_Int():
  while True:
    return (0, (65674,), (65676,))  # constructor of Prelude.Show.Show, {U_Prelude.Show.{Int instance of Prelude.Show.Show_lam0}1}, {U_Prelude.Show.{Int instance of Prelude.Show.Show_lam2}1}

# Prelude.List.case block in sortBy at ./Prelude/List.idr:775:22
def _idris_Prelude_46_List_46_sortBy_95__95__95__95__95_Prelude_95__95_List_95__95_idr_95_775_95_22_95_case(
  e0, e1, e2, e3
):
  while True:
    assert True  # Builtins.MkPair
    in0, in1 = e3
    return _idris_Prelude_46_List_46_mergeBy(
      None,
      e1,
      _idris_Prelude_46_List_46_sortBy(None, e1, in0),
      _idris_Prelude_46_List_46_sortBy(None, e1, in1)
    )
    return _idris_error("unreachable due to case in tail position")

# Prelude.Show.case block in showLitChar at ./Prelude/Show.idr:126:27
def _idris_Prelude_46_Show_46_showLitChar_95__95__95__95__95_Prelude_95__95_Show_95__95_idr_95_126_95_27_95_case(
  e0, e1
):
  while True:
    if e1 is not None:  # Prelude.Maybe.Just
      in0 = e1
      return (65648, None, None, None, (65699, u'\\'), (65683, in0))  # {U_Prelude.Basics..1}, {U_prim__strCons1}, {U_Prelude.Show.{case block in showLitChar at ./Prelude/Show.idr:126:27_lam0}1}
    else:  # Prelude.Maybe.Nothing
      aux2 = _idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Char_58__33_compare_58_0(
        e0,
        u'\u007f'
      )
      if aux2[0] == 2:  # Prelude.Classes.GT
        aux3 = True
      else:
        aux3 = False
      aux1 = aux3
      if not aux1:  # Prelude.Bool.False
        return (65699, e0)  # {U_prim__strCons1}
      else:  # Prelude.Bool.True
        return (
          65648,  # {U_Prelude.Basics..1}
          None,
          None,
          None,
          (65699, u'\\'),  # {U_prim__strCons1}
          (
            65673,  # {U_Prelude.Show.protectEsc1}
            (65650,),  # {U_Prelude.Chars.isDigit1}
            _idris_Prelude_46_Show_46_primNumShow(None, (65700,), (0,), ord(e0))  # {U_prim__toStrInt1}, Prelude.Show.Open
          )
        )
      return _idris_error("unreachable due to case in tail position")
    return _idris_error("unreachable due to case in tail position")

# case block in io_bind at IO.idr:109:34
def _idris_io_95_bind_95_IO_95__95_idr_95_109_95_34_95_case(
  e0, e1, e2, e3, e4, e5, e6, e7
):
  while True:
    return APPLY0(e7, e5)

# case block in Void
def _idris_Void_95_case():
  while True:
    return None

# <<Void eliminator>>
def _idris_Void_95_elim():
  while True:
    return None

if __name__ == '__main__':
  runMain0()
