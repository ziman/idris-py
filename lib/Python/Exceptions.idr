module Python.Exceptions

import Python.Objects
import Python.Fields
import Python.IO

%default total
%access public export

Exception : Signature
Exception f = case f of
  "message" => Attr String
  _ => Object f

||| Standard Python exceptions.
data ExceptionType : Type where
  StopIteration : ExceptionType
  StandardError : ExceptionType
  BufferError : ExceptionType
  ArithmeticError : ExceptionType
  FloatingPointError : ExceptionType
  OverflowError : ExceptionType
  ZeroDivisionError : ExceptionType
  AssertionError : ExceptionType
  AttributeError : ExceptionType
  EnvironmentError : ExceptionType
  IOError : ExceptionType
  OSError : ExceptionType
  WindowsError : ExceptionType
  VMSError : ExceptionType
  EOFError : ExceptionType
  ImportError : ExceptionType
  LookupError : ExceptionType
  IndexError : ExceptionType
  KeyError : ExceptionType
  MemoryError : ExceptionType
  NameError : ExceptionType
  UnboundLocalError : ExceptionType
  ReferenceError : ExceptionType
  RuntimeError : ExceptionType
  NotImplementedError : ExceptionType
  SyntaxError : ExceptionType
  IndentationError : ExceptionType
  TabError : ExceptionType
  SystemError : ExceptionType
  TypeError : ExceptionType
  ValueError : ExceptionType
  UnicodeError : ExceptionType
  UnicodeDecodeError : ExceptionType
  UnicodeEncodeError : ExceptionType
  UnicodeTranslateError : ExceptionType
  Other : String -> ExceptionType

private
fromString : String -> ExceptionType
fromString s = case s of
  "StopIteration" => StopIteration
  "StandardError" => StandardError
  "BufferError" => BufferError
  "ArithmeticError" => ArithmeticError
  "FloatingPointError" => FloatingPointError
  "OverflowError" => OverflowError
  "ZeroDivisionError" => ZeroDivisionError
  "AssertionError" => AssertionError
  "AttributeError" => AttributeError
  "EnvironmentError" => EnvironmentError
  "IOError" => IOError
  "OSError" => OSError
  "WindowsError" => WindowsError
  "VMSError" => VMSError
  "EOFError" => EOFError
  "ImportError" => ImportError
  "LookupError" => LookupError
  "IndexError" => IndexError
  "KeyError" => KeyError
  "MemoryError" => MemoryError
  "NameError" => NameError
  "UnboundLocalError" => UnboundLocalError
  "ReferenceError" => ReferenceError
  "RuntimeError" => RuntimeError
  "NotImplementedError" => NotImplementedError
  "SyntaxError" => SyntaxError
  "IndentationError" => IndentationError
  "TabError" => TabError
  "SystemError" => SystemError
  "TypeError" => TypeError
  "ValueError" => ValueError
  "UnicodeError" => UnicodeError
  "UnicodeDecodeError" => UnicodeDecodeError
  "UnicodeEncodeError" => UnicodeEncodeError
  "UnicodeTranslateError" => UnicodeTranslateError
  other => Other other

||| Result of try-catch.
data Result : Type -> Type where
  ||| No exception was raised, `PIO` action was performed normally.
  OK : (x : a) -> Result a

  ||| An exception was raised.
  Except : (etype : ExceptionType) -> (e : Obj Exception) -> Result a

||| Catch exceptions in the given PIO action.
export
try : PIO a -> PIO (Result a)
try {a = a} x = do
    MkRaw r <- foreign
      FFI_Py
      "_idris_try"
      (Raw (PIO a)
        -> (Obj Exception -> Raw (Either (Obj Exception) a))
        -> (Raw a -> Raw (Either (Obj Exception) a))
        -> PIO (Raw $ Either (Obj Exception) a)
      )
      (MkRaw x)
      (MkRaw . Left)
      (MkRaw . Right . unRaw)

    case r of
      Right x => pure $ OK x
      Left e => do
        let et = e /. "__class__" /. "__name__"
        pure $ Except (fromString et) e

export
raise : Obj Exception -> PIO a
raise {a = a} e = unRaw <$> foreign FFI_Py "_idris_raise" (Obj Exception -> PIO (Raw a)) e

catch : PIO (Result a) -> (ExceptionType -> Obj Exception -> PIO a) -> PIO a
catch action handler = do
  OK result <- action
    | Except etype e => handler etype e
  pure result

||| Get basic information about the exception as `String`.
export
showException : Obj Exception -> String
showException e =
  unsafePerformIO
    $ foreign FFI_Py "str" (Obj Exception -> PIO String) e

{-
instance Show (Obj Exception) where
  show = showException
-}
