module Python.Exceptions

import Python

%default total
%access public

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
  Except : (etype : ExceptionType) -> (e : Exception) -> Result a

||| Catch exceptions in the given PIO action.
abstract
try : PIO a -> PIO (Result a)
try {a = a} x =
  unRaw <$>
    foreign
      FFI_Py
      "idris_try"
      (Raw (PIO a)
        -> (String -> Exception -> Raw (Result a))
        -> (Raw a -> Raw (Result a))
        -> PIO (Raw $ Result a)
      )
      (MkRaw x)
      (\n, e => MkRaw $ Except (fromString n) e)
      (MkRaw . OK . unRaw)

abstract
raise : Exception -> PIO a
raise {a = a} e = unRaw <$> foreign FFI_Py "idris_raise" (Exception -> PIO (Raw a)) e

catch : PIO (Result a) -> (ExceptionType -> Exception -> PIO a) -> PIO a
catch action handler = do
  OK result <- action
    | Except etype e => handler etype e
  return result

||| Get basic information about the exception as `String`.
abstract
showException : Exception -> String
showException e =
  unsafePerformIO
    $ foreign FFI_Py "str" (Exception -> PIO String) e

instance Show Exception where
  show = showException
