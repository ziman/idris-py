module Main

import Python
import Python.Prim
import Python.Exceptions

-- These modules contain signatures for Python libraries.
import Python.Lib.Os
import Python.Lib.Requests
import Python.Lib.BeautifulSoup
import Python.Lib.Queue
import Python.Lib.Threading

%default total

-- Even though field names are strings,
-- everything is typechecked according to the signatures imported above.

partial
main : PIO ()
main = do
  reqs <- Requests.import_

  -- (/) extracts the named attribute
  -- ($) calls a function
  -- (/.) and ($.) work with pure LHS
  -- (/:) and ($:) work with monadic LHS (useful for chaining)
  --
  -- equivalent to: session = reqs.Session()
  session <- reqs /. "Session" $. []

  -- equivalent to: html = session.get("http://idris-lang.org").text
  -- Notice that chaining is not a problem.
  html <- session /. "get" $. ["http://idris-lang.org"] /: "text"

  -- import Beautiful Soup
  bs4 <- BeautifulSoup.import_

  -- construct soup from HTML
  soup <- bs4 /. "BeautifulSoup" $. [html, Parsers.HTML]

  -- get the iterator over <li> elements, given by CSS selector
  features <- soup /. "select" $. ["div.entry-content li"]

  -- print all <li> elements as features
  putStrLn' $ "Idris has got the following exciting features:"
  count <- iterate features 0 $ \i : Int, li : Obj Element => do
    -- collect : Iterator a -> PIO (List a)
    line <- concat <$> collect (li /. "strings")
    putStrLn' $ show (i+1) ++ ". " ++ line
    pure $ i + 1

  putStrLn' $ "Total number of features: " ++ show count
  putStrLn' ""


  -- ###  Concurrency  ###

  let thread : (String -> PIO Nat) = \name => do
    putStrLn' $ "thread " ++ name ++ " starting"
    html <- session /. "get" $. ["http://idris-lang.org"] /: "text"
    putStrLn' $ "thread " ++ name ++ " done"
    pure $ length html

  thrA <- forkPIO $ thread "A"
  thrB <- forkPIO $ thread "B"
  resA <- wait thrA
  resB <- wait thrB

  putStrLn' $ "thread A says " ++ show resA
  putStrLn' $ "thread B says " ++ show resB
  putStrLn' ""


  -- ###  Exceptions  ###

  os <- Os.import_
  putStrLn' "And now, let's fail!"

  -- the standard try-catch variant
  try (do
    os /. "mkdir" $. ["/root/hello"]
    putStrLn' $ "Something's wrong, your root's homedir is writable!"
  ) `catch` (\etype, e => case etype of
    OSError => putStrLn' $ "  -> (1) everything's fine: " ++ showException e
    _       => raise e
  )

  -- Idris sugar, originally used in Effects
  OK ret <- try $ os /. "mkdir" $. ["/root/hello"]
    | Except OSError e => putStrLn' ("  -> (2) everything's fine: " ++ showException e)
    | Except _       e => raise e
  putStrLn' $ "Your root could probably use some security lessons!"

exports : FFI_Export FFI_Py "example.py" []
exports =
    Fun greet "greet" $
    End
  where
    greet : String -> PIO ()
    greet name = putStrLn' $ "Hello " ++ name ++ "!"
