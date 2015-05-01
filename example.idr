import Python
import Python.Prim
import Python.Exceptions

-- These modules contain signatures for Python libraries.
import Python.Lib.Os
import Python.Lib.Requests
import Python.Lib.BeautifulSoup

%default total

infixr 1 =<<
(=<<) : Monad m => (a -> m b) -> m a -> m b
(=<<) f x = x >>= f

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
  session <- reqs /. "Session" $: []

  -- equivalent to: html = session.get("http://idris-lang.org").text
  -- Notice that chaining is not a problem.
  html <- session /. "get" $: ["http://idris-lang.org"] /: "text"

  -- import Beautiful Soup
  bs4 <- BeautifulSoup.import_

  -- construct soup from HTML
  soup <- bs4 /. "BeautifulSoup" $: [html]

  -- get the iterator over <li> elements, given by CSS selector
  -- First, we call the select() method, then we cast the result to Iterable.
  features <- soup /. "select" $: ["div.entry-content li"] >: Iterable (Obj Element)

  -- print all <li> elements as features
  putStrLn $ "Idris has got the following exciting features:"
  count <- iterate features 0 $ \i : Int, li => do
    -- collect : Iterator a -> PIO (List a)
    line <- map concat . collect =<< li /. "strings" >: Iterable String
    putStrLn $ show (i+1) ++ ". " ++ line
    return $ i + 1

  putStrLn $ "Total number of features: " ++ show count
  putStrLn ""

  -- let's test some exceptions
  os <- Os.import_
  putStrLn "And now, let's fail!"

  -- the standard try-catch variant
  try (do
    os /. "mkdir" $: ["/root/hello"]
    putStrLn $ "Something's wrong, your root's homedir is writable!"
  ) `catch` (\etype, e => case etype of
    OSError => putStrLn $ "  -> (1) everything's fine: " ++ show e
    _       => raise e
  )

  -- the special Idris sugar, originally used in Effects
  OK ret <- try $ os /. "mkdir" $: ["/root/hello"]
    | Except OSError e => putStrLn ("  -> (2) everything's fine: " ++ show e)
    | Except _       e => raise e
  putStrLn $ "Your root could probably use some security lessons!"

