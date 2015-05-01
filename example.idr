import Python

-- These modules contain signatures for Python classes.
import Python.Prim
import Python.Requests
import Python.BeautifulSoup

%default total

infixr 2 =<<
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
  html <- session /. "get" $: ["http://idris-lang.org"] /: "text"

  -- import Beautiful Soup
  bs4 <- BeautifulSoup.import_

  -- construct soup from HTML
  soup <- bs4 /. "BeautifulSoup" $: [html]

  -- get the iterator over <li> elements, given by CSS selector
  features <- soup /. "select" $: ["div.entry-content li"] >: Iterable (Object Element)

  -- print all <li> elements as features
  putStrLn $ "Idris has got the following exciting features:"
  count <- foreach features 0 $ \i : Int, li => do
    -- collect : Iterator a -> PIO (List a)
    line <- map concat . collect =<< (li /. "strings" >: Iterable String)
    putStrLn $ show (i+1) ++ ". " ++ line
    return $ i + 1

  putStrLn $ "Total number of features: " ++ show count

  -- test some exceptions
  putStrLn ""
  putStrLn "And now, let's print NULL!"
  Right unit <- try (putStrLn =<< return (believe_me prim__null))
    | Left e => do
        putStrLn $ "...aand it causes an exception, as it should."
        putStrLn $ "The message is: " ++ show e
  putStrLn $ "strange, printing null didn't fail and we got back this: " ++ show unit
