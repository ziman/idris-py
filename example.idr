import Python

-- These modules contain signatures for Python classes.
import Python.Prim
import Python.Requests
import Python.BeautifulSoup

infixr 2 =<<
(=<<) : Monad m => (a -> m b) -> m a -> m b
(=<<) f x = x >>= f

-- Although the following code seems stringly typed, fear not!
-- Everything is typechecked according to the signatures imported above.

main : PIO ()
main = do
  reqs <- import_ Requests "requests"

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
  bs4 <- import_ Bs4 "bs4"

  -- construct soup from HTML
  soup <- bs4 /. "BeautifulSoup" $: [html]

  -- get the iterator over <li> elements, given by CSS selector
  features <- soup /. "select" $: ["div.entry-content li"]

  putStrLn $ "Idris has got the following exciting features:"
  count <- foreach features 0 $ \i : Int, li => do
    -- collect : Iterator a -> PIO (List a)
    line <- map concat . collect =<< li /. "strings"
    putStrLn $ show (i+1) ++ ". " ++ line
    return $ i + 1

  putStrLn $ "Total number of features: " ++ show count
