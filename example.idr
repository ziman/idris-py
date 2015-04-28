import Python
import Python.Requests
import Python.BeautifulSoup

infixr 2 =<<
(=<<) : Monad m => (a -> m b) -> m a -> m b
(=<<) f x = x >>= f

main : PIO ()
main = do
  reqs <- import_ Requests
  session <- reqs /. "Session" $: []
  html <- session /. "get" $: ["http://idris-lang.org"] /: "text"

  bs4 <- import_ Bs4
  soup <- bs4 /. "BeautifulSoup" $: [html]

  features <- soup /. "select" $: ["div.entry-content li"]

  putStrLn $ "Idris has got the following exciting features:"
  count <- foreach features 0 $ \i : Int, li => do
    line <- map concat . collect =<< li /. "strings"
    putStrLn $ show (i+1) ++ ". " ++ line
    return $ i + 1

  putStrLn $ "Total number of features: " ++ show count
