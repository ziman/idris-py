import Python
import Python.Requests
import Python.BeautifulSoup

infixr 2 =<<
(=<<) : Monad m => (a -> m b) -> m a -> m b
(=<<) f x = x >>= f

mysum : Nat -> List Nat -> Nat
mysum acc [] = acc
mysum acc (x :: xs) = mysum (x + acc) xs

main : PIO ()
main = do
  reqs <- import_ Requests
  session <- reqs ./ "Session" .$ []
  resp <- session ./ "get" .$ ["http://idris-lang.org"]
  html <- resp ./ "text"

  bs4 <- import_ Bs4
  soup <- bs4 ./ "BeautifulSoup" .$ [html]

  features <- soup ./ "select" .$ ["div.entry-content li"]
  cnt <- foreach features 0 (\cnt, li => do
    putStrLn =<< (li ./ "string")
    return $ cnt + 1
  )

  putStrLn $ "Total number of features: " ++ show cnt
