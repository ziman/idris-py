import Python

mysum : Nat -> List Nat -> Nat
mysum acc [] = acc
mysum acc (x :: xs) = mysum (x + acc) xs

main : PIO ()
main = do
  reqs <- import_ Py_Requests "requests"
  session <- reqs ./ "Session" .$ []
  resp <- session ./ "get" .$ ["http://idris-lang.org"]
  html <- resp ./ "text"

  putStrLn html

  putStrLn $ "Hello world!"
  putStrLn $ "sum [1..10] = " ++ show (mysum 0 [1..10])
