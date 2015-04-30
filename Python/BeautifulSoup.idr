module Python.BeautifulSoup

import Python

Element : Signature
Element = MkSignature "Element"
  [ "string"  ::: String
  , "strings" ::: Iterator String
  ]

Soup : Signature
Soup = MkSignature "Soup"
  [ "select" ::: [String] ~> Iterator (Object Element)
  ]

Bs4 : Signature
Bs4 = MkSignature "Bs4"
  [ "BeautifulSoup" ::: [String] ~> Object Soup
  ]

import_ : PIO $ Object Bs4
import_ = importModule "bs4"
