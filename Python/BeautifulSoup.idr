module Python.BeautifulSoup

import Python
import Python.Prim

%access public
%default total

Element : Signature
Element = signature "Element"
  [ "string"  ::: String
  , "strings" ::: Object (PyList String)
  ]

Soup : Signature
Soup = signature "Soup"
  [ "select" ::: [String] ~> Object (PyList $ Object Element)
  ]

Bs4 : Signature
Bs4 = signature "Bs4"
  [ "BeautifulSoup" ::: [String] ~> Object Soup
  ]

import_ : PIO $ Object Bs4
import_ = importModule "bs4"
