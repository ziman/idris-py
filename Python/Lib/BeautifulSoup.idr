module Python.Lib.BeautifulSoup

import Python
import Python.Prim

%access public
%default total

Element : Signature
Element = signature "Element"
  [ "string"  ::: Maybe String
  , "strings" ::. PyList String
  ]

Soup : Signature
Soup = signature "Soup"
  [ "select" ::. [String] ~> Obj (PyList $ Obj Element)
  ]

Bs4 : Signature
Bs4 = signature "Bs4"
  [ "BeautifulSoup" ::. [String] ~> Obj Soup
  ]

import_ : PIO $ Obj Bs4
import_ = importModule "bs4"
