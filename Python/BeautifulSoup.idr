module Python.BeautifulSoup

import Python

Element : PySig
Element = MkPySig "Element"
  [ "string"  ::: String
  , "strings" ::: Iterator String
  ]

Soup : PySig
Soup = MkPySig "Soup"
  [ "select" ::: [String] ~> Iterator (Object Element)
  ]

Bs4 : PySig
Bs4 = MkPySig "Bs4"
  [ "BeautifulSoup" ::: [String] ~> Object Soup
  ]
