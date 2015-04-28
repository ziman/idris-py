module Python.BeautifulSoup

import Python

Element : PySig
Element =
  [ "string"  ::: String
  , "strings" ::: Iterator String
  ]

Soup : PySig
Soup =
  [ "select" ::: [String] ~> Iterator (Object Element)
  ]

Bs4 : PySig
Bs4 =
  [ "BeautifulSoup" ::: [String] ~> Object Soup
  ]
