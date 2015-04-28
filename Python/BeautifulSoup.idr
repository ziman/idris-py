module Python.BeautifulSoup

import Python

data Element : PySig where
  Element_string : Element "string" String
  Element_strings : Element "strings" (Iterator String)

data BeautifulSoup : PySig where
  Soup_select : BeautifulSoup "select" ([String] ~> Iterator (Object Element))

data Bs4 : PySig where
  Bs4_Soup : Bs4 "BeautifulSoup" ([String] ~> Object BeautifulSoup)

instance Importable Bs4 where
  moduleName _ = "bs4"
