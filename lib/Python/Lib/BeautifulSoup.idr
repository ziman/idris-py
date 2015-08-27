module Python.Lib.BeautifulSoup

import Python

%access public
%default total

abstract
record Element where
  ptr : Dyn

Element_sig : Signature
Element_sig f = case f of
  "string"  => Attr $ Maybe String
  "strings" => Attr $ PyList String
  _ => Object_sig f

instance Object Element Element_sig where {}

abstract
record Soup where
  ptr : Dyn

Soup_sig : Signature
Soup_sig f = case f of
  "select" => [String] ~> PyList Element
  _ => Object_sig f

instance Object Soup Soup_sig where {}

abstract
record Bs4 where
  ptr : Dyn

Bs4_sig : Signature
Bs4_sig f = case f of
  "BeautifulSoup" => [String] ~> Soup
  _ => Module_sig f

instance Object Bs4 Bs4_sig where {}

import_ : PIO Bs4
import_ = importModule "bs4"
