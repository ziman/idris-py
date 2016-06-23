module Python.Lib.BeautifulSoup

import Python
import Python.Prim

%access public export
%default total

namespace Parsers

  LXML : String
  LXML = "lxml"

  HTML : String
  HTML = "html.parser"

Element : Signature
Element f = case f of
  "string"  => Attr $ Maybe String
  "strings" => Attr . Obj $ PyList String
  _ => Object f

Soup : Signature
Soup f = case f of
  "select" => [String] ~~> Obj (PyList $ Obj Element)
  _ => Object f

Bs4 : Signature
Bs4 f = case f of
  "BeautifulSoup" => [String, String] ~~> Obj Soup
  _ => Module f

import_ : PIO $ Obj Bs4
import_ = importModule "bs4"
