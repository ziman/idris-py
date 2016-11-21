module Python.Lib.Requests

import Python

%access public export
%default total

Response : Signature
Response f = case f of
  "text" => Attr String
  _ => Object f

Session : Signature
Session f = case f of
  "get" => [String] ~~> Obj Response
  _ => Object f

Requests : Signature
Requests f = case f of
  "Session" => [] ~~> Obj Session
  _ => Module f

import_ : PIO $ Obj Requests
import_ = importModule "requests"
