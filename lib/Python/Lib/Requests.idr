module Python.Lib.Requests

import Python

%access public
%default total

abstract record Response where
  ptr : Dyn

Response_sig : Signature
Response_sig f = case f of
  "text" => Attr String
  _ => Object_sig f

instance Object Response Response_sig where {}

abstract record Session where
  ptr : Dyn

Session_sig : Signature
Session_sig f = case f of
  "get" => [String] ~> Response
  _ => Object_sig f

instance Object Session Session_sig where {}

abstract record Requests where
  ptr : Dyn

Requests_sig : Signature
Requests_sig f = case f of
  "Session" => [] ~> Session
  _ => Module_sig f

instance Object Requests Requests_sig where {}

import_ : PIO Requests
import_ = importModule "requests"
