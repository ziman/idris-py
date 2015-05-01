module Python.Requests

import Python

%access public
%default total

Response : Signature
Response = signature "Response"
  [ "text" ::: String
  ]

Session : Signature
Session = signature "Session"
  [ "get" ::: [String] ~> Obj Response
  ]

Requests : Signature
Requests = signature "Requests"
  [ "Session" ::: [] ~> Obj Session
  ]

import_ : PIO $ Obj Requests
import_ = importModule "requests"
