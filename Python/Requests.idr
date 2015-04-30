module Python.Requests

import Python

Response : Signature
Response = MkSignature "Response"
  [ "text" ::: String
  ]

Session : Signature
Session = MkSignature "Session"
  [ "get" ::: [String] ~> Object Response
  ]

Requests : Signature
Requests = MkSignature "Requests"
  [ "Session" ::: [] ~> Object Session
  ]

import_ : PIO $ Object Requests
import_ = importModule "requests"
