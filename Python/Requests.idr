module Python.Requests

import Python

Response : Signature
Response = signature "Response"
  [ "text" ::: String
  ]

Session : Signature
Session = signature "Session"
  [ "get" ::: [String] ~> Object Response
  ]

Requests : Signature
Requests = signature "Requests"
  [ "Session" ::: [] ~> Object Session
  ]

import_ : PIO $ Object Requests
import_ = importModule "requests"
