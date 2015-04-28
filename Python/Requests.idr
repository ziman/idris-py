module Python.Requests

import Python

Response : PySig
Response = MkPySig "Response"
  [ "text" ::: String
  ]

Session : PySig
Session = MkPySig "Session"
  [ "get" ::: [String] ~> Object Response
  ]

Requests : PySig
Requests = MkPySig "Requests"
  [ "Session" ::: [] ~> Object Session
  ]
