module Python.Requests

import Python

Response : PySig
Response =
  [ "text" ::: String
  ]

Session : PySig
Session =
  [ "get" ::: [String] ~> Object Response
  ]

Requests : PySig
Requests =
  [ "Session" ::: [] ~> Object Session
  ]
