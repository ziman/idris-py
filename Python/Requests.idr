module Python.Requests

import Python

data Response : PySig where
  Response_text : Response "text" String

data Session : PySig where
  Session_get : Session "get" ([String] ~> Object Response)

data Requests : PySig where
  Requests_Session : Requests "Session" ([] ~>Object Session)

instance Importable Requests where
  moduleName _ = "requests"
