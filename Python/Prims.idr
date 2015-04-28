module Python.Prims

data PyString : PySig where
    PyString_strip : PyString "strip" (FMethod [] String)
