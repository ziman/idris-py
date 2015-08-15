module Python.Builtins

import Python
import Python.IO
import Python.RTS

%default total
%access abstract

builtins : Ref
builtins = unsafePerformIO $ getGlobal "__builtins__"

%assert_total
private
mk : String -> Ref
mk name = unsafePerformIO $ builtins /. name

int : Ref
int = mk "int"

float : Ref
float = mk "float"

bool : Ref
bool = mk "bool"

str : Ref
str = mk "str"

list : Ref
list = mk "list"

toList : Ref -> Ref
toList x = unsafePerformIO $ list $. [x]

dict : Ref
dict = mk "dict"

set : Ref
set = mk "set"

tuple : Ref
tuple = mk "tuple"
