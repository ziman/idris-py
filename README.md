# Python back-end for Idris

WARNING: This is just a toy back-end (see [license](https://github.com/ziman/idris-py/blob/master/CRAPL-LICENSE.txt)), I probably won't maintain it actively.

## Goodies

* tail-call optimisation (does not work for mutual recursion)
* principled codegen monad makes it easy to compile from `DExp`
* allows typechecked use of Python libraries ([example](https://github.com/ziman/idris-py/blob/master/example.idr))
	- thanks to signatures for Python objects ([example](https://github.com/ziman/idris-py/blob/master/Python/BeautifulSoup.idr)).
* error reflection yields `Field "gets" does not exist in object signature "Session"` instead of
```idris
        Can't solve goal 
                Elem ("gets" ::: Method margs ret)
                     [("get" :::
                       Method (Fixed [String]) (Object [("text" ::: String)]))]
```
* `foreach` -- higher-order FFI :)

## Example

```bash
$ pip install requests bs4
$ idris example.idr --codegen py -o example.py
$ python example.py
Idris has got the following exciting features:
1. Full dependent types with dependent pattern matching
2. Simple foreign function interface (to C)
3. Compiler-supported interactive editing: the compiler helps you write code using the types
4. where clauses, with rule, simple case expressions, pattern matching let and lambda bindings
5. Dependent records with projection and update
6. Type classes
7. Type-driven overloading resolution
8. do notation and idiom brackets
9. Indentation significant syntax
10. Extensible syntax
11. Cumulative universes
12. Totality checking
13. Hugs style interactive environment
Total number of features: 13
```
