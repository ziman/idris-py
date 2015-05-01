# Python back-end for Idris

WARNING: This is just a toy back-end (see [license](https://github.com/ziman/idris-py/blob/master/CRAPL-LICENSE.txt)), I probably won't maintain it actively.

## Goodies

* tail-call optimisation (does not work for mutual recursion)
* principled codegen monad makes it easy to compile from `DExp`
* allows typechecked use of Python libraries ([example](https://github.com/ziman/idris-py/blob/master/example.idr))
	- thanks to signatures for Python objects ([example](https://github.com/ziman/idris-py/blob/master/Python/BeautifulSoup.idr)).
* error reflection for messages like `Field "gets" does not exist in object signature "Session"`
* `foreach` -- higher-order FFI :)
* big case trees compiled to binary search trees
	- seems to bring down `pythag 100` from 5.5 secs to 3.5 secs, probably because of `APPLY0`
* comments in the generated Python code show the meaning of low-level code
    - constructor names next to numeric constructor tags
    - readable names next to mangled names
* neat exceptions (no hierarchy yet, though)
```idris
main = do
  OK ret <- try $ os /. "mkdir" $: ["/root/hello"]
    | Catch OSError e => putStrLn ("  -> OSError as expected: " ++ show e)
    | Catch _ e => putStrLn ("  -> some other error: " ++ show e)
  putStrLn $ "Your root could probably use some security lessons!"
```

## Observations

* it turns out that using `Text` to represent generated code
  ([branch](https://github.com/ziman/idris-py/tree/text)) is not that much win
    - strict `Text` seems to be a bit slower than `String`
    - lazy `Text` seems to be about as fast as `String`
    - `String` is the simplest

## Example

```bash
$ cabal sandbox init  # possibly with --sandbox /path/to/idris/.cabal-sandbox
$ cabal configure && cabal build
$ export PATH="$PATH:$PWD/dist/build/idris-py"
$ idris example.idr --codegen py -o example.py

$ pip install requests bs4
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
