# Python back-end for Idris

## Goodies

* expressions in tail positions return directly
* if-elif sequences optimised
    - not doing comparison if it's clear which branch to take
    - single-branch trees turned into assertions (ignore them with `python -O`) and flattened
* tail-call optimisation (does not work for mutual recursion)
    - There is a [full TCO branch](https://github.com/ziman/idris-py/tree/trampoline-tco)
      using trampolines but it consumes more stack frames, thus making non-tail-recursive
      programs crash earlier so it's not merged into master.
    - [Another full TCO branch](https://github.com/ziman/idris-py/tree/inline-tco) preserves
      the number of stack frames but it's even slower.
* principled codegen monad makes it easy to compile from `DExp`
* allows typechecked use of Python libraries
  ([example](https://github.com/ziman/idris-py/blob/master/examples/example.idr))
	- thanks to signatures for Python objects
      ([example](https://github.com/ziman/idris-py/blob/master/lib/Python/Lib/BeautifulSoup.idr)).
    - fully dependent type signatures are supported
      ([example](https://github.com/ziman/idris-py/blob/master/lib/Python/Lib/Queue.idr#L18)),
      including arguments with defaults
* allows duck typing ([example](https://github.com/ziman/idris-py/blob/master/lib/Python/Functions.idr#L30))
* error reflection yields messages like:
    - `Field "gets" does not exist in object signature "Session"`
    - `Iterable Int is not mixed into signature "Response"`
* `foreach` -- higher-order FFI :)
* big case trees compiled specially
    - constructor-cases to binary search on tags
	    - seems to bring down `pythag 100` from 5.5 secs to 3.5 secs, probably because of `APPLY0`
	- constant-cases to dictionary lookups
        - a bit wasteful (evaluates too much in non-trivial cases) -- but apparently easy to do
* comments in the generated Python code show the meaning of low-level code
    - constructor names next to numeric constructor tags
    - readable names next to mangled names
* exceptions (no hierarchy yet, though) ([example](https://github.com/ziman/idris-py/blob/master/examples/example.idr#L80))
* threading, message passing and `forkPIO` ([example](https://github.com/ziman/idris-py/blob/master/examples/example.idr#L62))
* `Just x` compiles to `x`, `Nothing` compiles to `None`
    - this gives choice to FFI authors to say whether they care about `None`
      by having FFI functions take/return either bare values or maybe-values.
* calling Idris from Python
    ([exports](https://github.com/ziman/idris-py/blob/master/examples/example.idr#L105),
     [usage](#calling-idris-from-python))

## Observations

* it turns out that using `Text` to represent generated code in the prettyprinter
  ([branches/text](https://github.com/ziman/idris-py/tree/text)) is not that much win
    - strict `Text` seems to be a bit slower than `String`
    - lazy `Text` seems to be about as fast as `String`
    - `String` is the simplest

### Install using Stack

First, the codegen:
```bash
$ stack build
```

Then, the library:
```bash
$ cd lib
$ stack exec idris -- --install python.ipkg
```

Some Python libraries for the example programs:
```bash
$ pip install requests bs4 numpy
```

#### Running the Examples

Compile the example
```bash
$ cd examples/
$ stack exec idris -- example.idr -p python --codegen python -o example.py
```

### Install using Cabal

First, the codegen:
```bash
$ cabal sandbox init --sandbox $IDRIS_PATH/.cabal-sandbox
$ cabal configure && cabal build
```

Then, the library:
```bash
$ cd lib
$ idris --install python.ipkg
```

Some Python libraries for the example programs:
```bash
$ pip install requests bs4 numpy
```

Finally, set up your path appropriately:
```bash
$ export PATH="$PATH:$IDRIS_PATH/.cabal-sandbox/bin/"
```

#### Running the Examples

Compile the example
```bash
$ cd examples/
$ idris example.idr -p python --codegen python -o example.py
```

### Calling Python from Idris
```bash
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

thread A starting
thread B starting
thread A done
thread B done
thread A says 9121
thread B says 9121

And now, let's fail!
  -> (1) everything's fine: [Errno 13] Permission denied: '/root/hello'
  -> (2) everything's fine: [Errno 13] Permission denied: '/root/hello'
```

### Calling Idris from Python

```Python
>>> import example
>>> example.greet()
Hello world!
```
