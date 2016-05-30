# System T

This is an implementation of the language __T__ from
[PFPL](http://www.cs.cmu.edu/~rwh/pfpl.html). It uses the CMU ABT library (the
one used in the [15-312 course](https://www.cs.cmu.edu/~rwh/courses/ppl/) at CMU).

The aim is to closely follow the description in PFPL. All typing rules can be
found in `src/statics/typechecker.sml` and all dynamics rules in
`src/dynamics/dynamics.sml` somewhere close to the corresponding lines of code.

## Building and running

You need `smlnj` to build along with `ml-lex` and `ml-yacc`. Given that you have
these installed, just `make` in the project directory. If all goes well you
should find the executables `bin/system-t` and `bin/repl`. Here is an example
repl session:

```
$ ./bin/repl
> (\(x:nat) s(x))(s(s(z)))
Step: s(s(s(z)))
s(s(s(z))) : nat
> \ (x:nat) s(s(x))
lam{nat}(x@2.s(s(x@2))) : arr(nat; nat)
> (\ (f : nat -> nat) \ (x : nat) f(x))(\ (x:nat) s(x))(s(z))
Step: ap(lam{nat}(x@6.ap(lam{nat}(x@7.s(x@7)); x@6)); s(z))
Step: ap(lam{nat}(x@10.s(x@10)); s(z))
Step: s(s(z))
s(s(z)) : nat
> \ (f:nat->nat) s(f)
Type error: s can be only applied to a nat.
> rec s(s(s(z))) {z => z | s(u) with v => s(s(s(v)))}
Step: s(s(s(rec(z; x@7.x@8.s(s(s(x@8))); s(s(z))))))
Step: s(s(s(s(s(s(rec(z; x@11.x@12.s(s(s(x@12))); s(z))))))))
Step: s(s(s(s(s(s(s(s(s(rec(z; x@15.x@16.s(s(s(x@16))); z))))))))))
Step: s(s(s(s(s(s(s(s(s(z)))))))))
s(s(s(s(s(s(s(s(s(z))))))))) : nat
```

Using the `bin/system-t` executable you can execute the __T__ program in a file.
There is a sample program in `sample.t`. Executing it yields the following
result.

```
$ ./bin/system-t sample.t
s(s(s(s(z))))
```

Note that instead of ↪ we use =>.

I have followed the homework solutions to CMU's 15-312. The ABT library is
almost directly taken from the solutions that can be found on the
[course website](https://www.cs.cmu.edu/~rwh/courses/ppl/) as well as some of
the other files (e.g. `src/parser/parsestates.sml`).
