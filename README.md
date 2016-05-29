# System T

This is an implementation of the language __T__ from
[PFPL](http://www.cs.cmu.edu/~rwh/pfpl.html). It uses the CMU ABT library (the
one used in the [15-312 course](https://www.cs.cmu.edu/~rwh/courses/ppl/) at CMU).

The aim is to closely follow the description in PFPL. All typing rules can be
found in `src/statics/typechecker.sml` and all dynamics rules in
`src/dynamics/dynamics.sml` somewhere close to the corresponding lines of code.

## Building

You `smlnj` to build along with `ml-lex` and `ml-yacc`. Given that you have
these installed, just `make` in the project directory. If all goes well you
should find an executable `bin/repl`. Here is an example repl session:

```
> (\(x:nat) s(x))(s(s(z)))
Step: s(s(s(z)))
s(s(s(z))) : nat
> rec s(s(s(z))) {z => z | s(u) with v => s(s(s(v)))}
Step: s(s(s(rec(z; x@7.x@8.s(s(s(x@8))); s(s(z))))))
Step: s(s(s(s(s(s(rec(z; x@11.x@12.s(s(s(x@12))); s(z))))))))
Step: s(s(s(s(s(s(s(s(s(rec(z; x@15.x@16.s(s(s(x@16))); z))))))))))
Step: s(s(s(s(s(s(s(s(s(z)))))))))
s(s(s(s(s(s(s(s(s(z))))))))) : nat
```

Note that instead of ↪ we use =>.
