
# TPLPL

TPLPL is a pure logic programming language. The only data type supported is
a structure (e.g. foo(...)), although parenthesis may by omitted on zero arity
structures (`foo = foo()`), making them act like atoms. Only horn clauses and
unification are supported, cuts and negation by failure are not. Note that
variables start with a `?`, and are not indicated by being capitalized (as in
Prolog).

Future goals include:
1. Making the search algorithm breadth first. Although this is very inefficient,
   it does go along with the theme of a pure logic language.
2. Add readline support. Currently stack complains if it is used as a
   dependency.

## EBNF Grammar

Comments begin with # and extend to the end of the line.

```
var = '?' , ID ;
term = ID , [ '(' , [ value , { ',' , value } , ')' ] ;

value = var | term ;

and = value , { ',' , value } ;

query = and , '.' ;

rule = term , [ ':-' , and ] , '.' ;

ID = ? [a-zA-Z][_a-zA-Z0-9]* ? ;
```

## Examples

See `lib/*` for examples. Below is an example consultation session.

```
> stack build
...
> stack exec tplpl lib/core.tplpl
:- true.
true

:- eq(a, a).
true

:- eq(A, a).
false
:- eq(?X, a).
?X = a

:- list(?L).
?L = nil
;
?L = cons(?X_1, nil)
;
?L = cons(?X_1, cons(?X_2, nil))
;
?L = cons(?X_1, cons(?X_2, cons(?X_3, nil)))

:- add(?N, ?M, s(s(s(z)))).
?M = s(s(s(z))), ?N = z
;
?M = s(s(z)), ?N = s(z)
;
?M = s(z), ?N = s(s(z))
;
?M = z, ?N = s(s(s(z)))
;
false
:-
```
