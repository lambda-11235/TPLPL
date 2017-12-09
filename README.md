
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

```
true.

eq(?X, ?X).

number(z).
number(s(?N)) :- number(?N).

add(z, ?N, ?N).
add(s(?N), ?M, s(?R)) :- add(?N, ?M, ?R).


# In REPL
:- true.
true

:- eq(foo, foo()).
true

:- add(?N, ?M, ?R).
?M = ?R, ?N = z
;
?M = ?R_1, ?N = s(z), ?R = s(?R_1)
;
?M = ?R_2, ?N = s(s(z)), ?R = s(s(?R_2))
;
?M = ?R_3, ?N = s(s(s(z))), ?R = s(s(s(?R_3)))
;
?M = ?R_4, ?N = s(s(s(s(z)))), ?R = s(s(s(s(?R_4))))

:-
```
