
true.

eq(?X, ?X).


number(z).
number(s(?N)) :- number(?N).

add(z, ?N, ?N).
add(s(?N), ?M, s(?R)) :- add(?N, ?M, ?R).


list(nil).
list(cons(?X, ?Xs)) :- list(?Xs).

member(?X, cons(?X, ?Xs)).
member(?X, cons(?Y, ?Xs)) :- member(?X, ?Xs).

append(nil, ?Ys, ?Ys).
append(cons(?X, ?Xs), ?Ys, cons(?X, ?Zs)) :- append(?Xs, ?Ys, ?Zs).
