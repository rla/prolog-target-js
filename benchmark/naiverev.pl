append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]):-
	append(Xs, Ys, Zs).

naiverev([], []).
naiverev([H|T], R) :-
	naiverev(T, RevT),
	append(RevT, [H], R).