:- include('naiverev.pl').

run:-
	findall(E, between(1, 1000, E), Es),
	profile(naiverev(Es, _)).