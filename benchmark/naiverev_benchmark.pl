:- include('naiverev.pl').

run:-
	findall(E, between(1, 1000, E), Es),
	time(naiverev(Es, _)).