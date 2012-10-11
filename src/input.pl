:- module(input, [
	read_file/2
]).

%% read_file(+Name, -Preds) is det.
%
% Reads all clauses from the input file
% and sorts them into predicates.

read_file(Name, Preds):-
	open(Name, read, Fd),
	read_clauses(Fd, Clauses),
	collect_preds(Clauses, [], Preds),
	close(Fd).
	
read_clauses(Fd, Clauses):-
	read_clauses(Fd, [], ClausesReverse),
	reverse(ClausesReverse, Clauses).
	
read_clauses(Fd, Clauses, Clauses):-
	at_end_of_stream(Fd), !.
	
read_clauses(Fd, ClausesIn, ClausesOut):-
	read_term(Fd, Clause, []),
	read_clauses(Fd, [Clause|ClausesIn], ClausesOut).
	
collect_preds([], Preds, Preds).
	
collect_preds([Clause|Clauses], PredsIn, PredsOut):-
	(Clause = ':-'(Head, _) ; Clause = Head), !,
	functor(Head, Name, Arity),
	Functor = Name/Arity,
	append_to_multimap(Functor, Clause, PredsIn, PredsTmp),
	collect_preds(Clauses, PredsTmp, PredsOut).
	
append_to_multimap(Key, Value, MapIn, MapOut):-
	select(Key-ValuesIn, MapIn, MapTmp), !,
	append(ValuesIn, [Value], ValuesOut),
	MapOut = [Key-ValuesOut|MapTmp].
	
append_to_multimap(Key, Value, MapIn, MapOut):-
	MapOut = [Key-[Value]|MapIn].