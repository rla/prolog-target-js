% Head is atom, no arguments.

exargs(Head, Head, []):-
	atom(Head).

% Head is compound term.
	
exargs(Head1, Head2, Calls):-
	compound(Head1),
	Head1 =.. [Name|Args1],
	newargs(Args1, Args2, Calls),
	Head2 =.. [Name|Args2].

% Extracts arguments via unification.

newargs([], [], []).
newargs([Arg1|Args1], [Var|Args2], Calls1):-
	Calls1 = [Unification|Calls2],
	Unification = '='(Var, Arg1),
	newargs(Args1, Args2, Calls2).

% Turns body into list of predicate calls.
	
calls(Body, Calls1, Calls2):-
	Body = ','(Left, Right),
	calls(Left, Calls1, Calls3),
	calls(Right, Calls3, Calls2).
	
calls(Body, Calls1, Calls2):-
	atom(Body),
	append(Calls1, [Body], Calls2).
	
calls(Body, Calls1, Calls2):-
	functor(Body, Name, Arity),
	Name \= ',',
	Arity > 0,
	append(Calls1, [Body], Calls2).
	
listify(Head1 :- Body, Head2-Calls2):-
	exargs(Head1, Head2, Calls1),
	calls(Body, Calls1, Calls2).
	
listify(:- Head1, Head2-Calls):-
	exargs(Head1, Head2, Calls).
	
member_clauses(List):-
	List = [
		':-'(member(X, [X|_])),
		':-'(member(Y, [_|Ys]), member(Y, Ys))
	].
	
append_clauses(List):-
	List = [
		':-'(append([], Ys1, Ys1)),
		':-'(append([X|Xs], Ys, [X|Zs]), append(Xs, Ys, Zs))
	].
	
test:-
	append_clauses(Clauses),
	maplist(listify, Clauses, Listified),
	out_predicate(append/3, Listified).
	
out_predicate(Functor, Clauses):-
	Functor = Name/Arity,
	length(Clauses, NumChoices),
	length(Args, Arity),
	numbervars(Args, 0, _),
	maplist(varname, Args, ArgNames),
	atomic_list_concat(ArgNames, ', ', ArgString),
	format('function ~w_~w(~w, stack, cb) {', [Name, Arity, ArgString]),
	out_clauses(Clauses, 0, NumChoices),
	format('return ~w_~w_0(~w, stack, cb);', [Name, Arity, ArgString]),
	format('}').
	
out_clauses([], _, _).	
out_clauses([Clause|Clauses], Nth, NumChoices):-
	(Nth < NumChoices - 1 -> Choice = yes ; Choice = no),
	out_clause(Clause, Nth, Choice),
	NthNext is Nth + 1,
	out_clauses(Clauses, NthNext, NumChoices).

out_clause(Head-Calls, Nth, Choice):-
	Head =.. [Name|Args],
	length(Args, Arity),
	term_variables(Head-Calls, Vars),
	numbervars(Head-Calls, 0, _),
	maplist(varname, Args, ArgsNames),
	atomic_list_concat(ArgsNames, ', ', ArgString),
	format('function ~w_~w_~w(~w, stack, cb) {', [Name, Arity, Nth, ArgString]),
	maplist(varname, Vars, VarNames),
	findall(InitLocal, (
		member(LocalName, VarNames),
		\+ member(LocalName, ArgsNames),
		init_local(LocalName, InitLocal)
	), LocalNames),
	atomic_list_concat(LocalNames, ', ', LocalString),
	format('var ~w;', [LocalString]),
	NthNext is Nth + 1,
	push_choice(Choice, Name, Arity, ArgString, NthNext),
	out_calls(Calls),
	format('}').
	
init_local(LocalName, Atom):-
	format(atom(Atom), '~w = new Var()', [LocalName]).
	
push_choice(yes, Name, Arity, ArgString, NthNext):-
	Params = [Name, Arity, NthNext, ArgString],
	format('stack.push(function() { return ~w_~w_~w(~w, stack, cb); });', Params).
	
push_choice(no, _, _, _, _).
	
out_calls([]):-
	format('return cb;').
	
out_calls([Call|Calls]):-
	Call =.. [Name1|Args1],
	maplist(varname, Args1, Args2),
	length(Args1, Arity),
	atomic_list_concat(Args2, ', ', Args3),
	(Name1 = '=' ->
		Name2 = 'unify',
		RealArity = ''
	;
		Name2 = Name1,
		atomic_concat('_', Arity, RealArity)
	),
	format('return ~w~w(~w, stack, function() {', [Name2, RealArity, Args3]),
	out_calls(Calls),
	format('});').
	
varname(Term, Name):-
	var_number(Term, Num), !,
	atomic_concat('$', Num, Name).

varname(Term1, Term2):-
	Term1 =.. [Name|Args1],
	(Args1 = [] ->
		format(atom(Term2), '\'~w\'', [Name])
		;
		maplist(varname, Args1, Args2),
		atomic_list_concat(Args2, ', ', Args3),
		format(atom(Term2), 'new Struct(\'~w\', ~w)', [Name, Args3])
	).