:- module(output, [
	write_file/2
]).

%% write_file(+File, +Preds) is det.
%
% Writes the set of predicates into the
% given file.

write_file(File, Preds):-
	open(File, write, Fd),
	format(Fd, 'var runtime = require("../runtime");', []),
	format(Fd, 'var Var = runtime.Var;', []),
	format(Fd, 'var Struct = runtime.Struct;', []),
	format(Fd, 'var _u = runtime.unification;', []),
	format(Fd, 'var _b = runtime.backtrack;', []),
	format(Fd, 'var _is = runtime.is;', []),
	format(Fd, 'var _inequal = runtime.inequal;', []),
	format(Fd, 'var _less = runtime.less;', []),
	format(Fd, 'var _cut = runtime.cut;', []),
	format(Fd, 'var _s = runtime.toString;', []),
	maplist(write_pred(Fd), Preds), !,
	close(Fd).

%% write_pred(+Fd, +Pred) is det.
%
% Outputs one predicate. A predicate is compiled
% into a function that encloses its clauses.
% CommonJS export is also added.
	
write_pred(Fd, Name/Arity-Clauses):-
	length(Clauses, NumChoices),
	atomic_list_concat([Name, Arity], '_', Fun),
	write_clauses(Fd, Clauses, 0, NumChoices),
	format(Fd, 'exports.~w = ~w_0;', [Fun, Fun]).

%% write_clauses(Fd, Clauses, Nth, NumChoices) is det.
%
% Writes predicate clauses. When Nth < NumChoices - 1
% then the clause must add the predicate's next alternative
% clause as the choice. 
	
write_clauses(_, [], _, _).
	
write_clauses(Fd, [Clause|Clauses], Nth, NumChoices):-
	(Nth < NumChoices - 1 -> Choice = yes ; Choice = no), !,
	write_clause(Fd, Clause, Nth, Choice),
	NthNext is Nth + 1,
	write_clauses(Fd, Clauses, NthNext, NumChoices).

%% write_clause(Fd, Head-Calls, Nth, Choice) is det.
%
% Transpiles single clause. Choice is 'yes' or 'no'
% and sets whether the clause will push choice point or not.
	
write_clause(Fd, Head-Calls, Nth, Choice):-
	functor(Head, Name, Arity),
	clause_variables(Head-Calls, ArgNames, LocalNames),
	atomic_list_concat(ArgNames, ', ', ArgString),
	atomic_list_concat([Name, Arity, Nth], '_', Fun),
	format(Fd, 'function ~w(~w, s, cb) {', [Fun, ArgString]),
	write_locals(Fd, LocalNames),
	format(Fd, 'var _i = s.length;', []),
	NthNext is Nth + 1,
	push_choice(Fd, Choice, Name, Arity, ArgString, NthNext),
	write_calls(Fd, Calls),
	format(Fd, '}', []).

%% write_calls(+Fd, +Calls) is det.
%
% Writes predicate calls into JavaScript
% constructs.

write_calls(Fd, []):-
	format(Fd, 'return cb;', []).
	
write_calls(Fd, [Call|Calls]):-
	Call =.. [Name|Args],
	length(Args, Arity),
	maplist(termname, Args, ArgTerms),		
	atomic_list_concat(ArgTerms, ', ', ArgTermString),
	write_call(Fd, Name, Arity, ArgTermString, Calls).
	
write_call(Fd, '=', 2, ArgTermString, Calls):- !,
	format(Fd, 'if (!_u(s, ~w)) return _b(s);', [ArgTermString]),
	write_calls(Fd, Calls).
	
write_call(Fd, 'is', 2, ArgTermString, Calls):- !,
	format(Fd, 'return _is(~w, s, function() {', [ArgTermString]),
	write_calls(Fd, Calls),
	format(Fd, '});', []).
	
write_call(Fd, '=\\=', 2, ArgTermString, Calls):- !,
	format(Fd, 'return _inequal(~w, s, function() {', [ArgTermString]),
	write_calls(Fd, Calls),
	format(Fd, '});', []).
	
write_call(Fd, '<', 2, ArgTermString, Calls):- !,
	format(Fd, 'return _less(~w, s, function() {', [ArgTermString]),
	write_calls(Fd, Calls),
	format(Fd, '});', []).
	
write_call(Fd, '!', 0, _, Calls):- !,
	format(Fd, '_cut(s, _i);', []),
	write_calls(Fd, Calls).
	
write_call(Fd, 'writeln', 1, ArgTermString, Calls):- !,
	format(Fd, 'console.log(_s(~w));', [ArgTermString]),
	write_calls(Fd, Calls).
	
write_call(Fd, Name, Arity, ArgTermString, []):- !,
	atomic_list_concat([Name, Arity], '_', Fun),
	format(Fd, 'return ~w_0(~w, s, cb);', [Fun, ArgTermString]).
	
write_call(Fd, Name, Arity, ArgTermString, Calls):-
	atomic_list_concat([Name, Arity], '_', Fun),
	format(Fd, 'return ~w_0(~w, s, function() {', [Fun, ArgTermString]),
	write_calls(Fd, Calls),
	format(Fd, '});', []).

%% push_choice(+Fd, +Choice, +Name, +Arity, +ArgString, +NthNext) is det.
%
% Pushes choicepoint into the stack when needed (the clause
% is not the last one for the predicate). The choice is
% just a function that returns the next clause.

push_choice(_, no, _, _, _, _).
	
push_choice(Fd, yes, Name, Arity, ArgString, NthNext):-
	Params = [Name, Arity, NthNext, ArgString],
	format(Fd, 's.push(function() { return ~w_~w_~w(~w, s, cb); });', Params).

%% write_locals(+Fd, +LocalNames) is det.
%
% Writes statement for local variable initialization.

write_locals(_, []):- !.
	
write_locals(Fd, LocalNames):-
	maplist(init_local, LocalNames, LocalInits),
	atomic_list_concat(LocalInits, '; ', LocalString),
	format(Fd, '~w;', [LocalString]).

%% init_local(+LocalName, -Init) is det.
%
% Turns local variable into initialization
% expression.
	
init_local(LocalName, Init):-
	format(atom(Init), 'var ~w = new Var()', [LocalName]).

%% clause_variables(+Head-Calls, -ArgNames, -LocalNames) is det.
%
% Extracts variables from the clause and turn them
% into JavaScript variables. The naming scheme must
% match predargs/2.
	
clause_variables(Head-Calls, ArgNames, LocalNames):-
	term_variables(Head-Calls, Vars),
	numbervars(Head-Calls, 0, _),
	clause_arguments(Head, ArgNames),
	maplist(varname, Vars, AllNames),
	findall(LocalName, (
		member(LocalName, AllNames),
		\+ member(LocalName, ArgNames)
	), LocalNames).

%% clause_arguments(+Head, -ArgNames) is det.
%
% Turns clause arguments into JavaScript variables.
	
clause_arguments(Head, ArgNames):-
	Head =.. [_|Args],
	maplist(varname, Args, ArgNames).
	
%% termname(+Term, -Atom) is det.
%
% Turns Prolog term into JavaScript object.

termname(Term, Atom):-
	var_number(Term, _), !,
	varname(Term, Atom).
	
termname(Term, Term):-
	number(Term), !.
	
termname([], '\'[]\''):- !.
	
termname(Term, Atom):-
	atom(Term), !,
	format(atom(Atom), '~q', [Term]).
	
termname(Term, Atom):-
	Term =.. [Name|Args],
	maplist(termname, Args, ArgTerms),
	atomic_list_concat(ArgTerms, ', ', ArgTermString),
	format(atom(Atom), 'new Struct(\'~w\', ~w)', [Name, ArgTermString]).

%% Gives variable name for JavaScript.
%
% Assumes that numbervars/3 has been called
% on the term containing the variable.

varname(Var):-
	nonvar(Var), !,
	throw(error(varname_for_nonvar(Var))).
	
varname(Var, Name):-
	var_number(Var, Num), !,
	atomic_concat('$', Num, Name).