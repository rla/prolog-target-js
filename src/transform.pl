:- module(transform, [
	exargs/3,
	pred_calls/2
]).

%% exargs(+Head, -NewHead, -Calls) is det.
%
% Extracts non-variable arguments from the
% predicate head and replaces them with unification calls.
% Throws error when the head cannot be handled.
% Example call: exargs(pred([X|Xs]), pred(V1), [V1=[X|Xs]]).

exargs(Head, Head, []):-
	atom(Head), !.
	
exargs(HeadIn, HeadOut, Calls):-
	compound(HeadIn), !,
	HeadIn =.. [Name|ArgsIn],
	newargs(ArgsIn, ArgsOut, Calls),
	HeadOut =.. [Name|ArgsOut].
	
exargs(Head, _, _):-
	throw(error(invalid_head(Head))).

%% newargs(+Terms, -Vars, -Calls) is det.
%
% Takes list of terms and turns them into
% unifications Var=Term where Var is a fresh variable.

newargs([], [], []).

newargs([Term|Terms], [Var|Vars], CallsOut):-
	CallsOut = [Unification|CallsIn],
	Unification = '='(Var, Term),
	newargs(Terms, Vars, CallsIn).

%% body_calls(+Body, +CallsIn, -CallsOut) is det.
%
% Turns predicate body into a list of predicate calls.
% Throws error when Body contains non-supported predicate call.

body_calls(Body, CallsIn, CallsOut):-
	Body = ','(Left, Right), !,
	body_calls(Left, CallsIn, CallsLeft),
	body_calls(Right, CallsLeft, CallsOut).
	
body_calls(Body, CallsIn, CallsOut):-
	atom(Body), !,
	append(CallsIn, [Body], CallsOut).
	
body_calls(Body, _, _):-
	functor(Body, Name, Arity),
	not_supported_call(Name/Arity),
	throw(error(not_supported_call(Name/Arity))).	
	
body_calls(Body, CallsIn, CallsOut):-
	append(CallsIn, [Body], CallsOut).
	
%% clause_calls(+Clause, -Head-Calls) is det.
%
% Turns predicate clause (rule) into a list
% of calls. 
	
clause_calls(HeadIn :- Body, HeadOut-CallsOut):- !,
	exargs(HeadIn, HeadOut, CallsHead),
	body_calls(Body, CallsHead, CallsOut).
	
clause_calls(HeadIn, HeadOut-Calls):-
	exargs(HeadIn, HeadOut, Calls).

%% pred_calls(Functor-Clauses, Functor-ClauseCalls) is det.
%
% Applies clause_calls/2 for each clause of the predicate. 
	
pred_calls(Functor-Clauses, Functor-ClauseCalls):-
	maplist(clause_calls, Clauses, ClauseCalls).
	
%% not_supported_call(+Functor) is det.
%
% Some calls are currently not supported.
% These should be listed here.

not_supported_call(Functor):-
	memberchk(Functor, [
		'!'/0,
		';'/2,
		'->'/2,
		'\='/2,
		'call'/_,
		'not'/1,
		'\+'/1
	]).