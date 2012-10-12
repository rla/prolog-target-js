:- use_module(input).
:- use_module(output).
:- use_module(transform).

%% transpile(+FileIn) is det.
%
% Toplevel predicate to transpile the given Prolog file.
	
transpile(FileIn):-
	file_name_extension(Base, _, FileIn),
	atomic_concat(Base, '.js', FileOut),
	input:read_file(FileIn, PredsIn),
	maplist(transform:pred_calls, PredsIn, PredsOut),
	output:write_file(FileOut, PredsOut).

% Helper for running from command line.
	
transpile:-
	current_prolog_flag(argv, Argv),
	append(_, [File], Argv),
	transpile(File).