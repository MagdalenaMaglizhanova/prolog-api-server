% ================================================
% Manager – управлява зареждането на бази знания
% ================================================

:- dynamic active_kb/1.

% Зарежда нова база знания
load_kb(File) :-
    % ако вече има активна база → разтоварваме я
    active_kb(Current),
    Current \= File,
    unload_kb,
    retractall(active_kb(_)),
    consult(File),
    asserta(active_kb(File)),
    format("Loaded knowledge base: ~w~n", [File]),
    !.

% Ако няма активна база → директно зареждаме
load_kb(File) :-
    \+ active_kb(_),
    consult(File),
    asserta(active_kb(File)),
    format("Loaded knowledge base: ~w~n", [File]).

% Разтоварване на активната база
unload_kb :-
    active_kb(File),
    atom_concat(ModuleName, '.pl', File),
    unload_file(ModuleName),
    retractall(active_kb(_)),
    format("Unloaded knowledge base: ~w~n", [File]),
    !.

unload_kb :-
    \+ active_kb(_),
    format("No active knowledge base to unload.~n").
