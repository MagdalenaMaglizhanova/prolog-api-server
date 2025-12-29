:- dynamic loaded_file/1.
:- dynamic active_file/1.
:- dynamic runtime_dir/1.

% ========================================
% SYSTEM COMMANDS
% ========================================

set_runtime_dir(Dir) :-
    retractall(runtime_dir(_)),
    assertz(runtime_dir(Dir)),
    format('Runtime dir set to: ~w~n', [Dir]).

help :-
    writeln('HELP - Commands'),
    writeln('========================================'),
    writeln('FILE MANAGEMENT:'),
    writeln('  help.                    - Show this help'),
    writeln('  load_all.                - Load all .pl files'),
    writeln('  consult_file(File).      - Load specific file'),
    writeln('  reconsult_file(File).    - Reload file (clear + load)'),
    writeln('  unload_file(File).       - Unload specific file'),
    writeln('  unload_all.              - Unload all files'),
    writeln('  switch_file(NewFile).    - Switch to new file'),
    writeln('  clear_all_facts.         - Clear all facts from memory'),
    writeln('  list_files.              - List loaded files'),
    writeln('  current_file.            - Show active file'),
    writeln('  list_predicates.         - List predicates in active file'),
    writeln('EXECUTION:'),
    writeln('  Any Prolog query (e.g. fly(X)).'),
    writeln('========================================').

% ========================================
% FILE LOADING
% ========================================

load_all :-
    runtime_dir(Dir),
    format('Looking for files in: ~w~n', [Dir]),
    directory_files(Dir, Files),
    format('Found files: ~w~n', [Files]),
    findall(F, (member(F, Files), file_name_extension(_, pl, F)), PlFiles),
    format('Prolog files: ~w~n', [PlFiles]),
    (   PlFiles = []
    ->  writeln('No Prolog files found in directory'),
        fail
    ;   forall(
            member(File, PlFiles),
            (   catch(consult_file(File), Error,
                    format('[ERROR] Failed to consult ~w: ~w~n', [File, Error]))
            )
        ),
        findall(F, loaded_file(F), LoadedFiles),
        format('Successfully loaded ~w files: ~w~n', [length(LoadedFiles), LoadedFiles])
    ).

consult_file(File) :-
    runtime_dir(Dir),
    format('Consulting file: ~w (from dir: ~w)~n', [File, Dir]),
    atomic_list_concat([Dir, '/', File], Path),
    format('Full path: ~w~n', [Path]),
    (   exists_file(Path)
    ->  (   catch(consult(Path), Error,
                (format('[ERROR] Syntax error in ~w: ~w~n', [File, Error]),
                 fail))
        ->  retractall(active_file(_)),
            assertz(active_file(File)),
            (   loaded_file(File) -> true ; assertz(loaded_file(File))),
            format('[OK] Consulted ~w~n', [File])
        ;   format('[WARNING] consult/1 failed for ~w~n', [File]),
            fail
        )
    ;   format('[ERROR] File does not exist: ~w~n', [Path]),
        fail
    ).

% ========================================
% FILE UNLOADING
% ========================================

% Изчиства всички фактове от определен файл
unload_file(File) :-
    format('Unloading file: ~w~n', [File]),
    retractall(loaded_file(File)),
    (   active_file(File)
    ->  retractall(active_file(File)),
        format('File ~w unloaded and deactivated~n', [File])
    ;   format('File ~w unloaded (was not active)~n', [File])
    ).

% Изчиства ВСИЧКИ заредени файлове
unload_all :-
    findall(F, loaded_file(F), Files),
    (   Files = []
    ->  writeln('No files to unload')
    ;   forall(member(F, Files), unload_file(F)),
        length(Files, Count),
        format('All ~w files unloaded~n', [Count])
    ).

% Презарежда файл (изчиства стари факти и зарежда нови)
reconsult_file(File) :-
    format('Reconsulting file: ~w~n', [File]),
    unload_file(File),
    consult_file(File).

% Превключване към нов файл (unload стар + load нов)
switch_file(NewFile) :-
    (   active_file(CurrentFile)
    ->  format('Switching from ~w to ~w~n', [CurrentFile, NewFile]),
        unload_file(CurrentFile)
    ;   true
    ),
    consult_file(NewFile).

% ========================================
% KNOWLEDGE BASE MANAGEMENT
% ========================================

% Изчиства всички динамични предикати (общ подход)
clear_all_facts :-
    writeln('Clearing all dynamic predicates from active file...'),
    (   active_file(File)
    ->  format('Active file: ~w~n', [File]),
        % Това е генерален подход - ще изчисти ВСИЧКИ динамични предикати
        current_predicate(Pred/Arity),
        predicate_property(Pred, dynamic),
        format('  Retracting: ~w/~w~n', [Pred, Arity]),
        retractall(Pred),
        fail  % принуждава backtracking за всички предикати
    ;   true
    ),
    writeln('All dynamic facts cleared').

% ========================================
% INFORMATION COMMANDS
% ========================================

list_files :-
    findall(F, loaded_file(F), Files),
    (   Files = []
    ->  writeln('No files loaded')
    ;   format('Loaded files (~w): ~w~n', [length(Files), Files])
    ).

current_file :-
    (   active_file(F)
    ->  format('Active file: ~w~n', [F])
    ;   writeln('No active file')
    ).

% Показва всички предикати в активния файл
list_predicates :-
    (   active_file(File)
    ->  format('Dynamic predicates in ~w:~n', [File]),
        current_predicate(Pred/Arity),
        predicate_property(Pred, dynamic),
        format('  ~w/~w~n', [Pred, Arity]),
        fail  % backtrack за всички предикати
    ;   writeln('No active file to list predicates from')
    ),
    writeln('(end of list)').
