:- dynamic loaded_file/1.
:- dynamic active_file/1.
:- dynamic runtime_dir/1.

% ========================================
% SYSTEM COMMANDS (ENGLISH ONLY)
% ========================================

set_runtime_dir(Dir) :-
    retractall(runtime_dir(_)),
    assertz(runtime_dir(Dir)),
    format('Runtime dir set to: ~w~n', [Dir]).

help :-
    writeln('HELP - Available Commands'),
    writeln('========================================'),
    writeln('FILE MANAGEMENT:'),
    writeln('  help.                    - Show this help'),
    writeln('  load_all.                - Load all .pl files'),
    writeln('  consult(File).           - Load specific file'),
    writeln('  reconsult(File).         - Reload file (clear + load)'),
    writeln('  unload_file(File).       - Unload specific file'),
    writeln('  unload_all.              - Unload all files'),
    writeln('  switch_file(NewFile).    - Switch to new file'),
    writeln('  clear_all.               - Clear all facts from memory'),
    writeln('  list_files.              - List loaded files'),
    writeln('  current_file.            - Show active file'),
    writeln('  list_predicates.         - List predicates in active file'),
    writeln('  list_runtime_files.      - List all .pl files in runtime dir'),
    writeln('EXECUTION:'),
    writeln('  Any standard Prolog query (e.g. fly(X)).'),
    writeln('========================================').

% ========================================
% FILE LOADING 
% ========================================

load_all :-
    runtime_dir(Dir),
    format('Looking for files in: ~w~n', [Dir]),
    % Първо разширяваме директорията
    (   expand_file_name(Dir, [ExpandedDir])
    ->  true
    ;   ExpandedDir = Dir
    ),
    % Създаваме pattern за търсене
    atomic_list_concat([ExpandedDir, '/*.pl'], Pattern),
    format('Search pattern: ~w~n', [Pattern]),
    % Търсим всички .pl файлове
    expand_file_name(Pattern, PlFiles),
    format('Found Prolog files: ~w~n', [PlFiles]),
    (   PlFiles = []
    ->  writeln('No Prolog files found in directory'),
        fail
    ;   forall(
            member(Path, PlFiles),
            (   catch(load_file_with_tracking(Path), Error,
                    format('[ERROR] Failed to load ~w: ~w~n', [Path, Error]))
            )
        ),
        findall(F, loaded_file(F), LoadedFiles),
        format('Successfully loaded ~w files: ~w~n', [length(LoadedFiles), LoadedFiles])
    ).


consult(File) :-
    format('Attempting to consult: ~w~n', [File]),
    (   absolute_file_name(File, AbsPath, [file_type(prolog), access(read)])
    ->  format('Found file at: ~w~n', [AbsPath]),
        load_file_with_tracking(AbsPath)
    ;   format('File not found via absolute_file_name: ~w~n', [File]),
        (   runtime_dir(Dir)
        ->  atomic_list_concat([Dir, '/', File], Path),
            (   exists_file(Path)
            ->  format('Found in runtime dir: ~w~n', [Path]),
                load_file_with_tracking(Path)
            ;   format('[ERROR] File does not exist: ~w~n', [Path]),
                fail
            )
        ;   format('[ERROR] No runtime directory set~n'),
            fail
        )
    ).


load_file_with_tracking(Path) :-
    format('Loading with tracking: ~w~n', [Path]),
    (   catch(prolog:consult(Path), Error,
            (format('[ERROR] Syntax error in ~w: ~w~n', [Path, Error]),
             fail))
    ->  
        file_base_name(Path, FileName),
        retractall(active_file(_)),
        assertz(active_file(FileName)),
        (   loaded_file(FileName) -> true ; assertz(loaded_file(FileName))),
        format('[OK] Loaded ~w (active)~n', [FileName])
    ;   format('[WARNING] consult failed for ~w~n', [Path]),
        fail
    ).

% ========================================
% FILE UNLOADING
% ========================================


unload_file(File) :-
    format('Unloading file: ~w~n', [File]),
    retractall(loaded_file(File)),
    (   active_file(File)
    ->  retractall(active_file(File)),
        format('File ~w unloaded and deactivated~n', [File])
    ;   format('File ~w unloaded (was not active)~n', [File])
    ).


unload_all :-
    findall(F, loaded_file(F), Files),
    (   Files = []
    ->  writeln('No files to unload')
    ;   forall(member(F, Files), unload_file(F)),
        length(Files, Count),
        format('All ~w files unloaded~n', [Count])
    ).


reconsult(File) :-
    format('Reconsulting file: ~w~n', [File]),
    unload_file(File),
    consult(File).


switch_file(NewFile) :-
    (   active_file(CurrentFile)
    ->  format('Switching from ~w to ~w~n', [CurrentFile, NewFile]),
        unload_file(CurrentFile)
    ;   true
    ),
    consult(NewFile).

% ========================================
% KNOWLEDGE BASE MANAGEMENT
% ========================================


clear_all :-
    writeln('Clearing all dynamic predicates...'),
    current_predicate(Pred/Arity),
    predicate_property(Pred, dynamic),
    \+ member(Pred, [loaded_file/1, active_file/1, runtime_dir/1]), % Не изтрива системните
    format('  Retracting: ~w/~w~n', [Pred, Arity]),
    retractall(Pred),
    fail.  % Force backtracking to clear all
clear_all :-
    writeln('All dynamic facts cleared').

% ========================================
% INFORMATION COMMANDS
% ========================================

list_files :-
    findall(F, loaded_file(F), Files),
    (   Files = []
    ->  writeln('No files loaded')
    ;   format('Loaded files (~w):~n', [length(Files)]),
        forall(member(F, Files), format('  ~w~n', [F]))
    ).

current_file :-
    (   active_file(F)
    ->  format('Active file: ~w~n', [F])
    ;   writeln('No active file')
    ).


list_predicates :-
    (   active_file(File)
    ->  format('Dynamic predicates in ~w:~n', [File]),
        current_predicate(Pred/Arity),
        predicate_property(Pred, dynamic),
        format('  ~w/~w~n', [Pred, Arity]),
        fail  % backtrack for all predicates
    ;   writeln('No active file to list predicates from')
    ),
    writeln('(end of list)').

% Utility predicates (not in help - for internal use)
get_full_path(Filename, FullPath) :-
    runtime_dir(Dir),
    atomic_list_concat([Dir, '/', Filename], FullPath).

file_in_runtime(Filename) :-
    get_full_path(Filename, Path),
    exists_file(Path).

list_runtime_files :-
    runtime_dir(Dir),
    (   expand_file_name(Dir, [ExpandedDir])
    ->  true
    ;   ExpandedDir = Dir
    ),
    atomic_list_concat([ExpandedDir, '/*.pl'], Pattern),
    expand_file_name(Pattern, Files),
    format('Files in runtime directory (~w):~n', [Dir]),
    (   Files = []
    ->  writeln('  No .pl files found')
    ;   forall(member(F, Files), 
            (   file_base_name(F, Name),
                format('  ~w~n', [Name])
            ))
    ).

% ========================================
% INITIALIZATION
% ========================================

:- initialization((
    writeln('========================================'),
    writeln('PROLOG FILE MANAGEMENT SYSTEM'),
    writeln('Version 1.1 - Fixed load_all issue'),
    writeln('Type "help." to see available commands.'),
    writeln('========================================'),
    % Set default runtime directory
    (   \+ runtime_dir(_)
    ->  working_directory(Dir, Dir),
        set_runtime_dir(Dir)
    ;   true
    )
)).
