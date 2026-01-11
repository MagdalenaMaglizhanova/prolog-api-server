:- encoding(utf8).
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
% FILE UNLOADING (ПОПРАВЕНИ ВЕРСИИ)
% ========================================

% Изчиства всички фактове от определен файл - ПРОСТА ВЕРСИЯ
unload_file(File) :-
    format('Unloading file: ~w~n', [File]),
    % Първо премахваме файла от списъка със заредени
    retractall(loaded_file(File)),
    
    % Изчистваме активния файл ако това е той
    (   active_file(File)
    ->  retractall(active_file(File)),
        format('File ~w unloaded and deactivated~n', [File])
    ;   format('File ~w unloaded (was not active)~n', [File])
    ),
    
    % Изчистваме кеша на зареждането
    abolish_all_tables,
    format('Cleared all tables~n').

% Изчиства ВСИЧКИ заредени файлове
unload_all :-
    findall(F, loaded_file(F), Files),
    (   Files = []
    ->  writeln('No files to unload')
    ;   forall(member(F, Files), 
            (retractall(loaded_file(F)),
             format('Unloaded: ~w~n', [F]))
        ),
        retractall(active_file(_)),
        abolish_all_tables,
        length(Files, Count),
        format('All ~w files unloaded~n', [Count])
    ).

% Презарежда файл (изчиства стари факти и зарежда нови) - ФИКСИРАНА ВЕРСИЯ
reconsult_file(File) :-
    format('Reconsulting file: ~w~n', [File]),
    runtime_dir(Dir),
    atomic_list_concat([Dir, '/', File], Path),
    
    % Проверка дали файлът съществува
    (   exists_file(Path)
    ->  % Изчистване от нашия списък
        (   loaded_file(File) -> retractall(loaded_file(File)) ; true),
        (   active_file(File) -> retractall(active_file(File)) ; true),
        
        % Изчистване на кеша
        abolish_all_tables,
        
        % Зареждане на новия файл
        (   catch(consult(Path), Error,
                (format('[ERROR] Failed to reconsult ~w: ~w~n', [File, Error]),
                 fail))
        ->  assertz(loaded_file(File)),
            assertz(active_file(File)),
            format('[OK] Reconsulted ~w~n', [File])
        ;   format('[WARNING] Failed to reconsult ~w~n', [File]),
            fail
        )
    ;   format('[ERROR] File does not exist: ~w~n', [Path]),
        fail
    ).

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

% Изчиства всички динамични предикати от активния файл
clear_all_facts :-
    writeln('Clearing all dynamic facts...'),
    (   active_file(File)
    ->  format('Active file: ~w~n', [File]),
        % Намираме всички динамични предикати и ги изчистваме
        findall(Pred/Arity, (current_predicate(Pred/Arity), predicate_property(Pred, dynamic)), Predicates),
        (   Predicates = []
        ->  writeln('No dynamic predicates found')
        ;   forall(member(Pred/Arity, Predicates), 
                (format('  Retracting: ~w/~w~n', [Pred, Arity]),
                 retractall(Pred))
              ),
            format('Cleared ~w dynamic predicates~n', [length(Predicates)])
        ),
        abolish_all_tables
    ;   writeln('No active file selected')
    ).

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
    ->  runtime_dir(Dir),
        atomic_list_concat([Dir, '/', File], Path),
        format('Dynamic predicates in ~w (~w):~n', [File, Path]),
        findall(Pred/Arity, (current_predicate(Pred/Arity), predicate_property(Pred, dynamic)), Predicates),
        (   Predicates = []
        ->  writeln('  No dynamic predicates')
        ;   forall(member(Pred/Arity, Predicates), format('  ~w/~w~n', [Pred, Arity]))
        )
    ;   writeln('No active file to list predicates from')
    ).
