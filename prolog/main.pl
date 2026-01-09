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
    writeln('STANDARD PROLOG COMMANDS:'),
    writeln('  [File].                  - Consult file (standard Prolog)'),
    writeln('  consult(File).           - Consult file (standard Prolog)'),
    writeln('  reconsult(File).         - Reconsult file (standard Prolog)'),
    writeln('SYSTEM EXTENSIONS:'),
    writeln('  help.                    - Show this help'),
    writeln('  load_all.                - Load all .pl files'),
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
% FILE LOADING (STANDARD PROLOG COMPATIBILITY)
% ========================================

% Стандартен Prolog consult - презаписваме
user:consult(File) :-
    (   atom_concat(_, '.pl', File) -> true
    ;   atom_concat(File, '.pl', _)
    ),
    !,
    format('Consulting: ~w~n', [File]),
    consult_file_internal(File).

% Зареждане чрез [filename] синтаксис
user:(File) :-
    atom(File),
    (   atom_concat(_, '.pl', File) -> true
    ;   atom_concat(File, '.pl', _)
    ),
    !,
    format('Consulting: ~w~n', [File]),
    consult_file_internal(File).

% Вътрешна помощна функция за зареждане
consult_file_internal(File) :-
    % Проверка за абсолютен или относителен път
    (   exists_file(File)
    ->  Path = File
    ;   runtime_dir(Dir),
        atomic_list_concat([Dir, '/', File], Path)
    ),
    
    format('Full path: ~w~n', [Path]),
    (   exists_file(Path)
    ->  (   catch(standard_consult(Path), Error,
                (format('[ERROR] Syntax error in ~w: ~w~n', [File, Error]),
                 fail))
        ->  retractall(active_file(_)),
            assertz(active_file(File)),
            (   loaded_file(File) -> true ; assertz(loaded_file(File))),
            format('[OK] Consulted ~w~n', [File])
        ;   format('[WARNING] consult failed for ~w~n', [File]),
            fail
        )
    ;   format('[ERROR] File does not exist: ~w~n', [Path]),
        fail
    ).

% Използване на стандартния consult
standard_consult(File) :-
    consult(File).

% Стандартен reconsult
user:reconsult(File) :-
    format('Reconsulting: ~w~n', [File]),
    (   loaded_file(File)
    ->  unload_file_internal(File)
    ;   true
    ),
    consult_file_internal(File).

% ========================================
% EXTENDED FILE LOADING
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
            (   catch(consult_file_internal(File), Error,
                    format('[ERROR] Failed to consult ~w: ~w~n', [File, Error]))
            )
        ),
        findall(F, loaded_file(F), LoadedFiles),
        format('Successfully loaded ~w files: ~w~n', [length(LoadedFiles), LoadedFiles])
    ).

% ========================================
% FILE UNLOADING
% ========================================

% Вътрешно разтоварване на файл
unload_file_internal(File) :-
    format('Unloading file: ~w~n', [File]),
    retractall(loaded_file(File)),
    (   active_file(File)
    ->  retractall(active_file(File)),
        format('File ~w unloaded and deactivated~n', [File])
    ;   format('File ~w unloaded (was not active)~n', [File])
    ).

% Публична команда за разтоварване
unload_file(File) :-
    unload_file_internal(File).

% Разтоварване на всички файлове
unload_all :-
    findall(F, loaded_file(F), Files),
    (   Files = []
    ->  writeln('No files to unload')
    ;   forall(member(F, Files), unload_file_internal(F)),
        length(Files, Count),
        format('All ~w files unloaded~n', [Count])
    ).

% Превключване към нов файл
switch_file(NewFile) :-
    (   active_file(CurrentFile)
    ->  format('Switching from ~w to ~w~n', [CurrentFile, NewFile]),
        unload_file_internal(CurrentFile)
    ;   true
    ),
    consult_file_internal(NewFile).

% ========================================
% KNOWLEDGE BASE MANAGEMENT
% ========================================

clear_all_facts :-
    writeln('Clearing all dynamic predicates from active file...'),
    (   active_file(File)
    ->  format('Active file: ~w~n', [File]),
        % Изчистване на всички динамични предикати
        current_predicate(Pred/Arity),
        predicate_property(Pred, dynamic),
        \+ predicate_property(Pred, built_in),
        format('  Retracting: ~w/~w~n', [Pred, Arity]),
        retractall(Pred),
        fail  % backtrack за всички предикати
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

list_predicates :-
    (   active_file(File)
    ->  format('Dynamic predicates in ~w:~n', [File]),
        current_predicate(Pred/Arity),
        predicate_property(Pred, dynamic),
        \+ predicate_property(Pred, built_in),
        format('  ~w/~w~n', [Pred, Arity]),
        fail  % backtrack за всички предикати
    ;   writeln('No active file to list predicates from')
    ),
    writeln('(end of list)').

% ========================================
% ИНИЦИАЛИЗАЦИЯ
% ========================================

% Автоматично зареждане при стартиране
:- initialization(
    (   current_prolog_flag(argv, Args),
        (   member('--dir', Args),
            nextto('--dir', Dir, Args)
        ->  set_runtime_dir(Dir)
        ;   working_directory(Dir, Dir),
            set_runtime_dir(Dir)
        ),
        format('Prolog environment initialized in: ~w~n', [Dir]),
        format('Type help. for available commands.~n')
    )
).
