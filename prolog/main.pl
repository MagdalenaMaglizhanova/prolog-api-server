:- dynamic loaded_file/1.
:- dynamic active_file/1.
:- dynamic runtime_dir/1.

% ========================================
% ENCODING SETTINGS - много важно за кирилица!
% ========================================
:- set_prolog_flag(encoding, utf8).

% ========================================
% SYSTEM COMMANDS
% ========================================

set_runtime_dir(Dir) :-
    retractall(runtime_dir(_)),
    assertz(runtime_dir(Dir)),
    format('Runtime dir set to: ~w~n', [Dir]).

help :-
    format('~s~n', ['HELP - Команди']),
    format('~s~n', ['========================================']),
    format('~s~n', ['УПРАВЛЕНИЕ НА ФАЙЛОВЕ:']),
    format('~s~n', ['  help.                    - Покажи помощ']),
    format('~s~n', ['  load_all.                - Зареди всички .pl файлове']),
    format('~s~n', ['  consult_file(File).      - Зареди конкретен файл']),
    format('~s~n', ['  reconsult_file(File).    - Презареди файл (изчисти + зареди)']),
    format('~s~n', ['  unload_file(File).       - Разтовари файл']),
    format('~s~n', ['  unload_all.              - Разтовари всички файлове']),
    format('~s~n', ['  switch_file(NewFile).    - Превключи към нов файл']),
    format('~s~n', ['  clear_all_facts.         - Изчисти всички факти от паметта']),
    format('~s~n', ['  list_files.              - Покажи заредените файлове']),
    format('~s~n', ['  current_file.            - Покажи активния файл']),
    format('~s~n', ['  list_predicates.         - Покажи предикатите в активния файл']),
    format('~s~n', ['ИЗПЪЛНЕНИЕ:']),
    format('~s~n', ['  Всякакви Prolog заявки (например menu.).']),
    format('~s~n', ['========================================']).

% ========================================
% FILE LOADING (променено за кирилица)
% ========================================

load_all :-
    runtime_dir(Dir),
    format('Looking for files in: ~w~n', [Dir]),
    directory_files(Dir, Files),
    format('Found files: ~w~n', [Files]),
    
    % Намиране на всички .pl файлове, включително с кирилица
    findall(F, 
        (member(F, Files), 
         (file_name_extension(_, pl, F); 
          sub_atom(F, _, 3, 0, '.pl') ; 
          sub_atom(F, _, 3, 0, '.PL'))), 
        PlFiles),
    
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
    
    % Декодиране на URL encoded имена на файлове
    (   sub_atom(File, _, _, _, '%') 
    ->  uri_encoded(query_value, DecodedFile, File)
    ;   DecodedFile = File
    ),
    
    atomic_list_concat([Dir, '/', DecodedFile], Path),
    format('Full path: ~w~n', [Path]),
    
    (   exists_file(Path)
    ->  % Задаване на UTF-8 encoding за четене на файла
        open(Path, read, Stream, [encoding(utf8)]),
        close(Stream),
        
        (   catch(consult(Path), Error,
                (format('[ERROR] Syntax error in ~w: ~w~n', [DecodedFile, Error]),
                 fail))
        ->  retractall(active_file(_)),
            assertz(active_file(DecodedFile)),
            (   loaded_file(DecodedFile) -> true ; assertz(loaded_file(DecodedFile))),
            format('[OK] Consulted ~w~n', [DecodedFile])
        ;   format('[WARNING] consult/1 failed for ~w~n', [DecodedFile]),
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
% INFORMATION COMMANDS (променени за кирилица)
% ========================================

list_files :-
    findall(F, loaded_file(F), Files),
    (   Files = []
    ->  format('~s~n', ['Няма заредени файлове'])
    ;   format('Заредени файлове (~w): ~w~n', [length(Files), Files])
    ).

current_file :-
    (   active_file(F)
    ->  format('Активен файл: ~w~n', [F])
    ;   format('~s~n', ['Няма активен файл'])
    ).

% Показва всички предикати в активния файл
list_predicates :-
    (   active_file(File)
    ->  format('Динамични предикати в ~w:~n', [File]),
        current_predicate(Pred/Arity),
        predicate_property(Pred, dynamic),
        format('  ~w/~w~n', [Pred, Arity]),
        fail  % backtrack за всички предикати
    ;   format('~s~n', ['Няма активен файл за показване на предикати'])
    ),
    format('~s~n', ['(край на списъка)']).

% ========================================
% ДОПЪЛНИТЕЛНИ ПРЕДИКАТИ ЗА КИРИЛИЦА
% ========================================

% Предикат за показване на помощ с кирилица
помощ :-
    help.

% Предикат за списък на файловете с кирилица
списък_файлове :-
    list_files.

% Предикат за текущ файл с кирилица
текущ_файл :-
    current_file.

% Предикат за зареждане на всички файлове с кирилица
зареди_всички :-
    load_all.

% Предикат за изчистване на паметта с кирилица
изчисти_памет :-
    clear_all_facts.
