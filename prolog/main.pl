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
    writeln('  system_info.             - Show system information'),
    writeln('EXECUTION:'),
    writeln('  Any Prolog query (e.g. fly(X)).'),
    writeln('========================================').

system_info :-
    format('=== Prolog System Information ===~n', []),
    format('Runtime directory: ~w~n', [runtime_dir]),
    format('Loaded files: ~w~n', [loaded_file]),
    format('Active file: ~w~n', [active_file]),
    writeln('===============================').

% ========================================
% FILE LOADING (STANDARD PROLOG COMPATIBILITY)
% ========================================

% Standard Prolog consult - override
user:consult(File) :-
    (   atom_concat(_, '.pl', File) -> true
    ;   atom_concat(File, '.pl', _)
    ),
    !,
    format('Consulting: ~w~n', [File]),
    consult_file_internal(File).

% Loading via [filename] syntax
user:(File) :-
    atom(File),
    (   atom_concat(_, '.pl', File) -> true
    ;   atom_concat(File, '.pl', _)
    ),
    !,
    format('Consulting: ~w~n', [File]),
    consult_file_internal(File).

% Internal helper function for loading
consult_file_internal(File) :-
    % Check for absolute or relative path
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

% Use standard consult
standard_consult(File) :-
    consult(File).

% Standard reconsult
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

% Internal file unloading
unload_file_internal(File) :-
    format('Unloading file: ~w~n', [File]),
    retractall(loaded_file(File)),
    (   active_file(File)
    ->  retractall(active_file(File)),
        format('File ~w unloaded and deactivated~n', [File])
    ;   format('File ~w unloaded (was not active)~n', [File])
    ).

% Public unload command
unload_file(File) :-
    unload_file_internal(File).

% Unload all files
unload_all :-
    findall(F, loaded_file(F), Files),
    (   Files = []
    ->  writeln('No files to unload')
    ;   forall(member(F, Files), unload_file_internal(F)),
        length(Files, Count),
        format('All ~w files unloaded~n', [Count])
    ).

% Switch to new file
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
        % Clear all dynamic predicates
        current_predicate(Pred/Arity),
        predicate_property(Pred, dynamic),
        \+ predicate_property(Pred, built_in),
        format('  Retracting: ~w/~w~n', [Pred, Arity]),
        retractall(Pred),
        fail  % backtrack for all predicates
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
        fail  % backtrack for all predicates
    ;   writeln('No active file to list predicates from')
    ),
    writeln('(end of list)').

% ========================================
% INITIALIZATION
% ========================================

% Auto initialization
:- initialization(
    (   current_prolog_flag(argv, Args),
        (   member('--dir', Args),
            nextto('--dir', Dir, Args)
        ->  set_runtime_dir(Dir)
        ;   working_directory(Dir, Dir),
            set_runtime_dir(Dir)
        ),
        format('=== Prolog Environment Initialized ===~n', []),
        format('Runtime directory: ~w~n', [Dir]),
        format('Type help. for available commands.~n'),
        format('====================================~n')
    )
).

% ========================================
% DOMAIN MANAGEMENT EXTENSIONS
% ========================================

% Select domain command
select_domain(Domain) :-
    format('Selecting domain: ~w~n', [Domain]),
    runtime_dir(Dir),
    atomic_list_concat([Dir, '/', Domain, '.pl'], Path),
    (   exists_file(Path)
    ->  consult_file_internal(Path),
        format('Domain ~w loaded successfully.~n', [Domain])
    ;   atomic_list_concat([Dir, '/domains/', Domain, '.pl'], Path2),
        (   exists_file(Path2)
        ->  consult_file_internal(Path2),
            format('Domain ~w loaded from domains directory.~n', [Domain])
        ;   format('Domain file ~w.pl not found.~n', [Domain]),
            format('Looking in: ~w and ~w/domains/~n', [Dir, Dir]),
            fail
        )
    ).

% Command handler for HTTP requests
process_command(Command, Output) :-
    catch(
        (   (   Command = help
            ->  help
            ;   Command = list_files
            ->  list_files
            ;   Command = load_all
            ->  load_all
            ;   Command = system_info
            ->  system_info
            ;   atom_concat('consult_file(', Rest, Command),
                sub_atom(Rest, 0, _, 1, FileName)
            ->  consult_file_internal(FileName)
            ;   atom_concat('select_domain(', Rest2, Command),
                sub_atom(Rest2, 0, _, 1, Domain)
            ->  select_domain(Domain)
            ;   % For other commands, try to execute them
                term_string(Term, Command),
                call(Term)
            )
        ),
        Output = 'Command executed successfully.'
    ),
    catch(
        with_output_to(string(Output), 
            (   listing(loaded_file/1),
                listing(active_file/1)
            )
        ),
        _,
        Output = 'Command executed but output capture failed.'
    ).

% HTTP endpoint handler (simplified)
handle_query(Code, Query, Response) :-
    % Store code in a temporary file
    term_string(QueryTerm, Query),
    process_command(QueryTerm, Response).
