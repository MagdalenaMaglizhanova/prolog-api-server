:- dynamic loaded_file/1.
:- dynamic active_file/1.
:- dynamic runtime_dir/1.

set_runtime_dir(Dir) :-
    retractall(runtime_dir(_)),
    assertz(runtime_dir(Dir)),
    format('Runtime dir set to: ~w~n', [Dir]).

help :-
    writeln('HELP - Commands'),
    writeln('========================================'),
    writeln('SYSTEM COMMANDS:'),
    writeln('  help.'),
    writeln('  load_all.'),
    writeln('  list_files.'),
    writeln('  consult_file(File).'),
    writeln('  current_file.'),
    writeln('EXECUTION:'),
    writeln('  Any Prolog query (e.g. fly(X)).'),
    writeln('========================================').

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
            (consult_file(File) -> true ; format('Failed to consult: ~w~n', [File]))
        ),
        writeln('All files loaded successfully')
    ).

consult_file(File) :-
    runtime_dir(Dir),
    format('Consulting file: ~w (from dir: ~w)~n', [File, Dir]),
    atomic_list_concat([Dir, '/', File], Path),
    format('Full path: ~w~n', [Path]),
    (   exists_file(Path)
    ->  consult(Path),
        retractall(active_file(_)),
        assertz(active_file(File)),
        assertz(loaded_file(File)),
        format('[OK] Consulted ~w~n', [File])
    ;   format('[ERROR] File does not exist: ~w~n', [Path]),
        fail
    ).

list_files :-
    findall(F, loaded_file(F), Files),
    format('Loaded files: ~w~n', [Files]).

current_file :-
    (   active_file(F)
    ->  format('Active file: ~w~n', [F])
    ;   writeln('No active file')
    ).
