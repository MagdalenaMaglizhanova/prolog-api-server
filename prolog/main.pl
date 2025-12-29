:- dynamic loaded_file/1.
:- dynamic active_file/1.
:- dynamic runtime_dir/1.

% Задава се от Node
set_runtime_dir(Dir) :-
    retractall(runtime_dir(_)),
    assertz(runtime_dir(Dir)).

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
    directory_files(Dir, Files),
    forall(
        (
            member(F, Files),
            file_name_extension(_, pl, F)
        ),
        consult_file(F)
    ).

consult_file(File) :-
    runtime_dir(Dir),
    file_name_extension(Name, pl, File),
    directory_file_path(Dir, Name, Path),
    exists_file(Path),
    consult(Path),
    retractall(active_file(_)),
    assertz(active_file(Name)),
    assertz(loaded_file(Name)),
    format('[OK] Consulted ~w~n', [Name]).

list_files :-
    findall(F, loaded_file(F), Files),
    writeln(Files).

current_file :-
    ( active_file(F)
    -> format('Active file: ~w~n', [F])
    ;  writeln('No active file')
    ).
