:- encoding(utf8).
:- dynamic runtime_dir/1.
:- dynamic active_sessions/1.  % Track active sessions

% Thread-local variables - each thread (user) has its own copy
:- thread_local loaded_file/1.
:- thread_local active_file/1.

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
    writeln('  reconsult_file(File).    - Reload file'),
    writeln('  unload_file(File).       - Unload specific file'),
    writeln('  unload_all.              - Unload all files'),
    writeln('  clear_all_facts.         - Clear all facts'),
    writeln('  list_files.              - List loaded files'),
    writeln('  current_file.            - Show active file'),
    writeln('  list_predicates.         - List predicates'),
    writeln('  init_session.            - Initialize user session'),
    writeln('  end_session.             - End current session'),
    writeln('  list_sessions.           - List active sessions'),
    writeln('EXECUTION:'),
    writeln('  Any Prolog query (e.g. fly(X)).'),
    writeln('========================================').

% ========================================
% SESSION MANAGEMENT
% ========================================

% Initialize a new session for a user
init_session :-
    retractall(loaded_file(_)),
    retractall(active_file(_)),
    thread_self(ThreadId),
    (   active_sessions(Sessions)
    ->  (   memberchk(ThreadId, Sessions)
        ->  true
        ;   retractall(active_sessions(_)),
            assertz(active_sessions([ThreadId|Sessions]))
        )
    ;   assertz(active_sessions([ThreadId]))
    ),
    format('Session initialized for thread: ~w~n', [ThreadId]).

% Initialize session with user ID
init_session(UserId) :-
    retractall(loaded_file(_)),
    retractall(active_file(_)),
    (   active_sessions(Sessions)
    ->  (   memberchk(UserId, Sessions)
        ->  true
        ;   retractall(active_sessions(_)),
            assertz(active_sessions([UserId|Sessions]))
        )
    ;   assertz(active_sessions([UserId]))
    ),
    format('Session initialized for user: ~w~n', [UserId]).

% End current session
end_session :-
    thread_self(ThreadId),
    retractall(loaded_file(_)),
    retractall(active_file(_)),
    (   active_sessions(Sessions)
    ->  delete(Sessions, ThreadId, NewSessions),
        retractall(active_sessions(_)),
        (   NewSessions \= [] -> assertz(active_sessions(NewSessions)) ; true)
    ;   true
    ),
    format('Session ended for thread: ~w~n', [ThreadId]).

% End session for specific user
end_session(UserId) :-
    retractall(loaded_file(_)),
    retractall(active_file(_)),
    (   active_sessions(Sessions)
    ->  delete(Sessions, UserId, NewSessions),
        retractall(active_sessions(_)),
        (   NewSessions \= [] -> assertz(active_sessions(NewSessions)) ; true)
    ;   true
    ),
    format('Session ended for user: ~w~n', [UserId]).

% List all active sessions
list_sessions :-
    (   active_sessions(Sessions)
    ->  length(Sessions, Count),
        format('Active sessions (~d): ~w~n', [Count, Sessions])
    ;   writeln('No active sessions')
    ).

% ========================================
% FILE LOADING - REDUCED OUTPUT VERSION
% ========================================

% Load all Prolog files from runtime directory
load_all :-
    runtime_dir(Dir),
    directory_files(Dir, Files),
    include(has_pl_extension, Files, PlFiles),
    length(PlFiles, Total),
    format('Loading ~d file(s) from ~s... ', [Total, Dir]),
    flush_output,
    
    findall(File, (
        member(File, PlFiles),
        catch(consult_file_silent(File), _, fail)
    ), Loaded),
    
    length(Loaded, LoadedCount),
    (   LoadedCount =:= Total
    ->  format('✓ All ~d files loaded~n', [LoadedCount])
    ;   format('✓ Loaded ~d/~d files~n', [LoadedCount, Total])
    ).

% Check if file has .pl extension
has_pl_extension(File) :-
    file_name_extension(_, pl, File).

% Silent version of consult_file - minimal output
consult_file_silent(File) :-
    runtime_dir(Dir),
    atomic_list_concat([Dir, '/', File], Path),
    catch(consult(Path), _, fail),
    retractall(active_file(_)),
    assertz(active_file(File)),
    (   loaded_file(File) -> true ; assertz(loaded_file(File))).

% Consult a specific file with basic output
consult_file(File) :-
    runtime_dir(Dir),
    atomic_list_concat([Dir, '/', File], Path),
    (   exists_file(Path)
    ->  (   catch(consult(Path), Error,
                (format('[ERROR] ~s: ~w~n', [File, Error]), fail))
        ->  retractall(active_file(_)),
            assertz(active_file(File)),
            (   loaded_file(File) -> true ; assertz(loaded_file(File))),
            format('[OK] ~s loaded~n', [File])
        ;   format('[WARNING] Failed to load ~s~n', [File]), fail
        )
    ;   format('[ERROR] File not found: ~s~n', [File]), fail
    ).

% Consult file for specific user
consult_file(File, UserId) :-
    init_session(UserId),
    consult_file(File).

% ========================================
% FILE UNLOADING - SIMPLIFIED VERSION
% ========================================

% Unload a specific file
unload_file(File) :-
    retractall(loaded_file(File)),
    (   active_file(File) -> retractall(active_file(_)) ; true),
    abolish_all_tables,
    format('✓ Unloaded: ~s~n', [File]).

% Unload file for specific user
unload_file(File, UserId) :-
    init_session(UserId),
    unload_file(File).

% Unload all loaded files
unload_all :-
    findall(F, loaded_file(F), Files),
    (   Files = []
    ->  writeln('No files loaded')
    ;   forall(member(F, Files), retractall(loaded_file(F))),
        retractall(active_file(_)),
        abolish_all_tables,
        length(Files, Count),
        format('✓ Unloaded ~d file(s)~n', [Count])
    ).

% Unload all files for specific user
unload_all(UserId) :-
    init_session(UserId),
    unload_all.

% Reconsult a file (unload + load)
reconsult_file(File) :-
    runtime_dir(Dir),
    atomic_list_concat([Dir, '/', File], Path),
    (   exists_file(Path)
    ->  retractall(loaded_file(File)),
        (   active_file(File) -> retractall(active_file(_)) ; true),
        abolish_all_tables,
        (   catch(consult(Path), Error,
                (format('[ERROR] ~s: ~w~n', [File, Error]), fail))
        ->  assertz(loaded_file(File)),
            assertz(active_file(File)),
            format('[OK] ~s reloaded~n', [File])
        ;   format('[WARNING] Failed to reload ~s~n', [File]), fail
        )
    ;   format('[ERROR] File not found: ~s~n', [File]), fail
    ).

% Reconsult file for specific user
reconsult_file(File, UserId) :-
    init_session(UserId),
    reconsult_file(File).

% Switch to a new file (unload current + load new)
switch_file(NewFile) :-
    (   active_file(CurrentFile)
    ->  format('Switching from ~s to ~s~n', [CurrentFile, NewFile]),
        unload_file(CurrentFile)
    ;   true
    ),
    consult_file(NewFile).

% Switch file for specific user
switch_file(NewFile, UserId) :-
    init_session(UserId),
    switch_file(NewFile).

% ========================================
% KNOWLEDGE BASE MANAGEMENT
% ========================================

% Clear all dynamic facts from active file
clear_all_facts :-
    (   active_file(File)
    ->  findall(Pred/Arity, 
                (current_predicate(Pred/Arity), 
                 predicate_property(Pred, dynamic)), 
                Predicates),
        (   Predicates = []
        ->  format('No dynamic predicates in ~s~n', [File])
        ;   forall(member(Pred/Arity, Predicates), retractall(Pred)),
            length(Predicates, Count),
            format('✓ Cleared ~d predicate(s) from ~s~n', [Count, File])
        ),
        abolish_all_tables
    ;   writeln('No active file selected')
    ).

% Clear facts for specific user
clear_all_facts(UserId) :-
    init_session(UserId),
    clear_all_facts.

% ========================================
% INFORMATION COMMANDS
% ========================================

% List all loaded files
list_files :-
    findall(F, loaded_file(F), Files),
    (   Files = []
    ->  writeln('No files loaded')
    ;   length(Files, Count),
        format('Loaded files (~d):~n', [Count]),
        forall(member(F, Files), format('  - ~s~n', [F]))
    ).

% List files for specific user
list_files(UserId) :-
    init_session(UserId),
    list_files.

% Show current active file
current_file :-
    (   active_file(F)
    ->  format('Active file: ~s~n', [F])
    ;   writeln('No active file')
    ).

% Show current file for specific user
current_file(UserId) :-
    init_session(UserId),
    current_file.

% List all predicates in active file
list_predicates :-
    (   active_file(File)
    ->  findall(Pred/Arity, 
                (current_predicate(Pred/Arity), 
                 predicate_property(Pred, dynamic)), 
                Predicates),
        (   Predicates = []
        ->  format('No dynamic predicates in ~s~n', [File])
        ;   format('Dynamic predicates in ~s (~d):~n', [File, length(Predicates)]),
            forall(member(Pred/Arity, Predicates), 
                   format('  ~w/~w~n', [Pred, Arity]))
        )
    ;   writeln('No active file selected')
    ).

% List predicates for specific user
list_predicates(UserId) :-
    init_session(UserId),
    list_predicates.

% ========================================
% UTILITY PREDICATES
% ========================================

% Get current thread ID
show_thread :-
    thread_self(ThreadId),
    format('Current thread: ~w~n', [ThreadId]).

% Get session info for current thread
session_info :-
    thread_self(ThreadId),
    (   active_sessions(Sessions)
    ->  (   memberchk(ThreadId, Sessions)
        ->  format('Session active for thread: ~w~n', [ThreadId]),
            list_files
        ;   format('No active session for thread: ~w~n', [ThreadId])
        )
    ;   writeln('No active sessions')
    ).

% Execute command in user context
with_user(UserId, Command) :-
    init_session(UserId),
    catch(call(Command), Error,
          format('Error executing command for user ~w: ~w~n', [UserId, Error])),
    !.

% Clean up stale sessions (call periodically)
cleanup_sessions :-
    thread_self(CurrentThread),
    (   active_sessions(Sessions)
    ->  findall(Thread, 
                (member(Thread, Sessions),
                 Thread \= CurrentThread,
                 \+ thread_property(Thread, status(_))),
                Stale),
        (   Stale = []
        ->  true
        ;   forall(member(Thread, Stale),
                   (format('Removing stale session: ~w~n', [Thread]),
                    delete(Sessions, Thread, NewSessions),
                    retractall(active_sessions(_)),
                    assertz(active_sessions(NewSessions))))
        )
    ;   true
    ).

% ========================================
% INITIALIZATION
% ========================================

% Initialize at module load
:- initialization(init).

init :-
    retractall(active_sessions(_)),
    format('Prolog multi-user system initialized~n').
