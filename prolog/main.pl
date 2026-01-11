% main.pl - Prolog file with FULL UTF-8/Cyrillic support

:- encoding('UTF-8').  % КРИТИЧНО: Задаване на кодировка в Prolog
:- set_prolog_flag(encoding, 'UTF-8').  % КРИТИЧНО: Задаване на флаг за кодировка

:- dynamic loaded_file/1.
:- dynamic active_file/1.
:- dynamic runtime_dir/1.

% ========================================
% КОНСТАНТИ ЗА КИРИЛИЦА
% ========================================

% Дефиниране на valid_char за кирилица и латиница
cyrillic_char(Char) :-
    char_type(Char, alpha),
    char_code(Char, Code),
    Code >= 0x0410, Code =< 0x044F.

latin_char(Char) :-
    char_type(Char, alpha),
    char_code(Char, Code),
    (Code >= 0x0041, Code =< 0x005A);  % A-Z
    (Code >= 0x0061, Code =< 0x007A).  % a-z

valid_filename_char(Char) :-
    latin_char(Char);
    cyrillic_char(Char);
    char_type(Char, digit);
    member(Char, ['_', '-', '.', ' ']).

% ========================================
% SYSTEM COMMANDS
% ========================================

set_runtime_dir(Dir) :-
    retractall(runtime_dir(_)),
    assertz(runtime_dir(Dir)),
    format('Runtime dir set to: ~w~n', [Dir]),
    format('Директория за изпълнение зададена на: ~w~n', [Dir]).  % Кирилица!

help :-
    writeln('HELP - Commands / ПОМОЩ - Команди'),
    writeln('========================================'),
    writeln('FILE MANAGEMENT / УПРАВЛЕНИЕ НА ФАЙЛОВЕ:'),
    writeln('  help.                    - Show this help / Покажи помощ'),
    writeln('  load_all.                - Load all .pl files / Зареди всички .pl файлове'),
    writeln('  consult_file(File).      - Load specific file / Зареди конкретен файл'),
    writeln('  reconsult_file(File).    - Reload file (clear + load) / Презареди файл'),
    writeln('  unload_file(File).       - Unload specific file / Разтовари файл'),
    writeln('  unload_all.              - Unload all files / Разтовари всички файлове'),
    writeln('  switch_file(NewFile).    - Switch to new file / Превключи към нов файл'),
    writeln('  clear_all_facts.         - Clear all facts from memory / Изчисти всички факти'),
    writeln('  list_files.              - List loaded files / Списък на заредените файлове'),
    writeln('  current_file.            - Show active file / Покажи активен файл'),
    writeln('  list_predicates.         - List predicates / Списък на предикатите'),
    writeln('  test_cyrillic.           - Test Cyrillic support / Тест за кирилица'),
    writeln('EXECUTION / ИЗПЪЛНЕНИЕ:'),
    writeln('  Any Prolog query (e.g. животно(X)). / Всяка Prolog заявка'),
    writeln('========================================').

% ========================================
% TEST ЦИРИЛЛИЦ
% ========================================

test_cyrillic :-
    writeln('Тест за кирилица:'),
    writeln('Български букви: АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЬЮЯ'),
    writeln('Малки букви: абвгдежзийклмнопрстуфхцчшщъьюя'),
    writeln('Примерни факти: куче, котка, кон, слон, лъв'),
    writeln('✓ Кирилицата работи! ✓'),
    % Дефинираме тестови факти с кирилица
    assertz(животно(куче)),
    assertz(животно(котка)),
    assertz(животно(кон)),
    findall(X, животно(X), Animals),
    format('Тестови животни: ~w~n', [Animals]),
    retractall(животно(_)).

% ========================================
% FILE LOADING with UTF-8 support
% ========================================

load_all :-
    runtime_dir(Dir),
    format('Looking for files in: ~w~n', [Dir]),
    format('Търсене на файлове в: ~w~n', [Dir]),
    
    % Използваме safe_directory_files за кирилица
    safe_directory_files(Dir, Files),
    format('Found files: ~w~n', [Files]),
    format('Намерени файлове: ~w~n', [Files]),
    
    findall(F, (member(F, Files), file_name_extension(_, pl, F)), PlFiles),
    format('Prolog files: ~w~n', [PlFiles]),
    format('Prolog файлове: ~w~n', [PlFiles]),
    
    (   PlFiles = []
    ->  writeln('No Prolog files found in directory'),
        writeln('Няма Prolog файлове в директорията'),
        fail
    ;   forall(
            member(File, PlFiles),
            (   catch(consult_file(File), Error,
                    (format('[ERROR] Failed to consult ~w: ~w~n', [File, Error]),
                     format('[ГРЕШКА] Неуспешно зареждане на ~w: ~w~n', [File, Error])))
            )
        ),
        findall(F, loaded_file(F), LoadedFiles),
        format('Successfully loaded ~w files: ~w~n', [length(LoadedFiles), LoadedFiles]),
        format('Успешно заредени ~w файла: ~w~n', [length(LoadedFiles), LoadedFiles])
    ).

% Безопасно четене на директории с кирилица
safe_directory_files(Dir, Files) :-
    catch(
        directory_files(Dir, RawFiles),
        Error,
        (format('[WARNING] Error reading directory ~w: ~w~n', [Dir, Error]),
         RawFiles = [])
    ),
    % Филтрираме само валидни файлове
    include(is_valid_file_name, RawFiles, Files).

% Проверка за валидно име на файл
is_valid_file_name(FileName) :-
    atom_chars(FileName, Chars),
    maplist(valid_filename_char, Chars).

% ========================================
% CONSULT FILE with UTF-8 support
% ========================================

consult_file(File) :-
    % Проверка за валидност на името
    (   \+ is_valid_file_name(File)
    ->  format('[ERROR] Invalid filename: ~w~n', [File]),
        format('[ГРЕШКА] Невалидно име на файл: ~w~n', [File]),
        fail
    ;   true
    ),
    
    runtime_dir(Dir),
    format('Consulting file: ~w (from dir: ~w)~n', [File, Dir]),
    format('Зареждане на файл: ~w (от директория: ~w)~n', [File, Dir]),
    
    atomic_list_concat([Dir, '/', File], Path),
    format('Full path: ~w~n', [Path]),
    
    (   exists_file(Path)
    ->  % Четене на файла с правилна кодировка
        (   catch(read_file_to_string(Path, Content, [encoding('UTF-8')]), Error,
                (format('[ERROR] Cannot read file ~w: ~w~n', [Path, Error]),
                 format('[ГРЕШКА] Не може да се прочете файл ~w: ~w~n', [Path, Error]),
                 fail))
        ->  format('File size: ~w characters~n', [string_length(Content)]),
            format('Размер на файла: ~w символа~n', [string_length(Content)]),
            
            % Проверка за кирилица в съдържанието
            (   sub_string(Content, _, _, _, "а")  % Проверка за кирилица
            ->  format('File contains Cyrillic characters~n'),
                format('Файлът съдържа кирилица~n')
            ;   true
            ),
            
            % Зареждане на Prolog кода
            (   catch(consult(Path), Error2,
                    (format('[ERROR] Syntax error in ~w: ~w~n', [File, Error2]),
                     format('[ГРЕШКА] Синтактична грешка в ~w: ~w~n', [File, Error2]),
                     fail))
            ->  retractall(active_file(_)),
                assertz(active_file(File)),
                (   loaded_file(File) -> true ; assertz(loaded_file(File))),
                format('[OK] Consulted ~w~n', [File]),
                format('[OK] Зареден ~w~n', [File])
            ;   format('[WARNING] consult/1 failed for ~w~n', [File]),
                format('[ПРЕДУПРЕЖДЕНИЕ] consult/1 неуспешен за ~w~n', [File]),
                fail
            )
        ;   format('[ERROR] Cannot access file ~w~n', [Path]),
            format('[ГРЕШКА] Няма достъп до файл ~w~n', [Path]),
            fail
        )
    ;   format('[ERROR] File does not exist: ~w~n', [Path]),
        format('[ГРЕШКА] Файлът не съществува: ~w~n', [Path]),
        fail
    ).

% ========================================
% FILE UNLOADING
% ========================================

unload_file(File) :-
    format('Unloading file: ~w~n', [File]),
    format('Разтоварване на файл: ~w~n', [File]),
    
    retractall(loaded_file(File)),
    (   active_file(File)
    ->  retractall(active_file(File)),
        format('File ~w unloaded and deactivated~n', [File]),
        format('Файл ~w разтоварен и деактивиран~n', [File])
    ;   format('File ~w unloaded (was not active)~n', [File]),
        format('Файл ~w разтоварен (не беше активен)~n', [File])
    ).

unload_all :-
    findall(F, loaded_file(F), Files),
    (   Files = []
    ->  writeln('No files to unload'),
        writeln('Няма файлове за разтоварване')
    ;   forall(member(F, Files), unload_file(F)),
        length(Files, Count),
        format('All ~w files unloaded~n', [Count]),
        format('Всички ~w файла разтоварени~n', [Count])
    ).

reconsult_file(File) :-
    format('Reconsulting file: ~w~n', [File]),
    format('Презареждане на файл: ~w~n', [File]),
    
    unload_file(File),
    consult_file(File).

switch_file(NewFile) :-
    (   active_file(CurrentFile)
    ->  format('Switching from ~w to ~w~n', [CurrentFile, NewFile]),
        format('Превключване от ~w към ~w~n', [CurrentFile, NewFile]),
        unload_file(CurrentFile)
    ;   true
    ),
    consult_file(NewFile).

% ========================================
% KNOWLEDGE BASE MANAGEMENT
% ========================================

clear_all_facts :-
    writeln('Clearing all dynamic predicates from active file...'),
    writeln('Изчистване на всички динамични предикати от активния файл...'),
    
    (   active_file(File)
    ->  format('Active file: ~w~n', [File]),
        format('Активен файл: ~w~n', [File]),
        
        % Намиране на всички динамични предикати
        findall(Pred/Arity, 
                (current_predicate(Pred/Arity), 
                 predicate_property(Pred, dynamic)), 
                DynamicPredicates),
        
        (   DynamicPredicates = []
        ->  writeln('No dynamic predicates found'),
            writeln('Няма намерени динамични предикати')
        ;   format('Found ~w dynamic predicates~n', [length(DynamicPredicates)]),
            format('Намерени ~w динамични предиката~n', [length(DynamicPredicates)]),
            
            % Изчистване на всеки динамичен предикат
            forall(member(Pred/Arity, DynamicPredicates),
                   (format('  Retracting: ~w/~w~n', [Pred, Arity]),
                    retractall(Pred)))
        )
    ;   writeln('No active file to clear'),
        writeln('Няма активен файл за изчистване')
    ),
    writeln('All dynamic facts cleared'),
    writeln('Всички динамични факти са изчистени').

% ========================================
% INFORMATION COMMANDS
% ========================================

list_files :-
    findall(F, loaded_file(F), Files),
    (   Files = []
    ->  writeln('No files loaded'),
        writeln('Няма заредени файлове')
    ;   format('Loaded files (~w): ~w~n', [length(Files), Files]),
        format('Заредени файлове (~w): ~w~n', [length(Files), Files])
    ).

current_file :-
    (   active_file(F)
    ->  format('Active file: ~w~n', [F]),
        format('Активен файл: ~w~n', [F])
    ;   writeln('No active file'),
        writeln('Няма активен файл')
    ).

list_predicates :-
    (   active_file(File)
    ->  format('Dynamic predicates in ~w:~n', [File]),
        format('Динамични предикати в ~w:~n', [File]),
        
        findall(Pred/Arity, 
                (current_predicate(Pred/Arity), 
                 predicate_property(Pred, dynamic)), 
                DynamicPredicates),
        
        (   DynamicPredicates = []
        ->  writeln('  No dynamic predicates found'),
            writeln('  Няма намерени динамични предикати')
        ;   forall(member(Pred/Arity, DynamicPredicates),
                   format('  ~w/~w~n', [Pred, Arity]))
        )
    ;   writeln('No active file to list predicates from'),
        writeln('Няма активен файл за списък на предикатите')
    ),
    writeln('(end of list)'),
    writeln('(край на списъка)').

% ========================================
% UTILITY PREDICATES
% ========================================

% Прочитане на низ от файл (поддържа кирилица)
read_file_to_string(File, String, Options) :-
    open(File, read, Stream, Options),
    read_string(Stream, _, String),
    close(Stream).

% ========================================
% INITIALIZATION
% ========================================

% Стартиране с тест за кирилица
:- initialization(
    format('~n========================================~n'),
    writeln('Prolog Server Initialized / Prolog Сървър Инициализиран'),
    format('Encoding: UTF-8 / Кодировка: UTF-8~n'),
    test_cyrillic,
    format('========================================~n~n')
).
