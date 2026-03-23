:- dynamic student/5.

% student(ID, Name, CheckInMinutes, CheckOutMinutes, Status)
% Status is either in or out.

data_file(File) :-
    source_file(data_file(_), SourceFile),
    file_directory_name(SourceFile, CurrentDir),
    directory_file_path(CurrentDir, '../data/university.txt', File).

start :-
    initialize_system,
    main_menu.

initialize_system :-
    retractall(student(_, _, _, _, _)),
    load_students.

load_students :-
    retractall(student(_, _, _, _, _)),
    data_file(File),
    ( exists_file(File) ->
        open(File, read, Stream),
        read_students_stream(Stream),
        close(Stream)
    ;
        true
    ).

read_students_stream(Stream) :-
    read(Stream, Term),
    ( Term == end_of_file ->
        true
    ; normalize_term(Term, student(ID, Name, In, Out, Status)) ->
        assertz(student(ID, Name, In, Out, Status)),
        read_students_stream(Stream)
    ;
        read_students_stream(Stream)
    ).

normalize_term(student(ID, Name, In, Out, Status), student(ID, Name, In, Out, NStatus)) :-
    normalize_status(Status, NStatus).
normalize_term(estudiante(ID, Name, In, Out, Status), student(ID, Name, In, Out, NStatus)) :-
    normalize_status(Status, NStatus).

normalize_status(in, in).
normalize_status(out, out).
normalize_status('in', in).
normalize_status('out', out).
normalize_status(dentro, in).
normalize_status(fuera, out).
normalize_status('dentro', in).
normalize_status('fuera', out).

save_students :-
    data_file(File),
    file_directory_name(File, DataDir),
    make_directory_path(DataDir),
    open(File, write, Stream),
    forall(
        student(ID, Name, In, Out, Status),
        (
            writeq(Stream, student(ID, Name, In, Out, Status)),
            write(Stream, '.'),
            nl(Stream)
        )
    ),
    close(Stream).

current_minutes(Minutes) :-
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, local),
    date_time_value(hour, DateTime, Hour),
    date_time_value(minute, DateTime, Minute),
    Minutes is Hour * 60 + Minute.

check_in_student(ID, _Name) :-
    student(ID, _, _, _, in),
    !,
    fail.
check_in_student(ID, Name) :-
    current_minutes(Now),
    ( retract(student(ID, ExistingName, _, _, out)) ->
        FinalName = ExistingName
    ;
        FinalName = Name
    ),
    assertz(student(ID, FinalName, Now, 0, in)),
    save_students.

check_out_student(ID) :-
    student(ID, Name, In, _, in),
    !,
    current_minutes(Now),
    retract(student(ID, Name, In, _, in)),
    assertz(student(ID, Name, In, Now, out)),
    save_students.
check_out_student(_) :-
    fail.

search_inside_student(ID, student(ID, Name, In, Out, Status)) :-
    student(ID, Name, In, Out, Status),
    Status == in.

time_spent_minutes(ID, Minutes) :-
    student(ID, _, In, Out, Status),
    ( Status == out ->
        duration_minutes(In, Out, Minutes)
    ;
        current_minutes(Now),
        duration_minutes(In, Now, Minutes)
    ).

list_students_loaded(List) :-
    load_students,
    findall(student(ID, Name, In, Out, Status), student(ID, Name, In, Out, Status), List).

print_students([]) :-
    writeln('No students found in file.').
print_students([student(ID, Name, In, Out, Status) | Rest]) :-
    format('ID: ~w | Name: ~w | Entry: ', [ID, Name]),
    print_hhmm(In),
    ( Status == in ->
        format(' | Status: in~n', [])
    ;
        format(' | Exit: ', []),
        print_hhmm(Out),
        duration_minutes(In, Out, Duration),
        format(' | Status: out | Duration: ~w min~n', [Duration])
    ),
    print_students_rows(Rest).

print_students_rows([]) :-
    true.
print_students_rows([student(ID, Name, In, Out, Status) | Rest]) :-
    format('ID: ~w | Name: ~w | Entry: ', [ID, Name]),
    print_hhmm(In),
    ( Status == in ->
        format(' | Status: in~n', [])
    ;
        format(' | Exit: ', []),
        print_hhmm(Out),
        duration_minutes(In, Out, Duration),
        format(' | Status: out | Duration: ~w min~n', [Duration])
    ),
    print_students_rows(Rest).

duration_minutes(Start, End, Duration) :-
    Raw is End - Start,
    ( Raw >= 0 ->
        Duration = Raw
    ;
        Duration is Raw + 1440
    ).

print_hhmm(Minutes) :-
    Hour is (Minutes // 60) mod 24,
    Minute is Minutes mod 60,
    format('~|~`0t~d~2+:~|~`0t~d~2+', [Hour, Minute]).

main_menu :-
    writeln(''),
    writeln('=== STUDENT REGISTRATION SYSTEM (PROLOG) ==='),
    writeln('1. Check In'),
    writeln('2. Search by Student ID'),
    writeln('3. Time Calculation'),
    writeln('4. Students List (Load from File)'),
    writeln('5. Check Out'),
    writeln('6. Exit'),
    write('Choose option (1-6): '),
    read(Option),
    handle_option(Option).

handle_option(1) :-
    read_id(ID),
    read_name(Name),
    ( check_in_student(ID, Name) ->
        writeln('Check in recorded successfully.')
    ;
        writeln('The student is already inside or input was invalid.')
    ),
    main_menu.
handle_option(2) :-
    read_id(ID),
    ( search_inside_student(ID, student(ID, Name, In, _, _)) ->
        format('Student found: ~w (~w), entry at ', [Name, ID]),
        print_hhmm(In),
        nl
    ;
        writeln('Student not found inside the university.')
    ),
    main_menu.
handle_option(3) :-
    read_id(ID),
    ( time_spent_minutes(ID, Minutes) ->
        format('Time spent for ID ~w: ~w minutes.~n', [ID, Minutes])
    ;
        writeln('Student ID not found.')
    ),
    main_menu.
handle_option(4) :-
    list_students_loaded(List),
    print_students(List),
    main_menu.
handle_option(5) :-
    read_id(ID),
    ( check_out_student(ID) ->
        writeln('Check out recorded successfully.')
    ;
        writeln('Student not found inside the university.')
    ),
    main_menu.
handle_option(6) :-
    writeln('Program finished.'),
    !.
handle_option(_) :-
    writeln('Invalid option.'),
    main_menu.

read_id(ID) :-
    write('Enter student ID (integer): '),
    read(Input),
    ( integer(Input) ->
        ID = Input
    ;
        writeln('Invalid ID. Please enter an integer.'),
        read_id(ID)
    ).

read_name(Name) :-
    write('Enter student name (atom, for example juan): '),
    read(Input),
    ( atom(Input) ->
        Name = Input
    ;
        writeln('Invalid name. Use an atom like juan.'),
        read_name(Name)
    ).
