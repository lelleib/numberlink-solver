:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(file_systems)).

:- prolog_load_context(directory, Dir), current_directory(_, Dir).

% for all puzzles in PuzzleFolder, solution times will be appended to the file under OutFilename, using Timeout and different combinations of labeling options
numberlink_grid_search(PuzzleFolder, Timeout, OutFilename) :-
    member(VarOrdering, [leftmost, ff, ffc, impact, dom_w_deg]),
    member(ValSelection, [step, enum, bisect]),
    member(ValOrdering, [up, down, median]),
    LabelingOptions = [VarOrdering, ValSelection, ValOrdering],
    open(OutFilename, append, Stream),
    nl(Stream), write(Stream, 'Labeling options: '), write(Stream, LabelingOptions), nl(Stream),
    close(Stream),
    numberlink_test(PuzzleFolder, LabelingOptions, Timeout, OutFilename),
    fail.

% for all puzzles in PuzzleFolder, solution times will be appended to the file under OutFilename, while using Timeout and LabelingOptions
numberlink_test(PuzzleFolder, LabelingOptions, Timeout, OutFilename) :-
    findall(F, (
        directory_member_of_directory('puzzles', PuzzleFolder, _, Dir),
        file_member_of_directory(Dir, '*', _, F)
    ), Fs),
    reverse(Fs, Filenames),
    member(FileAbsolutePath, Filenames),
    ( numberlink_f(FileAbsolutePath, [time_out(Timeout, Result)|LabelingOptions], Solution, Time)
    -> Executed = 1
    ; Executed = 0
    ),
    open(OutFilename, append, Stream),
    write(Stream, 'Puzzle: '), convert_to_relative(FileAbsolutePath, FileRelativePath), write(Stream, FileRelativePath), nl(Stream),
    ( Executed == 1
    -> 
        ( Result == success
        -> write(Stream, 'Elapsed Time: '), write(Stream, Time), write(Stream, ' seconds\n'), 
            write(Stream, 'Found Solution: \n'),
            write_m(Stream, Solution)
        ; write(Stream, 'TIMEOUT ('), write(Stream, Timeout), write(Stream, 'ms)')
        )
    ; write(Stream, 'DID NOT EXECUTE')
    ),
    nl(Stream),
    close(Stream),
    fail.

% M is the solution of the puzzle that can be found under Filename, Time is the time required to solve, and solver uses LabelingOptions
numberlink_f(Filename, LabelingOptions, M, Time) :-
    see(Filename),
    read(M),
    seen,
    statistics(walltime, [Start,_]),
    numberlink(M, LabelingOptions),
    statistics(walltime, [Stop,_]),
    Time is (Stop-Start)*0.001.

% solves puzzle M (fills all free variables) with labeling options from LabelingOptions
numberlink(M, LabelingOptions) :-
    append(M, MFlat),
    max_member(Max, MFlat),
    domain(MFlat, 1, Max),
    apply_constraints(M),
    labeling(LabelingOptions, MFlat).

% applies all constraints for the puzzle M
apply_constraints(M) :-
    length(M, H),
    [Row|_] = M,
    length(Row, W),
    findall(I-J, (
        numlist(1, H, Is), member(I, Is),
        numlist(1, W, Js), member(J, Js)
    ), IJs),
    maplist(constraint(M), IJs, Constraints),
    maplist(call, Constraints).

% Constraint is the constraint that corresponds to the puzzle cell M[I,J] (Constraint is not posted)
constraint(M, I-J, Constraint) :-
    IP is I-1, IN is I+1,
    JP is J-1, JN is J+1,
    NeighborIJs = [IP-J, I-JP, I-JN, IN-J],
    scanlist(append_IJth(M), NeighborIJs, [], Neighbors),
    nth1(I, M, Row), nth1(J, Row, Cell),
    maplist(make_eq_constraint_reif(Cell), Neighbors, NeighborsEqList),
    ( nonvar(Cell) 
    -> Constraint = sum(NeighborsEqList, #=, 1) % filled cells
    ; Constraint = sum(NeighborsEqList, #=, 2) % empty cells
    ).

% append_IJth(List, I-J, Acc, NAcc):
% E := List[I,J], if E exists NAcc = [E|Acc], else NAcc = Acc
append_IJth(List, I-J, Acc, [Elem|Acc]) :- nth1(I, List, Row), nth1(J, Row, Elem), !.
append_IJth(_, _, List, List).

% ConstraintReif is a reified constraint that A equals B
make_eq_constraint_reif(A, B, ConstraintReif) :- A #= B #<=> ConstraintReif.

% prints a matrix
write_m([]).
write_m([H|T]) :-
    write(H),
    nl,
    write_m(T).

% prints a matrix on Stream
write_m(_, []).
write_m(Stream, [H|T]) :-
    write(Stream, H),
    nl(Stream),
    write_m(Stream, T).

convert_to_relative(AbsolutePath, RelativePath) :-
    current_directory(CurrentDir),
    atom_chars(AbsolutePath, AbsChars),
    atom_chars(CurrentDir, CurrentChars),
    append(CurrentChars, RelChars, AbsChars),
    atom_chars(RelativePath, RelChars).
