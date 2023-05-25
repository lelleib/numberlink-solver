:- use_module(library(clpfd)).
:- use_module(library(lists)).

% sample puzzle
puz([
[_, _, 1, 6, _, _, 4, _, _, _],
[_, _, _, _, 7, _, _, _, _, _],
[_, _, _, _, _, _, _, 5, _, _],
[_, _, _, _, 2, _, _, _, _, _],
[_, _, _, _, 3, _, 4, _, _, _],
[_, _, _, _, _, _, _, _, _, 6],
[_, _, _, _, _, _, _, _, _, _],
[_, _, _, _, _, _, _, _, _, _],
[_, 7, 5, _, _, _, _, 1, 2, _],
[_, _, _, _, 3, _, _, _, _, _]
]).
% solution to the sample puzzle
sol([
[1, 1, 1, 6, 6, 6, 4, 4, 4, 4],
[1, 7, 7, 7, 7, 6, 6, 6, 6, 4],
[1, 7, 5, 5, 5, 5, 5, 5, 6, 4],
[1, 7, 5, 2, 2, 6, 6, 6, 6, 4],
[1, 7, 5, 2, 3, 6, 4, 4, 4, 4],
[1, 7, 5, 2, 3, 6, 6, 6, 6, 6],
[1, 7, 5, 2, 3, 3, 3, 3, 3, 3],
[1, 7, 5, 2, 2, 2, 2, 2, 2, 3],
[1, 7, 5, 1, 1, 1, 1, 1, 2, 3],
[1, 1, 1, 1, 3, 3, 3, 3, 3, 3]
]).

puzZ([
[_, _, _, _, _],
[2, _, _, 1, _],
[_, _, _, _, _],
[1, _, _, 2, _],
[4, _, 4, 3, 3]
]).

% reads a puzzle from a specified file and solves it
numberlink_f(Filename, M) :-
    see(Filename),
    read(M),
    seen,
    numberlink(M).

% solves a puzzle, input matrix format above
numberlink(M) :-
    term_variables(M, MVars),
    append(M, MFlat), max_member(Max, MFlat),
    domain(MVars, 1, Max),
    constraints(M),
    labeling([], MVars).

% creates constraints for the puzzle given
constraints([HM|TM]) :-
    length([HM|TM], LengthI),
    length(HM, LengthJ),
    constraints([HM|TM], 0, LengthI, LengthJ).

% creates constraints for the puzzle given
% parameters: current index and matrix dimensions
constraints(_M, LengthI, LengthI, _LengthJ) :- !.
constraints(M, Idx, LengthI, LengthJ) :-
    nth0(Idx, M, Row),
    PIdx is Idx-1, NIdx is Idx+1,
    ( nth0(PIdx, M, PRow) -> true
    ; PRow = []),
    ( nth0(NIdx, M, NRow) -> true
    ; NRow = []),
    constraints_row(Row, PRow, NRow, 0, LengthJ),
    constraints(M, NIdx, LengthI, LengthJ).

% creates constraints for a row of the puzzle
% parameters: row, previous row, nex row and matrix dimensions
constraints_row(_Row, _PRow, _NRow, LengthJ, LengthJ) :- !.
constraints_row(Row, PRow, NRow, Idx, LengthJ) :-
    nth0(Idx, Row, Cell),
    ( nth0(Idx, PRow, Up) -> UNeigh = [Up]
    ; UNeigh = []),
    ( PIdx is Idx-1, nth0(PIdx, Row, Left) -> LUNeigh = [Left|UNeigh]
    ; LUNeigh = UNeigh),
    ( nth0(Idx, NRow, Down) -> DLUNeigh = [Down|LUNeigh]
    ; DLUNeigh = LUNeigh),
    ( NIdx is Idx+1, nth0(NIdx, Row, Right) -> RDLUNeigh = [Right|DLUNeigh]
    ; RDLUNeigh = DLUNeigh),
    constraints_cell(Cell, RDLUNeigh),
    NIdx is Idx+1,
    constraints_row(Row, PRow, NRow, NIdx, LengthJ).

% posts a constraint for one cell (restricting the count of neighboring cells containing the same number)
constraints_cell(Cell, Neighbors) :-
    make_constlist(Cell, Neighbors, ConstList),
    /*filled cells*/ ( nonvar(Cell) -> sum(ConstList, #=, 1)
    /*empty cells */ ; sum(ConstList, #=, 2))
    .

% creates a list with reified constraints assuring that the cell's number matches the neighbors' number
make_constlist(_, [], []).
make_constlist(Cell, [HN|TN], [Const|TC]) :-
    Cell#=HN #<=> Const,
    make_constlist(Cell, TN, TC).

% prints a matrix
write_m([]).
write_m([H|T]) :-
    write(H),
    nl,
    write_m(T).
