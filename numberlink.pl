:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(between)).

% M is the solution of the puzzle that can be found under Filename, Time is the time required to solve
numberlink_f(Filename, M, Time) :-
    see(Filename),
    read(M),
    seen,
    statistics(walltime, [Start,_]),
    numberlink(M),
    statistics(walltime, [Stop,_]),
    Time is Stop-Start.

% solves puzzle M (fills all free variables)
numberlink(M) :-
    append(M, MFlat),
    max_member(Max, MFlat),
    domain(MFlat, 1, Max),
    apply_constraints(M),
    labeling([], MFlat).

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
