 
% engine.pl
% Représentation : Board est une liste de 6 lignes, chaque ligne est une liste de 7 entiers (0 vide, 1, 2)
% le bas est la ligne 5, le haut la ligne 0

:- module(engine, [valid_moves/2, apply_move/4, winner/2, display_board/1]).

% valid_moves(+Board, -Cols)
valid_moves(Board, Cols) :-
    nth0(0, Board, TopRow),
    findall(C, (nth0(C, TopRow, V), V == 0), Cols). %findall poru collecter les réponses d'un but

% apply_move(+Board, +Col, +Player, -NewBoard)
apply_move(Board, Col, Player, NewBoard) :-
    % on joue le coup dans la colonne Col sur le premier endroit libre
    transpose(Board, TBoard),
    nth0(Col, TBoard, ColList),
    drop_into_column(ColList, Player, NewColList),
    nth0(Col, TBoard, _, Rest),
    nth0(Col, NewTBoard, NewColList, Rest),
    transpose(NewBoard, NewTBoard), !.

drop_into_column(ColList, Player, NewColList) :-
    reverse(ColList, Rev),
    ( select(0, Rev, 0, After) -> % find first zero from bottom
        select(0, After, Player, RevNew),
        reverse(RevNew, NewColList)
    ;
        fail
    ).

% winner(+Board, -Winner)
winner(Board, Winner) :-
    ( check_win(Board, 1) -> Winner = 1
    ; check_win(Board, 2) -> Winner = 2
    ; Winner = 0 ).

% check_win(B, P) true if P has 4 in a row
check_win(Board, P) :-
    between(0,5,R), between(0,6,C), nth0(R, Board, Row), nth0(C, Row, P),
    ( horizontal(Board,R,C,P)
    ; vertical(Board,R,C,P)
    ; diag1(Board,R,C,P)
    ; diag2(Board,R,C,P)
    ), !.

horizontal(Board,R,C,P) :-
    C2 is C+1, C3 is C+2, C4 is C+3,
    C4 =< 6,
    nth0(R, Board, Row), nth0(C2, Row, P), nth0(C3, Row, P), nth0(C4, Row, P).

vertical(Board,R,C,P) :-
    R2 is R+1, R3 is R+2, R4 is R+3,
    R4 =< 5,
    nth0(R2, Board, Row2), nth0(R3, Board, Row3), nth0(R4, Board, Row4),
    nth0(C, Row2, P), nth0(C, Row3, P), nth0(C, Row4, P).

diag1(Board,R,C,P) :- % down-right
    R2 is R+1, R3 is R+2, R4 is R+3,
    C2 is C+1, C3 is C+2, C4 is C+3,
    R4 =< 5, C4 =< 6,
    nth0(R2, Board, Row2), nth0(R3, Board, Row3), nth0(R4, Board, Row4),
    nth0(C2, Row2, P), nth0(C3, Row3, P), nth0(C4, Row4, P).

diag2(Board,R,C,P) :- % down-left
    R2 is R+1, R3 is R+2, R4 is R+3,
    C2 is C-1, C3 is C-2, C4 is C-3,
    R4 =< 5, C4 >= 0,
    nth0(R2, Board, Row2), nth0(R3, Board, Row3), nth0(R4, Board, Row4),
    nth0(C2, Row2, P), nth0(C3, Row3, P), nth0(C4, Row4, P).

% display board (for debugging)
display_board(Board) :-
    forall(member(Row, Board), (write(Row), nl)).

% transpose helper
transpose([[]|_], []) :- !.
transpose(Matrix, [Row|Rows]) :-
    maplist(nth0(0), Matrix, Row),
    maplist(remove_head, Matrix, Rest),
    transpose(Rest, Rows).
remove_head([_|T], T).

