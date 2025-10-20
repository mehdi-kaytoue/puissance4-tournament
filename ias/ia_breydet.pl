:- dynamic board/1.

displayBoard:- 
    board([A,B,C,D,E,F,G,
    A1,B1,C1,D1,E1,F1,G1,
    A2,B2,C2,D2,E2,F2,G2,
    A3,B3,C3,D3,E3,F3,G3,
    A4,B4,C4,D4,E4,F4,G4,
    A5,B5,C5,D5,E5,F5,G5]),
    format('|~w|~w|~w|~w|~w|~w|~w|',[A, B, C, D, E, F, G]), nl,
    format('|~w|~w|~w|~w|~w|~w|~w|',[A1, B1, C1, D1, E1, F1, G1]), nl,
    format('|~w|~w|~w|~w|~w|~w|~w|',[A2, B2, C2, D2, E2, F2, G2]), nl,
    format('|~w|~w|~w|~w|~w|~w|~w|',[A3, B3, C3, D3, E3, F3, G3]), nl,
    format('|~w|~w|~w|~w|~w|~w|~w|',[A4, B4, C4, D4, E4, F4, G4]), nl,
    format('|~w|~w|~w|~w|~w|~w|~w|',[A5, B5, C5, D5, E5, F5, G5]), nl.+

index(Row,Col,Idx) :-
    Idx is Row*7 + Col + 1.

get_cell(Board, Row, Col, Val) :-
    index(Row, Col, Idx), nth1(Idx, Board, Val).

drop_in_column(Board, Column, Player, NewBoard, PlacedIdx) :-
    between(1,7,Column),
    Col0 is Column - 1,
    findall(R, (between(0,5,R), index(R,Col0,I), nth1(I,Board,' ')), Rs),
    Rs \= [],                                
    last(Rs, RowPlaced),                    
    index(RowPlaced, Col0, PlacedIdx),
    nth1(PlacedIdx, Board, ' '),             
    nth1(PlacedIdx, NewBoard, Player, Rest), 
    nth1(PlacedIdx, Board, ' ', Rest).       

is_column_full(Board, Column) :-
    Col0 is Column - 1,
    index(0, Col0, Idx),
    nth1(Idx, Board, V),
    V \= ' '.

next_move(Board, Player, Column) :-
    between(1,7,Column),
    \+ is_column_full(Board, Column),
    drop_in_column(Board, Column, Player, NewBoard, _),
    four_in_a_row(NewBoard, Player), !.

four_in_a_row(Board, P) :-
    ( between(0,5,R), between(0,3,C), check_seq(Board, R, C, 0, 1, P) );
    ( between(0,2,R), between(0,6,C), check_seq(Board, R, C, 1, 0, P) );
    ( between(0,2,R), between(0,3,C), check_seq(Board, R, C, 1, 1, P) );
    ( between(3,5,R), between(0,3,C), check_seq(Board, R, C, -1, 1, P) ),
    P \= ' '.

check_seq(Board, R, C, DR, DC, P) :-
    R0 is R, C0 is C,
    index(R0, C0, I0), nth1(I0, Board, P),
    R1 is R0 + DR, C1 is C0 + DC, index(R1, C1, I1), nth1(I1, Board, P),
    R2 is R1 + DR, C2 is C1 + DC, index(R2, C2, I2), nth1(I2, Board, P),
    R3 is R2 + DR, C3 is C2 + DC, index(R3, C3, I3), nth1(I3, Board, P).

win(Board) :-
    four_in_a_row(Board, P),
    P \= ' ',
    format('Le joueur ~w a gagne !~n', [P]).

draw(Board) :-
    \+ (four_in_a_row(Board, _)),
    \+ member(' ', Board),
    writeln('Egalite !').

ia(Board, Column, Player) :-
    changePlayer(Player, Opponent),
    (   next_move(Board, Player, Column) -> true
    ;   next_move(Board, Opponent, Column) -> true
    ;
    findall(C, (between(1,7,C), \+ is_column_full(Board,C)), ValidCols),
    ValidCols \= [],
    length(ValidCols, N),
    random_between(1, N, R),
    nth1(R, ValidCols, Column)
    ) .

ia_def(Board, Column, Player) :-
    changePlayer(Player, Opponent),
    (   next_move(Board, Opponent, Column) -> true
    ;
    findall(C, (between(1,7,C), \+ is_column_full(Board,C)), ValidCols),
    ValidCols \= [],
    length(ValidCols, N),
    random_between(1, N, R),
    nth1(R, ValidCols, Column)
    ) .

ia_att(Board, Column, Player) :-
    (   next_move(Board, Player, Column) -> true
    ;
    findall(C, (between(1,7,C), \+ is_column_full(Board,C)), ValidCols),
    ValidCols \= [],
    length(ValidCols, N),
    random_between(1, N, R),
    nth1(R, ValidCols, Column)
    ) .

playMove(Board,Column,NewBoard,Player) :-
    drop_in_column(Board, Column, Player, NewBoard, _).

applyIt(Board, NewBoard):- 
    retract(board(Board)),
    assert(board(NewBoard)).

changePlayer('x','o').
changePlayer('o','x').

play(Player) :-
    write('New turn for: '), writeln(Player),
    board(Board),
    displayBoard,
    (   Player == 'x' ->
        prompt_move(Board, Column),
        playMove(Board, Column, NewBoard, Player)
    ;
        ia(Board, Move, Player),
        playMove(Board, Move, NewBoard, Player)
    ),
    applyIt(Board, NewBoard),
    (   win(NewBoard) -> displayBoard, ask_restart
    ;   draw(NewBoard) -> displayBoard, ask_restart
    ;   changePlayer(Player, Next),
        play(Next)
    ).

prompt_move(Board, Column) :-
    repeat,
        write('Entrez une colonne (1..7) pour jouer (ou "q" pour quitter) : '), flush_output,
        read_line_to_string(user_input, S),
        ( S = "q" -> writeln('Quit.'), abort ; true ),
        ( catch(number_string(N, S), _, fail) ->
            ( integer(N), between(1,7,N), \+ is_column_full(Board,N) ->
                Column = N, ! 
            ; writeln('Colonne invalide ou pleine. Reessayer.'), fail
            )
        ; writeln('Entree non numérique. Réessayer.'), fail
        ).

reset :-
    retractall(board(_)).

ask_restart :-
    write('Recommencer une nouvelle partie ? (y/n) : '), flush_output,
    read_line_to_string(user_input, S),
    ( member(S, ["y","Y"]) ->
        reset,
        init
    ; writeln('Fin.')
    ).

init :-
    retractall(board(_)),
    length(Board,42), maplist(=(' '), Board), assert(board(Board)),
    play('x').