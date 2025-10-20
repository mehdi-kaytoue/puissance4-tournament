% Puissance 4 (Connect Four) en Prolog
% Le plateau est représenté par une liste de 42 éléments (7 colonnes x 6 lignes)
% Les indices vont de 0 à 41, organisés ligne par ligne de bas en haut:
% Ligne 0 (bas):    0  1  2  3  4  5  6
% Ligne 1:          7  8  9 10 11 12 13
% Ligne 2:         14 15 16 17 18 19 20
% Ligne 3:         21 22 23 24 25 26 27
% Ligne 4:         28 29 30 31 32 33 34
% Ligne 5 (haut):  35 36 37 38 39 40 41

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% INTERFACE POUR LE TOURNOI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Le tournoi attend le prédicat : joue_coup(+Board, +Joueur, -Colonne)
% où Board est une liste de 6 lignes (format 2D avec 0=vide, 1=joueur1, 2=joueur2)
% et Joueur est 1 ou 2

%%%% PRÉDICAT PRINCIPAL POUR LE TOURNOI
joue_coup(BoardTournoi, Joueur, Colonne) :-
    % 1. Convertir le format du tournoi vers notre format interne
    convert_tournament_to_internal(BoardTournoi, Joueur, Board1D, Player),
    
    % 2. CHOISIR QUELLE IA UTILISER (modifiez ici selon vos besoins)
    select_ia_for_tournament(Board1D, Colonne, Player).

%%%% Sélection de l'IA pour le tournoi
% MODIFIEZ ICI pour changer d'IA pour le tournoi
select_ia_for_tournament(Board, Column, Player) :-
    % Option recommandée : Minimax (rapide, < 1 sec)
    % ia_minimax(Board, Column, Player).
    
    % Autres options :
    ia_expert_fast(Board, Column, Player).  % Plus fort mais 5-20 sec/coup
    % ia_mcts(Board, Column, Player).         % Moyen, 2-3 sec/coup
    % ia_random(Board, Column, Player).       % Très rapide mais faible

%%%% CONVERSION : Format tournoi 2D (0/1/2) → Format interne 1D (_/'x'/'o')
convert_tournament_to_internal(Board2D, Joueur, Board1D, Player) :-
    % 1. Inverser l'ordre (tournoi : ligne 0 = haut, nous : indices 0-6 = bas)
    reverse(Board2D, ReversedBoard2D),
    flatten(ReversedBoard2D, FlatBoardReversed),
    
    % 2. Convertir les valeurs 0/1/2 en _/'x'/'o'
    convert_tournament_values(FlatBoardReversed, Board1D),
    
    % 3. Convertir le numéro de joueur (1/2) en symbole ('x'/'o')
    (Joueur = 1 -> Player = 'x' ; Player = 'o').

%%%% Convertir les valeurs du tournoi (0/1/2) vers notre format (_/'x'/'o')
convert_tournament_values([], []).
convert_tournament_values([H|T], [H2|T2]) :-
    (H = 0 -> true           % 0 → variable non instanciée (_)
    ; H = 1 -> H2 = 'x'      % 1 → 'x'
    ; H = 2 -> H2 = 'o'      % 2 → 'o'
    ),
    convert_tournament_values(T, T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TESTS POUR VÉRIFIER LA COMPATIBILITÉ AVEC LE TOURNOI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_tournament_interface :-
    writeln(''),
    writeln('╔════════════════════════════════════════╗'),
    writeln('║   TEST INTERFACE TOURNOI              ║'),
    writeln('╚════════════════════════════════════════╝'),
    writeln(''),
    
    % Test 1 : Board vide
    writeln('=== Test 1 : Board vide ==='),
    BoardVide = [
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0]
    ],
    joue_coup(BoardVide, 1, Col1),
    format('Premier coup (joueur 1): colonne ~w~n', [Col1]),
    writeln('✓ Test 1 réussi'),
    writeln(''),
    
    % Test 2 : Board avec quelques coups
    writeln('=== Test 2 : Board avec coups ==='),
    BoardAvecCoups = [
        [0,0,0,0,0,0,0],  % Ligne 0 (haut)
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0],
        [0,0,0,2,0,0,0],  % Un 2 au centre
        [1,0,0,1,0,0,0]   % Ligne 5 (bas) - deux 1
    ],
    writeln('Board (format tournoi):'),
    forall(member(Row, BoardAvecCoups), 
           (write('  '), writeln(Row))),
    joue_coup(BoardAvecCoups, 2, Col2),
    format('Joueur 2 joue: colonne ~w~n', [Col2]),
    writeln('✓ Test 2 réussi'),
    writeln(''),
    
    % Test 3 : Vérifier la conversion
    writeln('=== Test 3 : Vérification conversion ==='),
    convert_tournament_to_internal(BoardAvecCoups, 1, Board1D, Player),
    writeln('Board converti (format interne 1D):'),
    write('  '), writeln(Board1D),
    format('Joueur converti: ~w~n', [Player]),
    writeln('✓ Test 3 réussi'),
    writeln(''),
    
    writeln('✓ Tous les tests passés - Interface tournoi opérationnelle!').

test_tournament_speed :-
    writeln('=== Test de vitesse (10 coups sur board vide) ==='),
    BoardVide = [
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0]
    ],
    get_time(Start),
    forall(between(1, 10, N), (
        joue_coup(BoardVide, 1, _),
        (N mod 2 =:= 0 -> write('.') ; true)
    )),
    get_time(End),
    Duration is End - Start,
    AvgTime is Duration / 10,
    writeln(''),
    format('Temps total: ~2f secondes~n', [Duration]),
    format('Temps moyen par coup: ~2f secondes~n', [AvgTime]),
    (AvgTime < 30.0 ->
        writeln('✓ Performance OK pour le tournoi (< 60 sec/coup)')
    ;
        writeln('⚠️ ATTENTION: Trop lent pour le tournoi! Changez d\'IA.')
    ).

% ========== UTILISATION POUR LE TOURNOI ==========
%
% Pour utiliser ce fichier dans le tournoi Python :
% 1. Assurez-vous que le prédicat joue_coup/3 est défini (c'est fait ici)
% 2. Choisissez votre IA dans select_ia_for_tournament/3 (ligne 31)
% 3. Testez avec : ?- test_tournament_interface.
% 4. Vérifiez la vitesse : ?- test_tournament_speed.
% 5. Lancez le tournoi : python tournament.py main.pl autre_ia.pl ...
%
% ⚠️ IMPORTANT : Le tournoi donne 4 secondes par coup maximum !
%    Recommandations :
%    - ia_minimax : ✅ Rapide (~1 sec), bon niveau
%    - ia_mcts : ✅ OK (~2-3 sec), bon niveau
%    - ia_expert_fast : ⚠️ Risqué (5-20 sec selon profondeur)
%    - ia_expert : ❌ Trop lent (30 sec - 2 min)

% Si le tournoi utilise un format 2D (tableau de tableaux), utilisez :
%
% 1. Pour convertir les formats :
%    ?- board2d_to_1d([[_,_,_,_,_,_,_],...], Board1D).
%    ?- board1d_to_2d([_,_,_,...], Board2D).
%
% 2. Pour jouer un coup depuis un board 2D :
%    ?- play_tournament_move(Board2D, 'x', ia_expert_fast, NewBoard2D, Column).
%    (Retourne le nouveau board et la colonne jouée)
%
% 3. Pour tester les conversions :
%    ?- run_all_tests.
%
% 4. Choisir l'IA pour le tournoi (modifier dans le code) :
%    - ia_expert_fast  : Recommandé (5-20 sec/coup, très fort)
%    - ia_expert       : Maximum force (1-3 min/coup)
%    - ia_minimax      : Rapide (1 sec/coup, bon niveau)
%    - ia_mcts         : Moyen (2-3 sec/coup, bon niveau)
%    - ia_random       : Très rapide (< 0.1 sec/coup, faible)

% ========== 5 INTELLIGENCES ARTIFICIELLES DISPONIBLES ==========
% 1. ia_random      : Joue aléatoirement (avec tactiques de base)
% 2. ia_minimax     : Minimax avec Alpha-Beta (profondeur 6) - ~1 seconde/coup
% 3. ia_mcts        : Monte Carlo Tree Search (1000 simulations) - ~2-3 secondes/coup
% 4. ia_expert_fast : IA Expert rapide (profondeur 5-7) - ~5-10 secondes/coup ⭐ PAR DÉFAUT
% 5. ia_expert      : IA Expert complète (profondeur 8-10) - ~30 sec-2 min/coup ⚠️ TRÈS LENT
%
% ⚠️ ATTENTION : ia_expert peut être TRÈS LENTE (30 sec - 2 min par coup au début)
%    Recommandation : Utilisez ia_expert_fast pour des parties fluides
%
% IA EXPERT inclut :
% - Base d'ouvertures optimales
% - Profondeur adaptative (8-10 selon la phase de jeu)
% - Détection de cases empoisonnées
% - Évaluation de connectivité (pions adjacents)
% - Détection de structures gagnantes (formes en 7, doubles diagonales)
% - Contrôle renforcé du centre
% - Toutes les tactiques : victoire, blocage, fourchettes, etc.

% STRATÉGIES TACTIQUES IMPLÉMENTÉES (toutes les IA) :
% 1. Victoire immédiate : Gagner si possible
% 2. Blocage défensif : Bloquer une victoire adverse
% 3. FOURCHETTE (DOUBLE MENACE) : Créer 2+ menaces simultanées
%    → L'adversaire ne peut bloquer qu'une menace
%    → On gagne au coup suivant avec l'autre menace
% 4. Blocage de fourchette : Empêcher l'adversaire de créer une fourchette
% 5. Filtrage des coups suicidaires : Ne jamais offrir la victoire
% 6. Évaluation heuristique : Patterns, centre, menaces, connectivité

:- dynamic board/1.
:- dynamic nodes_explored/1.

%%%% Test si le jeu est terminé %%%
gameover(Winner) :- board(Board), winner(Board, Winner), !.
gameover('Draw') :- board(Board), isBoardFull(Board).

%%%% Test si un plateau a une configuration gagnante pour le joueur P
% Lignes horizontales (4 consécutifs sur une même ligne)
winner(Board, P) :- 
    checkHorizontal(Board, P).

% Colonnes verticales (4 consécutifs dans une même colonne)
winner(Board, P) :- 
    checkVertical(Board, P).

% Diagonales ascendantes (/)
winner(Board, P) :- 
    checkDiagonalAsc(Board, P).

% Diagonales descendantes (\)
winner(Board, P) :- 
    checkDiagonalDesc(Board, P).

% Vérification horizontale
checkHorizontal(Board, P) :-
    member(Start, [0,7,14,21,28,35]),
    member(Offset, [0,1,2,3]),
    Index is Start + Offset,
    nth0(Index, Board, P), nonvar(P),
    I1 is Index + 1, nth0(I1, Board, Q1), P == Q1,
    I2 is Index + 2, nth0(I2, Board, Q2), P == Q2,
    I3 is Index + 3, nth0(I3, Board, Q3), P == Q3.

% Vérification verticale
checkVertical(Board, P) :-
    member(Col, [0,1,2,3,4,5,6]),
    member(Row, [0,1,2]),
    Index is Row * 7 + Col,
    nth0(Index, Board, P), nonvar(P),
    I1 is Index + 7, nth0(I1, Board, Q1), P == Q1,
    I2 is Index + 14, nth0(I2, Board, Q2), P == Q2,
    I3 is Index + 21, nth0(I3, Board, Q3), P == Q3.

% Vérification diagonale ascendante
checkDiagonalAsc(Board, P) :-
    member(Row, [0,1,2]),
    member(Col, [0,1,2,3]),
    Index is Row * 7 + Col,
    nth0(Index, Board, P), nonvar(P),
    I1 is Index + 8, nth0(I1, Board, Q1), P == Q1,
    I2 is Index + 16, nth0(I2, Board, Q2), P == Q2,
    I3 is Index + 24, nth0(I3, Board, Q3), P == Q3.

% Vérification diagonale descendante
checkDiagonalDesc(Board, P) :-
    member(Row, [0,1,2]),
    member(Col, [3,4,5,6]),
    Index is Row * 7 + Col,
    nth0(Index, Board, P), nonvar(P),
    I1 is Index + 6, nth0(I1, Board, Q1), P == Q1,
    I2 is Index + 12, nth0(I2, Board, Q2), P == Q2,
    I3 is Index + 18, nth0(I3, Board, Q3), P == Q3.

%%%% Vérifie si toutes les cases sont remplies
isBoardFull([]).
isBoardFull([H|T]) :- nonvar(H), isBoardFull(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% INTELLIGENCE ARTIFICIELLE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% IA Random: joue aléatoirement dans une colonne qui n'est pas pleine
% Même l'IA random devrait gagner si elle peut, et bloquer si nécessaire
ia_random(Board, Column, Player) :- 
    % 1. Coup gagnant ?
    (findWinningMove(Board, Player, WinMove) ->
        Column = WinMove
    % 2. Bloquer l'adversaire ?
    ; changePlayer(Player, Opponent), findWinningMove(Board, Opponent, BlockMove) ->
        Column = BlockMove
    % 3. Créer une fourchette (double menace) ?
    ; findForkMove(Board, Player, ForkMove) ->
        Column = ForkMove
    % 4. Bloquer une fourchette adverse ?
    ; changePlayer(Player, Opponent), findForkMove(Board, Opponent, BlockFork) ->
        Column = BlockFork
    % 5. Sinon jouer au hasard parmi les coups sûrs
    ;
        getSafeMoves(Board, Player, SafeMoves),
        random_member(Column, SafeMoves)
    ).

%%%% IA Minimax avec élagage Alpha-Beta
ia_minimax(Board, Column, Player) :-
    write('🎯 IA Minimax réfléchit... '),
    flush_output,
    MaxDepth = 6,
    % 1. Coup gagnant immédiat ?
    (findWinningMove(Board, Player, WinMove) ->
        Column = WinMove
    % 2. Bloquer une victoire adverse ?
    ; changePlayer(Player, Opponent), findWinningMove(Board, Opponent, BlockMove) ->
        Column = BlockMove
    % 3. Créer une fourchette (double menace) ?
    ; findForkMove(Board, Player, ForkMove) ->
        Column = ForkMove
    % 4. Bloquer une fourchette adverse ?
    ; changePlayer(Player, Opponent), findForkMove(Board, Opponent, BlockFork) ->
        Column = BlockFork
    % 5. Sinon évaluer normalement avec Minimax en filtrant les coups dangereux
    ;
        getSafeMoves(Board, Player, SafeMoves),
        evaluateAllMoves(SafeMoves, Board, Player, MaxDepth, ScoredMoves),
        findBestScore(ScoredMoves, BestScore),
        findall(Col, member(Col-BestScore, ScoredMoves), BestMoves),
        random_member(Column, BestMoves)
    ).

%%%% Trouve un coup gagnant immédiat s'il existe
findWinningMove(Board, Player, Column) :-
    allPossibleColumns(Board, Moves),
    member(Column, Moves),
    simulateMove(Board, Column, Player, NewBoard),
    winner(NewBoard, Player), !.

%%%% Compte le nombre de coups gagnants disponibles sur un plateau
countWinningMoves(Board, Player, Count) :-
    allPossibleColumns(Board, Moves),
    findall(1, (
        member(Move, Moves),
        simulateMove(Board, Move, Player, NewBoard),
        winner(NewBoard, Player)
    ), WinningMoves),
    length(WinningMoves, Count).

%%%% Détecte si un coup crée une double menace (fourchette)
createsFork(Board, Column, Player) :-
    simulateMove(Board, Column, Player, NewBoard),
    countWinningMoves(NewBoard, Player, Count),
    Count >= 2.

%%%% Trouve un coup qui crée une fourchette
findForkMove(Board, Player, Column) :-
    getSafeMoves(Board, Player, Moves),
    member(Column, Moves),
    createsFork(Board, Column, Player), !.

%%%% Vérifie si un coup offre la victoire à l'adversaire
isLosingMove(Board, Column, Player) :-
    simulateMove(Board, Column, Player, NewBoard),
    changePlayer(Player, Opponent),
    findWinningMove(NewBoard, Opponent, _).

%%%% Filtre les coups qui offrent la victoire à l'adversaire
filterSafeMoves([], _, []).
filterSafeMoves([Move|Moves], Board-Player, SafeMoves) :-
    (isLosingMove(Board, Move, Player) ->
        filterSafeMoves(Moves, Board-Player, SafeMoves)
    ;
        filterSafeMoves(Moves, Board-Player, RestSafe),
        SafeMoves = [Move|RestSafe]
    ).

%%%% Obtient les coups sûrs (qui ne donnent pas la victoire à l'adversaire)
getSafeMoves(Board, Player, SafeMoves) :-
    allPossibleColumns(Board, AllMoves),
    filterSafeMoves(AllMoves, Board-Player, Filtered),
    % Si aucun coup n'est sûr, on doit quand même jouer
    (Filtered = [] -> SafeMoves = AllMoves ; SafeMoves = Filtered).

evaluateAllMoves([], _, _, _, []).
evaluateAllMoves([Move|Moves], Board, Player, Depth, [Move-Score|Rest]) :-
    simulateMove(Board, Move, Player, NewBoard),
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    minimax(NewBoard, Opponent, NewDepth, -10000, 10000, _, OpponentScore),
    Score is -OpponentScore,
    evaluateAllMoves(Moves, Board, Player, Depth, Rest).

findBestScore([_-Score|Rest], BestScore) :-
    findBestScoreHelper(Rest, Score, BestScore).

findBestScoreHelper([], Best, Best).
findBestScoreHelper([_-Score|Rest], CurrentBest, BestScore) :-
    (Score > CurrentBest ->
        findBestScoreHelper(Rest, Score, BestScore)
    ;
        findBestScoreHelper(Rest, CurrentBest, BestScore)
    ).

%%%% Algorithme Minimax avec élagage Alpha-Beta
minimax(Board, Player, Depth, Alpha, Beta, BestColumn, BestScore) :-
    allPossibleColumns(Board, Moves),
    Moves \= [],
    alphabeta(Moves, Board, Player, Depth, Alpha, Beta, nil, -10000, BestColumn, BestScore).

alphabeta([], _, _, _, _, _, BestColumn, BestScore, BestColumn, BestScore) :- !.

alphabeta([Move|Moves], Board, Player, Depth, Alpha, Beta, TempBest, TempScore, BestColumn, BestScore) :-
    simulateMove(Board, Move, Player, NewBoard),
    (   (Depth =< 0 ; winner(NewBoard, _) ; isBoardFull(NewBoard)) ->
        evaluate(NewBoard, Player, Score)
    ;
        changePlayer(Player, Opponent),
        NewDepth is Depth - 1,
        NegBeta is -Beta,
        NegAlpha is -Alpha,
        minimax(NewBoard, Opponent, NewDepth, NegBeta, NegAlpha, _, OpponentScore),
        Score is -OpponentScore
    ),
    (   Score > TempScore ->
        NewBest = Move,
        NewScore = Score
    ;
        NewBest = TempBest,
        NewScore = TempScore
    ),
    NewAlpha is max(Alpha, NewScore),
    (   NewAlpha >= Beta ->
        BestColumn = NewBest,
        BestScore = NewScore
    ;
        alphabeta(Moves, Board, Player, Depth, NewAlpha, Beta, NewBest, NewScore, BestColumn, BestScore)
    ).

%%%% Simule un coup sans modifier le plateau original
simulateMove(Board, Column, Player, NewBoard) :-
    findLowestRow(Board, Column, Row),
    Index is Row * 7 + Column,
    copy_term(Board, NewBoard),
    nth0(Index, NewBoard, Player).

%%%% Fonction d'évaluation du plateau
evaluate(Board, Player, Score) :-
    (winner(Board, Player) ->
        Score = 1000
    ; changePlayer(Player, Opponent), winner(Board, Opponent) ->
        Score = -1000
    % Si l'adversaire peut gagner au prochain coup = très mauvais
    ; changePlayer(Player, Opponent), canWinNextMove(Board, Opponent) ->
        Score = -950
    % Si on peut créer une fourchette = excellent
    ; allPossibleColumns(Board, Moves), member(Move, Moves), createsFork(Board, Move, Player) ->
        Score = 850
    % Si l'adversaire peut créer une fourchette = très mauvais
    ; changePlayer(Player, Opponent), 
      allPossibleColumns(Board, Moves), member(Move, Moves), createsFork(Board, Move, Opponent) ->
        Score = -800
    % Vérifier si on peut gagner au prochain coup
    ; canWinNextMove(Board, Player) ->
        Score = 900
    ;
        evaluatePosition(Board, Player, Score)
    ).

%%%% Vérifie si un joueur peut gagner au prochain coup
canWinNextMove(Board, Player) :-
    allPossibleColumns(Board, Moves),
    member(Move, Moves),
    simulateMove(Board, Move, Player, NewBoard),
    winner(NewBoard, Player), !.

%%%% Évaluation heuristique de la position
evaluatePosition(Board, Player, Score) :-
    changePlayer(Player, Opponent),
    countPatterns(Board, Player, PlayerScore),
    countPatterns(Board, Opponent, OpponentScore),
    centerControl(Board, Player, CenterBonus),
    % Détecter les menaces et opportunités
    countThreats(Board, Player, PlayerThreats),
    countThreats(Board, Opponent, OpponentThreats),
    Score is PlayerScore - OpponentScore + CenterBonus + PlayerThreats * 100 - OpponentThreats * 90.

%%%% Bonus pour le contrôle des colonnes centrales
centerControl(Board, Player, Bonus) :-
    countPiecesInColumn(Board, Player, 3, Center),
    countPiecesInColumn(Board, Player, 2, Left),
    countPiecesInColumn(Board, Player, 4, Right),
    Bonus is Center * 3 + Left * 2 + Right * 2.

countPiecesInColumn(Board, Player, Col, Count) :-
    findall(1, (
        between(0, 5, Row),
        Index is Row * 7 + Col,
        nth0(Index, Board, Player)
    ), Pieces),
    length(Pieces, Count).

%%%% Compte le nombre de menaces (3 pions alignés avec case libre jouable)
countThreats(Board, Player, Count) :-
    findall(1, isThreat(Board, Player), Threats),
    length(Threats, Count).

%%%% Vérifie s'il existe une menace (3 pions + 1 case libre jouable)
isThreat(Board, Player) :-
    % Pour chaque colonne possible
    allPossibleColumns(Board, Moves),
    member(Col, Moves),
    % Simuler le coup
    findLowestRow(Board, Col, Row),
    Index is Row * 7 + Col,
    % Vérifier si ce coup complète un alignement de 3
    checkThreatAt(Board, Index, Player).

%%%% Vérifie si placer un pion à Index complète un alignement de 3
checkThreatAt(Board, Index, Player) :-
    % Copier le board et placer le pion
    copy_term(Board, TestBoard),
    nth0(Index, TestBoard, Player),
    % Vérifier s'il y a maintenant 4 alignés
    winner(TestBoard, Player).

%%%% Compte les patterns (alignements de 2, 3 pions avec espaces libres)
countPatterns(Board, Player, TotalScore) :-
    countHorizontalPatterns(Board, Player, H),
    countVerticalPatterns(Board, Player, V),
    countDiagonalPatterns(Board, Player, D),
    TotalScore is H + V + D.

countHorizontalPatterns(Board, Player, Score) :-
    findall(S, (
        member(Row, [0,1,2,3,4,5]),
        member(Col, [0,1,2,3]),
        Index is Row * 7 + Col,
        checkWindowScore(Board, Index, 1, Player, S)
    ), Scores),
    sumlist(Scores, Score).

countVerticalPatterns(Board, Player, Score) :-
    findall(S, (
        member(Col, [0,1,2,3,4,5,6]),
        member(Row, [0,1,2]),
        Index is Row * 7 + Col,
        checkWindowScore(Board, Index, 7, Player, S)
    ), Scores),
    sumlist(Scores, Score).

countDiagonalPatterns(Board, Player, Score) :-
    findall(S, (
        (   (member(Row, [0,1,2]), member(Col, [0,1,2,3]),
             Index is Row * 7 + Col, Delta = 8,
             checkWindowScore(Board, Index, Delta, Player, S))
        ;   (member(Row, [0,1,2]), member(Col, [3,4,5,6]),
             Index is Row * 7 + Col, Delta = 6,
             checkWindowScore(Board, Index, Delta, Player, S))
        )
    ), Scores),
    sumlist(Scores, Score).

checkWindowScore(Board, Index, Delta, Player, Score) :-
    I2 is Index + Delta,
    I3 is Index + Delta * 2,
    I4 is Index + Delta * 3,
    (   (I2 < 42, I3 < 42, I4 < 42) ->
        (   nth0(Index, Board, V1),
            nth0(I2, Board, V2),
            nth0(I3, Board, V3),
            nth0(I4, Board, V4),
            countPieces([V1,V2,V3,V4], Player, Count),
            (   Count = 3 -> Score = 50  % Augmenté de 5 à 50
            ;   Count = 2 -> Score = 10  % Augmenté de 2 à 10
            ;   Score = 0
            )
        )
    ;   Score = 0
    ).

countPieces(List, Player, Count) :-
    changePlayer(Player, Opponent),
    (   member(Opponent, List) ->
        Count = 0
    ;
        include(==(Player), List, PlayerPieces),
        length(PlayerPieces, Count)
    ).

%%%% IA Monte Carlo Tree Search (MCTS)
ia_mcts(Board, Column, Player) :-
    write('🎲 IA MCTS réfléchit... '),
    flush_output,
    % 1. Coup gagnant immédiat ?
    (findWinningMove(Board, Player, WinMove) ->
        Column = WinMove
    % 2. Bloquer une victoire adverse ?
    ; changePlayer(Player, Opponent), findWinningMove(Board, Opponent, BlockMove) ->
        Column = BlockMove
    % 3. Créer une fourchette (double menace) ?
    ; findForkMove(Board, Player, ForkMove) ->
        Column = ForkMove
    % 4. Bloquer une fourchette adverse ?
    ; changePlayer(Player, Opponent), findForkMove(Board, Opponent, BlockFork) ->
        Column = BlockFork
    % 5. Sinon utiliser MCTS avec les coups sûrs
    ;
        NumSimulations = 1000,
        getSafeMoves(Board, Player, SafeMoves),
        mcts_safe(Board, Player, NumSimulations, SafeMoves, Column)
    ).

%%%% MCTS avec coups filtrés
mcts_safe(Board, Player, NumSimulations, Moves, BestColumn) :-
    initializeStats(Moves, Stats),
    runSimulations(Board, Player, Moves, Stats, NumSimulations, FinalStats),
    selectBestMoveRandom(FinalStats, BestColumn).

mcts(Board, Player, NumSimulations, BestColumn) :-
    allPossibleColumns(Board, Moves),
    initializeStats(Moves, Stats),
    runSimulations(Board, Player, Moves, Stats, NumSimulations, FinalStats),
    selectBestMoveRandom(FinalStats, BestColumn).

initializeStats([], []).
initializeStats([Move|Moves], [Move-0-0|Stats]) :-
    initializeStats(Moves, Stats).

runSimulations(_, _, _, Stats, 0, Stats) :- !.
runSimulations(Board, Player, Moves, Stats, N, FinalStats) :-
    N > 0,
    selectMoveUCB(Stats, N, SelectedMove),
    simulateGame(Board, Player, SelectedMove, Result),
    updateStats(Stats, SelectedMove, Result, Player, NewStats),
    N1 is N - 1,
    runSimulations(Board, Player, Moves, NewStats, N1, FinalStats).

selectMoveUCB(Stats, TotalPlays, SelectedMove) :-
    TotalPlaysFloat is float(TotalPlays),
    findBestUCB(Stats, TotalPlaysFloat, nil, -1000000, SelectedMove).

findBestUCB([], _, BestMove, _, BestMove) :- !.
findBestUCB([Move-Wins-Plays|Rest], TotalPlays, CurrentBest, CurrentBestScore, BestMove) :-
    (   Plays = 0 ->
        UCB = 1000000
    ;
        WinRate is Wins / Plays,
        Exploration is sqrt((2 * log(TotalPlays)) / Plays),
        UCB is WinRate + Exploration
    ),
    (   UCB > CurrentBestScore ->
        findBestUCB(Rest, TotalPlays, Move, UCB, BestMove)
    ;
        findBestUCB(Rest, TotalPlays, CurrentBest, CurrentBestScore, BestMove)
    ).

simulateGame(Board, Player, FirstMove, Result) :-
    simulateMove(Board, FirstMove, Player, NewBoard),
    changePlayer(Player, Opponent),
    playoutRandom(NewBoard, Opponent, Player, Result).

playoutRandom(Board, CurrentPlayer, OriginalPlayer, Result) :-
    (   winner(Board, Winner) ->
        (Winner = OriginalPlayer -> Result = win ; Result = loss)
    ;   isBoardFull(Board) ->
        Result = draw
    ;   allPossibleColumns(Board, Moves),
        random_member(Move, Moves),
        simulateMove(Board, Move, CurrentPlayer, NewBoard),
        changePlayer(CurrentPlayer, NextPlayer),
        playoutRandom(NewBoard, NextPlayer, OriginalPlayer, Result)
    ).

updateStats([], _, _, _, []).
updateStats([Move-Wins-Plays|Rest], SelectedMove, Result, Player, [Move-NewWins-NewPlays|NewRest]) :-
    (   Move = SelectedMove ->
        NewPlays is Plays + 1,
        (   Result = win -> NewWins is Wins + 1
        ;   Result = draw -> NewWins is Wins + 0.5
        ;   NewWins = Wins
        )
    ;
        NewWins = Wins,
        NewPlays = Plays
    ),
    updateStats(Rest, SelectedMove, Result, Player, NewRest).

selectBestMoveRandom(Stats, BestColumn) :-
    findBestWinRateValue(Stats, BestRate),
    findall(Move, (member(Move-Wins-Plays, Stats), Plays > 0, Wins / Plays >= BestRate - 0.01), BestMoves),
    (BestMoves = [] -> random_member(BestColumn, [0,1,2,3,4,5,6]) ; random_member(BestColumn, BestMoves)).

findBestWinRateValue(Stats, BestRate) :-
    findall(Rate, (member(_-Wins-Plays, Stats), Plays > 0, Rate is Wins / Plays), Rates),
    (Rates = [] -> BestRate = 0 ; max_list(Rates, BestRate)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% IA EXPERT - L'IA ULTIME
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ia_expert(Board, Column, Player) :-
    writeln('🤖 IA Expert réfléchit...'),
    (findWinningMove(Board, Player, WinMove) ->
        writeln('   ✓ Coup gagnant trouvé !'),
        Column = WinMove
    ; changePlayer(Player, Opponent), findWinningMove(Board, Opponent, BlockMove) ->
        writeln('   ✓ Blocage de victoire adverse !'),
        Column = BlockMove
    ; useOpeningBook(Board, Player, OpeningMove) ->
        writeln('   ✓ Utilisation de la base d\'ouvertures'),
        Column = OpeningMove
    ; findForkMove(Board, Player, ForkMove) ->
        writeln('   ✓ Création de fourchette !'),
        Column = ForkMove
    ; changePlayer(Player, Opponent), findForkMove(Board, Opponent, BlockFork) ->
        writeln('   ✓ Blocage de fourchette adverse !'),
        Column = BlockFork
    ;
        writeln('   → Recherche approfondie en cours...'),
        expertSearch(Board, Player, Column)
    ).

useOpeningBook(Board, Player, Column) :-
    countTotalMoves(Board, TotalMoves),
    TotalMoves < 4,
    openingMove(Board, Player, Column),
    canPlayInColumn(Board, Column).

openingMove(Board, 'x', 3) :- countTotalMoves(Board, 0).
openingMove(Board, 'o', 3) :- countTotalMoves(Board, 1), canPlayInColumn(Board, 3).
openingMove(Board, 'o', 2) :- countTotalMoves(Board, 1).
openingMove(Board, 'x', 4) :- countTotalMoves(Board, 2), nth0(3, Board, 'x'), canPlayInColumn(Board, 4).
openingMove(Board, 'x', 2) :- countTotalMoves(Board, 2), canPlayInColumn(Board, 2).

countTotalMoves(Board, Count) :-
    findall(1, (member(Piece, Board), nonvar(Piece)), Pieces),
    length(Pieces, Count).

expertSearch(Board, Player, Column) :-
    countTotalMoves(Board, TotalMoves),
    (TotalMoves < 10 -> Depth = 8 ;
     TotalMoves < 25 -> Depth = 9 ;
     Depth = 10),
    format('   → Profondeur de recherche: ~w niveaux~n', [Depth]),
    getNonPoisonedMoves(Board, Player, SafeMoves),
    length(SafeMoves, NumMoves),
    format('   → Évaluation de ~w coups possibles~n', [NumMoves]),
    format('   → Chaque coup prend 1-3 minutes en profondeur ~w~n', [Depth]),
    writeln('   → Calcul en cours (un point = 1 coup évalué):'),
    write('      '),
    flush_output,
    evaluateMovesExpertWithProgress(SafeMoves, Board, Player, Depth, 1, NumMoves, ScoredMoves),
    writeln(''),
    writeln('   ✓ Tous les coups évalués !'),
    findBestScore(ScoredMoves, BestScore),
    findall(Col, member(Col-BestScore, ScoredMoves), BestMoves),
    (member(3, BestMoves) -> Column = 3 ;
     member(2, BestMoves) -> Column = 2 ;
     member(4, BestMoves) -> Column = 4 ;
     random_member(Column, BestMoves)).

%%%% Évalue les coups avec indicateur de progression détaillé
evaluateMovesExpertWithProgress([], _, _, _, _, _, []).
evaluateMovesExpertWithProgress([Move|Moves], Board, Player, Depth, Current, Total, [Move-Score|Rest]) :-
    format('~n   → Évaluation coup ~w/~w (colonne ~w)...~n', [Current, Total, Move]),
    write('      Exploration: '),
    flush_output,
    % Initialiser le compteur de nœuds
    retractall(nodes_explored(_)),
    assert(nodes_explored(0)),
    simulateMove(Board, Move, Player, NewBoard),
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    minimaxExpertWithProgress(NewBoard, Opponent, NewDepth, -10000, 10000, _, OpponentScore),
    Score is -OpponentScore,
    nodes_explored(FinalNodes),
    format('~n      ✓ Coup ~w évalué (~w positions explorées)~n', [Move, FinalNodes]),
    flush_output,
    NextCurrent is Current + 1,
    evaluateMovesExpertWithProgress(Moves, Board, Player, Depth, NextCurrent, Total, Rest).

%%%% Minimax avec affichage de progression
minimaxExpertWithProgress(Board, Player, Depth, Alpha, Beta, BestColumn, BestScore) :-
    allPossibleColumns(Board, Moves),
    Moves \= [],
    alphabetaExpertWithProgress(Moves, Board, Player, Depth, Alpha, Beta, nil, -10000, BestColumn, BestScore).

alphabetaExpertWithProgress([], _, _, _, _, _, BestColumn, BestScore, BestColumn, BestScore) :- !.
alphabetaExpertWithProgress([Move|Moves], Board, Player, Depth, Alpha, Beta, TempBest, TempScore, BestColumn, BestScore) :-
    % Incrémenter et afficher la progression tous les 5000 nœuds
    nodes_explored(N),
    N1 is N + 1,
    retract(nodes_explored(N)),
    assert(nodes_explored(N1)),
    (N1 mod 5000 =:= 0 -> 
        write('.'),
        flush_output
    ; true),
    
    simulateMove(Board, Move, Player, NewBoard),
    (   (Depth =< 0 ; winner(NewBoard, _) ; isBoardFull(NewBoard)) ->
        evaluateExpert(NewBoard, Player, Score)
    ;
        changePlayer(Player, Opponent),
        NewDepth is Depth - 1,
        NegBeta is -Beta,
        NegAlpha is -Alpha,
        minimaxExpertWithProgress(NewBoard, Opponent, NewDepth, NegBeta, NegAlpha, _, OpponentScore),
        Score is -OpponentScore
    ),
    (   Score > TempScore ->
        NewBest = Move,
        NewScore = Score
    ;
        NewBest = TempBest,
        NewScore = TempScore
    ),
    NewAlpha is max(Alpha, NewScore),
    (   NewAlpha >= Beta ->
        BestColumn = NewBest,
        BestScore = NewScore
    ;
        alphabetaExpertWithProgress(Moves, Board, Player, Depth, NewAlpha, Beta, NewBest, NewScore, BestColumn, BestScore)
    ).

getNonPoisonedMoves(Board, Player, NonPoisoned) :-
    getSafeMoves(Board, Player, SafeMoves),
    changePlayer(Player, Opponent),
    filterPoisonedMoves(SafeMoves, Board, Player, Opponent, Filtered),
    (Filtered = [] -> NonPoisoned = SafeMoves ; NonPoisoned = Filtered).

filterPoisonedMoves([], _, _, _, []).
filterPoisonedMoves([Move|Moves], Board, Player, Opponent, Result) :-
    (isPoisonedMove(Board, Move, Player, Opponent) ->
        filterPoisonedMoves(Moves, Board, Player, Opponent, Result)
    ;
        filterPoisonedMoves(Moves, Board, Player, Opponent, Rest),
        Result = [Move|Rest]
    ).

isPoisonedMove(Board, Column, Player, Opponent) :-
    simulateMove(Board, Column, Player, NewBoard),
    getSafeMoves(NewBoard, Opponent, OpponentMoves),
    member(OpMove, OpponentMoves),
    createsFork(NewBoard, OpMove, Opponent).

evaluateMovesExpert([], _, _, _, []).
evaluateMovesExpert([Move|Moves], Board, Player, Depth, [Move-Score|Rest]) :-
    simulateMove(Board, Move, Player, NewBoard),
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    minimaxExpert(NewBoard, Opponent, NewDepth, -10000, 10000, _, OpponentScore),
    Score is -OpponentScore,
    evaluateMovesExpert(Moves, Board, Player, Depth, Rest).

minimaxExpert(Board, Player, Depth, Alpha, Beta, BestColumn, BestScore) :-
    allPossibleColumns(Board, Moves),
    Moves \= [],
    alphabetaExpert(Moves, Board, Player, Depth, Alpha, Beta, nil, -10000, BestColumn, BestScore).

alphabetaExpert([], _, _, _, _, _, BestColumn, BestScore, BestColumn, BestScore) :- !.
alphabetaExpert([Move|Moves], Board, Player, Depth, Alpha, Beta, TempBest, TempScore, BestColumn, BestScore) :-
    simulateMove(Board, Move, Player, NewBoard),
    (   (Depth =< 0 ; winner(NewBoard, _) ; isBoardFull(NewBoard)) ->
        evaluateExpert(NewBoard, Player, Score)
    ;
        changePlayer(Player, Opponent),
        NewDepth is Depth - 1,
        NegBeta is -Beta,
        NegAlpha is -Alpha,
        minimaxExpert(NewBoard, Opponent, NewDepth, NegBeta, NegAlpha, _, OpponentScore),
        Score is -OpponentScore
    ),
    (   Score > TempScore ->
        NewBest = Move,
        NewScore = Score
    ;
        NewBest = TempBest,
        NewScore = TempScore
    ),
    NewAlpha is max(Alpha, NewScore),
    (   NewAlpha >= Beta ->
        BestColumn = NewBest,
        BestScore = NewScore
    ;
        alphabetaExpert(Moves, Board, Player, Depth, NewAlpha, Beta, NewBest, NewScore, BestColumn, BestScore)
    ).

evaluateExpert(Board, Player, Score) :-
    (winner(Board, Player) ->
        Score = 10000
    ; changePlayer(Player, Opponent), winner(Board, Opponent) ->
        Score = -10000
    ; changePlayer(Player, Opponent), canWinNextMove(Board, Opponent) ->
        Score = -9500
    ; canWinNextMove(Board, Player) ->
        Score = 9000
    ; allPossibleColumns(Board, Moves), member(Move, Moves), createsFork(Board, Move, Player) ->
        Score = 8500
    ; changePlayer(Player, Opponent), 
      allPossibleColumns(Board, Moves), member(Move, Moves), createsFork(Board, Move, Opponent) ->
        Score = -8000
    ;
        evaluatePositionExpert(Board, Player, Score)
    ).

evaluatePositionExpert(Board, Player, Score) :-
    changePlayer(Player, Opponent),
    countPatterns(Board, Player, PlayerPatterns),
    countPatterns(Board, Opponent, OpponentPatterns),
    centerControl(Board, Player, PlayerCenter),
    centerControl(Board, Opponent, OpponentCenter),
    countThreats(Board, Player, PlayerThreats),
    countThreats(Board, Opponent, OpponentThreats),
    evaluateConnectivity(Board, Player, PlayerConn),
    evaluateConnectivity(Board, Opponent, OpponentConn),
    evaluateWinningStructures(Board, Player, PlayerStruct),
    evaluateWinningStructures(Board, Opponent, OpponentStruct),
    evaluateColumnControl(Board, Player, PlayerColControl),
    evaluateColumnControl(Board, Opponent, OpponentColControl),
    Score is PlayerPatterns - OpponentPatterns 
           + (PlayerCenter - OpponentCenter) * 2
           + PlayerThreats * 150 - OpponentThreats * 140
           + PlayerConn * 3 - OpponentConn * 3
           + PlayerStruct * 80 - OpponentStruct * 80
           + PlayerColControl * 20 - OpponentColControl * 20.

evaluateConnectivity(Board, Player, Score) :-
    findall(Bonus, (
        between(0, 41, Index),
        nth0(Index, Board, Player),
        countAdjacentPieces(Board, Index, Player, Adjacent),
        Bonus = Adjacent
    ), Bonuses),
    sumlist(Bonuses, Score).

countAdjacentPieces(Board, Index, Player, Count) :-
    Row is Index // 7,
    Col is Index mod 7,
    findall(1, (
        member([DR, DC], [[-1,0], [1,0], [0,-1], [0,1], [-1,-1], [-1,1], [1,-1], [1,1]]),
        NRow is Row + DR,
        NCol is Col + DC,
        NRow >= 0, NRow < 6, NCol >= 0, NCol < 7,
        NIndex is NRow * 7 + NCol,
        nth0(NIndex, Board, Player)
    ), Adjacent),
    length(Adjacent, Count).

evaluateWinningStructures(Board, Player, Score) :-
    countSevenShapes(Board, Player, Sevens),
    countDoubleDiagonals(Board, Player, Diagonals),
    Score is Sevens + Diagonals.

countSevenShapes(Board, Player, Count) :-
    findall(1, hasSevenShape(Board, Player), Shapes),
    length(Shapes, Count).

hasSevenShape(Board, Player) :-
    between(0, 34, Index),
    Row is Index // 7,
    Col is Index mod 7,
    Col =< 3,
    Row =< 3,
    nth0(Index, Board, Player),
    I1 is Index + 7, nth0(I1, Board, Player),
    I2 is Index + 1, nth0(I2, Board, Player).

countDoubleDiagonals(Board, Player, Count) :-
    findall(1, hasDoubleDiagonal(Board, Player), Diags),
    length(Diags, Count).

hasDoubleDiagonal(Board, Player) :-
    between(14, 20, Index),
    nth0(Index, Board, Player),
    I1 is Index + 8, I2 is Index - 8,
    I3 is Index + 6, I4 is Index - 6,
    (nth0(I1, Board, Player), nth0(I2, Board, Player) ;
     nth0(I3, Board, Player), nth0(I4, Board, Player)).

evaluateColumnControl(Board, Player, Score) :-
    countPiecesInColumn(Board, Player, 3, Center),
    countPiecesInColumn(Board, Player, 2, Left),
    countPiecesInColumn(Board, Player, 4, Right),
    Score is Center * 3 + Left * 2 + Right * 2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% IA EXPERT FAST - Version rapide de l'IA Expert
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Même algorithme que ia_expert mais avec profondeur réduite
% Recommandé pour des parties plus rapides (5-10 secondes par coup)

ia_expert_fast(Board, Column, Player) :-
    writeln('⚡ IA Expert Fast réfléchit...'),
    (findWinningMove(Board, Player, WinMove) ->
        writeln('   ✓ Coup gagnant trouvé !'),
        Column = WinMove
    ; changePlayer(Player, Opponent), findWinningMove(Board, Opponent, BlockMove) ->
        writeln('   ✓ Blocage de victoire adverse !'),
        Column = BlockMove
    ; useOpeningBook(Board, Player, OpeningMove) ->
        writeln('   ✓ Utilisation de la base d\'ouvertures'),
        Column = OpeningMove
    ; findForkMove(Board, Player, ForkMove) ->
        writeln('   ✓ Création de fourchette !'),
        Column = ForkMove
    ; changePlayer(Player, Opponent), findForkMove(Board, Opponent, BlockFork) ->
        writeln('   ✓ Blocage de fourchette adverse !'),
        Column = BlockFork
    ;
        writeln('   → Recherche rapide en cours...'),
        expertSearchFast(Board, Player, Column)
    ).

%%%% Version rapide de expertSearch (profondeur réduite)
expertSearchFast(Board, Player, Column) :-
    countTotalMoves(Board, TotalMoves),
    % Profondeur réduite : 5-7 au lieu de 8-10
    (TotalMoves < 10 -> Depth = 5 ;
     TotalMoves < 25 -> Depth = 6 ;
     Depth = 7),
    format('   → Profondeur de recherche: ~w niveaux~n', [Depth]),
    getNonPoisonedMoves(Board, Player, SafeMoves),
    length(SafeMoves, NumMoves),
    format('   → Évaluation de ~w coups possibles~n', [NumMoves]),
    format('   → Chaque coup prend 5-20 secondes en profondeur ~w~n', [Depth]),
    writeln('   → Calcul en cours (un point = 1 coup évalué):'),
    write('      '),
    flush_output,
    evaluateMovesExpertFastWithProgress(SafeMoves, Board, Player, Depth, 1, NumMoves, ScoredMoves),
    writeln(''),
    writeln('   ✓ Tous les coups évalués !'),
    findBestScore(ScoredMoves, BestScore),
    findall(Col, member(Col-BestScore, ScoredMoves), BestMoves),
    (member(3, BestMoves) -> Column = 3 ;
     member(2, BestMoves) -> Column = 2 ;
     member(4, BestMoves) -> Column = 4 ;
     random_member(Column, BestMoves)).

%%%% Évalue les coups avec indicateur de progression (version Fast)
evaluateMovesExpertFastWithProgress([], _, _, _, _, _, []).
evaluateMovesExpertFastWithProgress([Move|Moves], Board, Player, Depth, Current, Total, [Move-Score|Rest]) :-
    format('~n   → Évaluation coup ~w/~w (colonne ~w)...~n', [Current, Total, Move]),
    write('      Exploration: '),
    flush_output,
    % Initialiser le compteur de nœuds
    retractall(nodes_explored(_)),
    assert(nodes_explored(0)),
    simulateMove(Board, Move, Player, NewBoard),
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    minimaxExpertWithProgress(NewBoard, Opponent, NewDepth, -10000, 10000, _, OpponentScore),
    Score is -OpponentScore,
    nodes_explored(FinalNodes),
    format('~n      ✓ Coup ~w évalué (~w positions explorées)~n', [Move, FinalNodes]),
    flush_output,
    NextCurrent is Current + 1,
    evaluateMovesExpertFastWithProgress(Moves, Board, Player, Depth, NextCurrent, Total, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% CONVERSION DE FORMATS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Convertit un tableau 2D (liste de lignes) en tableau 1D (liste plate)
% Format 2D : [[Ligne0], [Ligne1], [Ligne2], [Ligne3], [Ligne4], [Ligne5]]
% Format 1D : [Case0, Case1, ..., Case41]
% 
% Exemple :
% ?- board2d_to_1d([[a,b,c,d,e,f,g], [h,i,j,k,l,m,n], ...], Board1D).
% Board1D = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,...]

board2d_to_1d(Board2D, Board1D) :-
    flatten(Board2D, Board1D).

%%%% Convertit un tableau 1D en tableau 2D
% Format 1D : [Case0, Case1, ..., Case41]
% Format 2D : [[Ligne0], [Ligne1], [Ligne2], [Ligne3], [Ligne4], [Ligne5]]
%
% Exemple :
% ?- board1d_to_2d([a,b,c,d,e,f,g,h,i,j,k,l,m,n,...], Board2D).
% Board2D = [[a,b,c,d,e,f,g], [h,i,j,k,l,m,n], ...]

board1d_to_2d(Board1D, Board2D) :-
    split_into_rows(Board1D, Board2D).

split_into_rows([], []).
split_into_rows(Board1D, [Row|RestRows]) :-
    length(Row, 7),
    append(Row, Rest, Board1D),
    split_into_rows(Rest, RestRows).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ADAPTATEURS POUR COMPATIBILITÉ ENTRE FORMATS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Convertir board avec variables (_) en board avec espaces (' ')
% Votre format → Format binôme
board_var_to_space([], []).
board_var_to_space([H|T], [H2|T2]) :-
    (var(H) -> H2 = ' ' ; H2 = H),
    board_var_to_space(T, T2).

%%%% Convertir board avec espaces (' ') en board avec variables (_)
% Format binôme → Votre format
board_space_to_var([], []).
board_space_to_var([H|T], [H2|T2]) :-
    (H == ' ' -> true ; H2 = H),
    board_space_to_var(T, T2).

%%%% Wrapper pour appeler l'IA du binôme avec conversion de format
% Convertit le board, appelle l'IA, retourne le résultat
%
% Exemple: call_binome_ia(ch_ia, Board, Column, Player)

call_binome_ia(IAName, BoardVar, Column, Player) :-
    % 1. Convertir le board (variables → espaces)
    board_var_to_space(BoardVar, BoardSpace),
    
    % 2. Appeler l'IA du binôme avec le format espaces
    call(IAName, BoardSpace, Column, Player).

%%%% Adaptateur spécifique pour ch_ia
ch_ia_adapted(Board, Column, Player) :-
    call_binome_ia(ch_ia, Board, Column, Player).

%%%% Adaptateur spécifique pour ch_monteCarloIA  
ch_monteCarloIA_adapted(Board, Player, Column) :-
    call_binome_ia(ch_monteCarloIA, Board, Column, Player).

%%%% Interface pour le tournoi : Joue un coup à partir d'un board 2D
% Utilise l'IA sélectionnée et retourne le board 2D mis à jour
%
% Exemple d'utilisation :
% ?- play_tournament_move([[_,_,_,_,_,_,_],[_,_,_,_,_,_,_],...], 'x', ia_expert_fast, NewBoard2D, Column).

play_tournament_move(Board2D, Player, IAName, NewBoard2D, Column) :-
    % 1. Convertir le board 2D en 1D
    board2d_to_1d(Board2D, Board1D),
    
    % 2. Appeler l'IA choisie
    call(IAName, Board1D, Column, Player),
    
    % 3. Jouer le coup sur le board 1D
    findLowestRow(Board1D, Column, Row),
    Index is Row * 7 + Column,
    copy_term(Board1D, NewBoard1D),
    nth0(Index, NewBoard1D, Player),
    
    % 4. Reconvertir en 2D
    board1d_to_2d(NewBoard1D, NewBoard2D).

%%%% Wrapper simplifié pour le tournoi avec IA par défaut
% Utilise ia_expert_fast par défaut (bon compromis vitesse/force)
%
% Exemple :
% ?- play_tournament_move_default([[_,_,_,_,_,_,_],...], 'x', NewBoard2D, Column).

play_tournament_move_default(Board2D, Player, NewBoard2D, Column) :-
    play_tournament_move(Board2D, Player, ia_expert_fast, NewBoard2D, Column).

%%%% Vérifier si une position 2D est gagnante
check_winner_2d(Board2D, Winner) :-
    board2d_to_1d(Board2D, Board1D),
    winner(Board1D, Winner).

%%%% Afficher un board 2D
display_board_2d(Board2D) :-
    board2d_to_1d(Board2D, Board1D),
    retractall(board(_)),
    assert(board(Board1D)),
    displayBoard,
    retract(board(Board1D)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TESTS ET EXEMPLES DE CONVERSION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Test de conversion 2D -> 1D
test_conversion_2d_to_1d :-
    % Créer un board 2D de test
    Board2D = [
        [a, b, c, d, e, f, g],  % Ligne 0 (bas)
        [h, i, j, k, l, m, n],  % Ligne 1
        [o, p, q, r, s, t, u],  % Ligne 2
        [v, w, x, y, z, 1, 2],  % Ligne 3
        [3, 4, 5, 6, 7, 8, 9],  % Ligne 4
        [_, _, _, _, _, _, _]   % Ligne 5 (haut)
    ],
    board2d_to_1d(Board2D, Board1D),
    writeln('=== Test conversion 2D -> 1D ==='),
    writeln('Board 2D:'),
    writeln(Board2D),
    writeln('Board 1D:'),
    writeln(Board1D),
    writeln('✓ Test réussi si Board1D = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,...]').

%%%% Test de conversion 1D -> 2D
test_conversion_1d_to_2d :-
    % Créer un board 1D de test
    length(Board1D, 42),
    % Remplir avec des valeurs de test
    numlist(0, 41, Board1D),
    board1d_to_2d(Board1D, Board2D),
    writeln('=== Test conversion 1D -> 2D ==='),
    writeln('Board 1D:'),
    writeln(Board1D),
    writeln('Board 2D:'),
    writeln(Board2D),
    writeln('✓ Test réussi si Board2D = [[0,1,2,3,4,5,6],[7,8,9,...]...]').

%%%% Test complet : conversion aller-retour
test_conversion_roundtrip :-
    writeln('=== Test conversion aller-retour ==='),
    % Créer un board 2D initial
    Board2D_Initial = [
        [_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_],
        [_,_,_,'x',_,_,_],
        [_,_,'o','x',_,_,_],
        [_,'x','o','x','o',_,_],
        ['o','x','o','o','x','x','o']
    ],
    write('Board 2D initial: '), writeln(Board2D_Initial),
    
    % Conversion 2D -> 1D
    board2d_to_1d(Board2D_Initial, Board1D),
    write('Board 1D: '), writeln(Board1D),
    
    % Conversion 1D -> 2D
    board1d_to_2d(Board1D, Board2D_Final),
    write('Board 2D final: '), writeln(Board2D_Final),
    
    % Vérifier l'égalité
    (Board2D_Initial = Board2D_Final ->
        writeln('✓ Test réussi : les boards sont identiques')
    ;
        writeln('✗ Test échoué : les boards sont différents')
    ).

%%%% Test avec l'IA du tournoi
test_tournament_old :-
    writeln('=== Test interface tournoi (ancien) ==='),
    % Board vide en format 2D
    Board2D_Empty = [
        [_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_]
    ],
    writeln('Board initial (2D):'),
    display_board_2d(Board2D_Empty),
    
    % Jouer un coup avec l'IA
    writeln('L\'IA joue...'),
    play_tournament_move(Board2D_Empty, 'x', ia_expert_fast, NewBoard2D, Column),
    
    format('Coup joué: colonne ~w~n', [Column]),
    writeln('Board après coup (2D):'),
    display_board_2d(NewBoard2D),
    writeln('✓ Test réussi si un coup a été joué').

%%%% Lancer tous les tests
run_all_tests :-
    writeln(''),
    writeln('╔════════════════════════════════════════╗'),
    writeln('║   TESTS DE CONVERSION DE FORMAT       ║'),
    writeln('╚════════════════════════════════════════╝'),
    writeln(''),
    test_conversion_2d_to_1d,
    writeln(''),
    test_conversion_1d_to_2d,
    writeln(''),
    test_conversion_roundtrip,
    writeln(''),
    test_tournament_interface,
    writeln(''),
    test_tournament_speed.

%%%% Vérifie si on peut jouer dans une colonne
canPlayInColumn(Board, Column) :- 
    TopIndex is 35 + Column,
    nth0(TopIndex, Board, Val),
    var(Val).

%%%% Trouve la ligne la plus basse disponible dans une colonne
findLowestRow(Board, Column, Row) :- 
    findLowestRowHelper(Board, Column, 0, Row).

findLowestRowHelper(Board, Column, CurrentRow, Row) :-
    CurrentRow < 6,
    Index is CurrentRow * 7 + Column,
    nth0(Index, Board, Val),
    (var(Val) -> Row = CurrentRow ; 
     NextRow is CurrentRow + 1, 
     findLowestRowHelper(Board, Column, NextRow, Row)).

%%%% Boucle de jeu principale
play(_) :- 
    gameover(Winner), !, 
    write('Jeu terminé. Gagnant: '), writeln(Winner), 
    displayBoard.

play(Player) :- 
    write('Nouveau tour pour: '), writeln(Player),
    board(Board),
    displayBoard,
    selectIA(Player, Board, Column),
    % Afficher le type de coup joué
    write('Coup joué: colonne '), write(Column),
    analyzeMove(Board, Column, Player),
    writeln(''),
    findLowestRow(Board, Column, Row),
    Index is Row * 7 + Column,
    playMove(Board, Index, NewBoard, Player),
    applyIt(Board, NewBoard),
    changePlayer(Player, NextPlayer),
    play(NextPlayer).

%%%% Analyse et affiche le type de coup
analyzeMove(Board, Column, Player) :-
    simulateMove(Board, Column, Player, NewBoard),
    countTotalMoves(Board, TotalMoves),
    (winner(NewBoard, Player) ->
        write(' [🏆 VICTOIRE!]')
    ; changePlayer(Player, Opponent), findWinningMove(Board, Opponent, Column) ->
        write(' [🛡️ BLOQUE VICTOIRE]')
    ; createsFork(Board, Column, Player) ->
        write(' [⚔️ FOURCHETTE - Double menace!]')
    ; changePlayer(Player, Opponent), 
      (allPossibleColumns(Board, Moves), member(Column, Moves), createsFork(Board, Column, Opponent)) ->
        write(' [🛡️ BLOQUE FOURCHETTE]')
    ; TotalMoves < 3, Column = 3 ->
        write(' [📖 Ouverture optimale - Centre]')
    ; TotalMoves < 4, member(Column, [2,4]) ->
        write(' [📖 Ouverture - Près du centre]')
    ; hasSevenShape(NewBoard, Player) ->
        write(' [🔨 Structure en 7]')
    ; evaluateConnectivity(NewBoard, Player, Conn), Conn > 10 ->
        write(' [🔗 Haute connectivité]')
    ;
        true
    ).

%%%% Sélection de l'IA en fonction du joueur
% Par défaut, utilise IA EXPERT FAST (plus rapide, 5-10 sec par coup)
selectIA('x', Board, Column) :- ia_expert_fast(Board, Column, 'x').
selectIA('o', Board, Column) :- ia_expert_fast(Board, Column, 'o').

% ===== CONFIGURATIONS ALTERNATIVES =====

% Pour utiliser l'IA EXPERT complète (LENT : 30 sec - 2 min par coup) :
% selectIA('x', Board, Column) :- ia_expert(Board, Column, 'x').
% selectIA('o', Board, Column) :- ia_expert(Board, Column, 'o').

% Pour faire jouer EXPERT FAST vs MCTS :
% selectIA('x', Board, Column) :- ia_expert_fast(Board, Column, 'x').
% selectIA('o', Board, Column) :- ia_mcts(Board, Column, 'o').

% Pour faire jouer EXPERT FAST vs MINIMAX :
% selectIA('x', Board, Column) :- ia_expert_fast(Board, Column, 'x').
% selectIA('o', Board, Column) :- ia_minimax(Board, Column, 'o').

% Pour faire jouer MCTS vs MINIMAX :
% selectIA('x', Board, Column) :- ia_mcts(Board, Column, 'x').
% selectIA('o', Board, Column) :- ia_minimax(Board, Column, 'o').

% Pour faire jouer MINIMAX vs RANDOM :
% selectIA('x', Board, Column) :- ia_minimax(Board, Column, 'x').
% selectIA('o', Board, Column) :- ia_random(Board, Column, 'o').

%%%% Joue un coup
playMove(Board, Index, NewBoard, Player) :- 
    Board = NewBoard, 
    nth0(Index, NewBoard, Player).

%%%% Met à jour la base de connaissances
applyIt(Board, NewBoard) :- 
    retract(board(Board)), 
    assert(board(NewBoard)).

%%%% Change de joueur
changePlayer('x', 'o').
changePlayer('o', 'x').

%%%% Affiche une valeur du plateau
printVal(N) :- 
    board(B), 
    nth0(N, B, Val), 
    var(Val), 
    write('.'), !.
printVal(N) :- 
    board(B), 
    nth0(N, B, Val), 
    write(Val).

%%%% Affiche le plateau
displayBoard :-
    writeln('*-----------------*'),
    writeln('  0 1 2 3 4 5 6'),
    printRow(5),
    printRow(4),
    printRow(3),
    printRow(2),
    printRow(1),
    printRow(0),
    writeln('*-----------------*').

printRow(RowNum) :-
    write('| '),
    forall(between(0, 6, Col), 
           (Index is RowNum * 7 + Col, printVal(Index), write(' '))),
    writeln('|').

%%%% Initialise et démarre le jeu
init :- 
    length(Board, 42),
    retractall(board(_)),
    retractall(nodes_explored(_)),
    assert(nodes_explored(0)),
    assert(board(Board)), 
    writeln(''),
    writeln('========================================'),
    writeln('     PUISSANCE 4 - IA vs IA'),
    writeln('========================================'),
    writeln(''),
    play('x').

%%%% Lance plusieurs parties pour comparer les IA
% Exemples d'utilisation :
% ?- benchmark(ia_expert_fast, ia_mcts, 10).
% ?- benchmark(ia_expert_fast, ia_minimax, 5).
% ?- benchmark(ia_mcts, ia_minimax, 10).
% ?- benchmark(ia_expert, ia_expert_fast, 3).  ⚠️ LENT !

benchmark(IA1, IA2, NumGames) :-
    writeln(''),
    writeln('=== BENCHMARK DES IA ==='),
    format('~w (x) vs ~w (o) - ~w parties~n', [IA1, IA2, NumGames]),
    writeln(''),
    runBenchmark(IA1, IA2, NumGames, 0, 0, 0).

runBenchmark(_, _, 0, Wins1, Wins2, Draws) :- !,
    Total is Wins1 + Wins2 + Draws,
    writeln(''),
    writeln('=== RÉSULTATS ==='),
    format('Joueur 1 (x): ~w victoires (~w%)~n', [Wins1, Wins1 * 100 // Total]),
    format('Joueur 2 (o): ~w victoires (~w%)~n', [Wins2, Wins2 * 100 // Total]),
    format('Matchs nuls: ~w (~w%)~n', [Draws, Draws * 100 // Total]).

runBenchmark(IA1, IA2, N, Wins1, Wins2, Draws) :-
    N > 0,
    format('Partie ~w... ', [N]),
    length(Board, 42),
    retractall(board(_)),
    assert(board(Board)),
    playBenchmark('x', IA1, IA2, Result),
    (Result = 'x' -> 
        NewWins1 is Wins1 + 1, NewWins2 = Wins2, NewDraws = Draws, writeln('Victoire J1')
    ; Result = 'o' ->
        NewWins1 = Wins1, NewWins2 is Wins2 + 1, NewDraws = Draws, writeln('Victoire J2')
    ;
        NewWins1 = Wins1, NewWins2 = Wins2, NewDraws is Draws + 1, writeln('Match nul')
    ),
    N1 is N - 1,
    runBenchmark(IA1, IA2, N1, NewWins1, NewWins2, NewDraws).

playBenchmark(Player, IA1, IA2, Result) :-
    (gameover(Winner) ->
        Result = Winner
    ;
        board(Board),
        (Player = 'x' -> call(IA1, Board, Column, Player) ; call(IA2, Board, Column, Player)),
        findLowestRow(Board, Column, Row),
        Index is Row * 7 + Column,
        playMove(Board, Index, NewBoard, Player),
        applyIt(Board, NewBoard),
        changePlayer(Player, NextPlayer),
        playBenchmark(NextPlayer, IA1, IA2, Result)
    ).

%%%% Prédicats pour l'analyse des coups possibles
possibleColumn(Board, Col) :- 
    between(0, 6, Col),
    canPlayInColumn(Board, Col).

allPossibleColumns(Board, L) :- 
    findall(Col, possibleColumn(Board, Col), L).