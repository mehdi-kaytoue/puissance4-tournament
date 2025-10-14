# tournament.py
# Orchestrateur: prend toutes les pairs d'ia possibles du dossier ias
# et les fait jouer en mode round-robin, en enregistrant les résultats dans un CSV horodaté.

import subprocess # pour appeler swipl
import itertools # pour faire les  pairs d'ia
import csv # pour écrire le csv
from pathlib import Path # pour manipuler les chemins de fichiers
from datetime import datetime # pour horodater le csv


SWIPL = 'swipl'  # assume que swipl est dans la variable PATH

ENGINE = 'engine.pl'
TIME_PER_MOVE = 4.0  # secondes : on donne un budget temps par coup

def board_to_prolog(board):
    # board/plateau: une liste liste (6 lignes avec pour chacune 7 éléments)
    return str(board).replace('None','0') # On traduit dans un format pour le programme Prolog les cases vides.


def ask_ai_move(ia_file, board, player, timeout=TIME_PER_MOVE):
# demande à l'ia de jouer un coup pour un joueur donnée sur un plateau donné avec un budget temps
    board_term = board_to_prolog(board)
    print ("Board term:", board_term) # DEBUG
    # Construire la requête Prolog
    query = f"Board={board_term}, Joueur={player}, (joue_coup(Board, Joueur, C) -> format('~w',[C]); halt(2)), halt."
    print(query) # DEBUG
    cmd = [SWIPL, '-s', ia_file, '-s', ENGINE, '-g', query] # commande à exécuter
    print(cmd) # DEBUG
    try:
        p = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)
        print("AI stderr:", p.returncode, p.stderr) # DEBUG
        if p.returncode == 0:
            out = p.stdout.strip()
            print("AI output:", out)
            if out == '':
                return None
            return int(out)
        else:
            return None
    except subprocess.TimeoutExpired:
        return None

def apply_move_py(board, col, player):
    # applique un coup dans le plateau (vérifie que c'est valide)
    new = [row.copy() for row in board]
    for r in range(5, -1, -1):
        if new[r][col] == 0:
            new[r][col] = player
            return new
    raise Exception('Invalid move')

def winner_py(board):
    # brute-force ...
    rows = 6; cols = 7
    for r in range(rows):
        for c in range(cols):
            p = board[r][c]
            if p == 0: continue
            # horiz
            if c+3 < cols and all(board[r][c+i]==p for i in range(4)): return p
            # vert
            if r+3 < rows and all(board[r+i][c]==p for i in range(4)): return p
            # diag1
            if r+3 < rows and c+3 < cols and all(board[r+i][c+i]==p for i in range(4)): return p
            # diag2
            if r+3 < rows and c-3 >= 0 and all(board[r+i][c-i]==p for i in range(4)): return p
    return 0

def play_game(ia1, ia2, timeout_per_move=TIME_PER_MOVE, verbose=True):
    board = [[0]*7 for _ in range(6)]
    current = 1
    moves = 0
    while True:
        ia = ia1 if current==1 else ia2
        col = ask_ai_move(ia, board, current, timeout=timeout_per_move)
        print("Col-------", col) # DEBUG
        if col is None:
            # timeout or error -> choose random valid
            # pick first valid
            valid = [c for c in range(7) if board[0][c]==0]
            if not valid:
                return 0, moves
            col = valid[0]
        try:
            board = apply_move_py(board, col, current)
            print(board)
            for row in board:
                print(' '.join(str(x) for x in row))
                
            print('---')
        except Exception:
            # invalid move => opponent wins
            return 3-current, moves
        moves += 1
        w = winner_py(board)
        if w != 0:
            return w, moves
        if moves >= 42:
            return 0, moves
        current = 3 - current

def round_robin(iapaths):
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    out_csv = f"results/tournament_results_{timestamp}.csv"
    rows = []
    pairs = list(itertools.permutations(iapaths,2))
    for a,b in pairs:   
        winner, moves = play_game(a,b)
        rows.append({'ia1':Path(a).name, 'ia2':Path(b).name, 'winner': Path(a).name if winner==1 else (Path(b).name if winner==2 else 'draw'), 'moves':moves})
        print(f"{Path(a).name} vs {Path(b).name} -> {rows[-1]['winner']} in {moves} moves")
    with open(out_csv,'w',newline='') as f:
        writer = csv.DictWriter(f, fieldnames=['ia1','ia2','winner','moves'])
        writer.writeheader(); writer.writerows(rows)
    return out_csv

if __name__=='__main__':
    import sys
    if len(sys.argv) < 2:
        print('Usage: python tournament.py ia1.pl ia2.pl ia3.pl ...')
        sys.exit(1)
    ias = sys.argv[1:]
    round_robin(ias)
