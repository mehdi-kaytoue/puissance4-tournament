# visualize.py: lit results.csv et produit un graphe simple (bracket-like)
import pandas as pd
import networkx as nx
import matplotlib.pyplot as plt

def visualize(csvfile='results.csv', out='bracket.png'):
    df = pd.read_csv(csvfile)
    # compute wins per IA
    wins = {}
    for _, r in df.iterrows():
        winner = r['winner']
        if winner=='draw': continue
        wins[winner] = wins.get(winner,0)+1
    items = sorted(wins.items(), key=lambda x:-x[1])
    # simple bar plot
    names = [i[0] for i in items]
    vals = [i[1] for i in items]
    plt.figure(figsize=(8,4))
    plt.bar(names, vals)
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(out)
    print('Saved', out)

if __name__=='__main__':
    visualize()