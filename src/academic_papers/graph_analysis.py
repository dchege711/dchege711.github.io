import sqlite3
import networkx as nx
import os
import pickle

db_path = "/Users/dchege711/datasets/citations_data/papers.db"
conn = sqlite3.connect(db_path)
conn.row_factory = sqlite3.Row

def get_references_graph(use_cache=True):
    """
    Generate a graph object where nodes are paper ids and edge `(u, v)` means 
    that paper `u` cited paper `v`
    """

    cache_path = "./cache/reference_graph.pickle"
    if use_cache:
        if os.path.exists(cache_path):
            cache_mtime = os.path.getmtime(cache_path)
            db_mtime = os.path.getmtime(db_path)
            if db_mtime < cache_mtime:
                return pickle.load(open(cache_path, "r"))

    G = nx.DiGraph()

    print("Creating the graph of references...")
    cursor = conn.execute(
        "SELECT paper_id, cited_paper_id FROM paper_references;"
    )
    for row in cursor:
        G.add_edge(row["paper_id"], row["cited_paper_id"])

    print("Adding title information...")
    cursor = conn.execute(
        "SELECT paper_id, title FROM papers;"
    )
    for row in cursor:
        if row["paper_id"] not in G:
            G.add_node(row["paper_id"], title=row["title"])
        else:
            G.nodes[row["paper_id"]]["title"] = row["title"]
    
    print("Caching the graph object...")
    with open(cache_path, "w") as f:
        pickle.dump(G, f)
        
    return G

def cycle_analysis(G):
    print("Looking for cycles in the reference graph...")
    cycles = nx.cycle_basis(G)
    for cycle in cycles:
        print("______________________")
        for node in cycle:
            print(node["title"])
        print("______________________")

if __name__ == "__main__":
    G = get_references_graph()
    cycle_analysis(G)




