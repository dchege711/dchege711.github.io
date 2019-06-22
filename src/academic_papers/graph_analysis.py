import sqlite3
import os
import pickle
from cProfile import Profile
from typing import List

import networkx as nx
import numpy as np
from sklearn.preprocessing import normalize

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

def get_normalized_adjacency_citation_matrix():
    """
    Construct the transitional matrix M, where `M[i][j]` represents the link 
    from `j` to `i` such that for all `j`, `sum(i, M[i][j]) = 1`

    """
    cursor = conn.execute("SELECT count(papers.paper_id) from papers;")
    N = cursor.fetchone()["count(papers.paper_id)"]

    paper_id_to_idx_mapping = {}
    cursor = conn.execute(
        "SELECT paper_id, cited_paper_id FROM paper_references;"
    )
    idx = -1
    adjacency_matrix = np.zeros((N, N))
    for row in cursor:
        if row["paper_id"] not in paper_id_to_idx_mapping:
            idx += 1
            paper_id_to_idx_mapping[row["paper_id"]] = idx

        if row["cited_paper_id"] not in paper_id_to_idx_mapping:
            idx += 1
            paper_id_to_idx_mapping[row["cited_paper_id"]] = idx
        
        i = paper_id_to_idx_mapping[row["cited_paper_id"]]
        j = paper_id_to_idx_mapping[row["paper_id"]]
        adjacency_matrix[i, j] = 1

    adjacency_matrix = normalize(adjacency_matrix, norm="l1", axis=0, copy=False)
    return adjacency_matrix, paper_id_to_idx_mapping

def calculate_pagerank(M, d=0.85, eps=1.0e-8) -> List[float]:
    """
    Calculate the page rank of the various articles. Adapted from 
    https://en.wikipedia.org/wiki/PageRank#Python
    
    :kwarg d: ``float`` Damping factor

    :kwarg eps: ``float`` The quadratic error for the vector of ranks, `v`

    :return List[float]: A vector of ranks such that `v[i]` is the rank 
    of the `i`-th paper from `[0, 1]`
    """

    N = M.shape[1]
    v = np.random.rand(N, 1)
    v = v / np.linalg.norm(v, 1)
    last_v = np.ones((N, 1), dtype=np.float32) * 100
    
    while np.linalg.norm(v - last_v, 2) > eps:
        last_v = v
        v = d * np.matmul(M, v) + (1 - d) / N
    return v

def run_pagerank():
    print("Fetching the normalized citations adjacency matrix...", end=" ")
    M, mapping = get_normalized_adjacency_citation_matrix()

if __name__ == "__main__":
    pr = Profile()
    pr.enable()

    run_pagerank()

    pr.disable()
    pr.print_stats()
