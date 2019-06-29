import sqlite3
from typing import List
from operator import itemgetter
from cProfile import Profile
from pstats import Stats
import json
from csv import DictWriter

import networkx as nx
from networkx.readwrite import json_graph
import numpy as np
from sklearn.preprocessing import normalize
from scipy.sparse import csr_matrix, lil_matrix

db_path = "/Users/dchege711/datasets/citations_data/papers.db"
conn = sqlite3.connect(db_path)
conn.row_factory = sqlite3.Row

CACHE_FILEPATH = "./__cache__/citation_graph_cache.json"
CITATIONS_QUERY = " ".join([
    "SELECT paper_references.paper_id, paper_references.cited_paper_id",
    "FROM paper_references, papers",
    "WHERE paper_references.paper_id = papers.paper_id AND papers.n_citation >= ?;"
])
MIN_NUM_CITATIONS = 100

def cache_graph(G):
    g_data = json_graph.node_link_data(G)
    data_to_cache = {
        "query": CITATIONS_QUERY, "graph": g_data
    }
    with open(CACHE_FILEPATH, "w") as fp:
        json.dump(data_to_cache, fp)

def retrieve_graph_from_cache():
    try: fp = open(CACHE_FILEPATH, "r")
    except FileNotFoundError: return None

    cached_data = json.load(fp)
    if cached_data["query"] == CITATIONS_QUERY: 
        print("Retrieved citations graph from cache!")
        return json_graph.node_link_graph(cached_data["graph"])
    else: return None

def get_citation_graph():
    """
    Generate a graph object where nodes are paper ids and edge `(u, v)` means 
    that paper `u` cited paper `v`
    """
    G = retrieve_graph_from_cache()
    if G is not None: return G
    
    G = nx.DiGraph()
    cursor = conn.execute(CITATIONS_QUERY, [MIN_NUM_CITATIONS])
    for row in cursor:
        for paper_id in [row["paper_id"], row["cited_paper_id"]]:
            if paper_id not in G:
                G.add_node(paper_id)
                paper_info = conn.execute(
                    """
                    SELECT papers.title, papers.year, papers.n_citation  
                    FROM papers WHERE papers.paper_id = ? 
                    """, [paper_id]
                ).fetchone()
                paper_info_dict = {}
                for key in paper_info.keys():
                    G.nodes[paper_id][key] = paper_info[key]

        G.add_edge(row["paper_id"], row["cited_paper_id"])

    # Append author information for easier Google searches
    cursor = conn.execute(
        """
        SELECT authors.author_name, authors.paper_id 
        FROM authors WHERE authors.appearance_order = 1;
        """
    )
    for row in cursor:
        if row["paper_id"] in G:
            G.nodes[row["paper_id"]]["author_name"] = row["author_name"]

    cache_graph(G)
    return G

def cycle_analysis(G):
    print("Looking for cycles in the reference graph...")
    cycles = nx.cycle_basis(G)
    for cycle in cycles:
        print("______________________")
        for node in cycle:
            print(node["title"])
        print("______________________")

def get_citations_adjacency_matrix() -> (lil_matrix, dict):
    """
    Construct the adjacency matrix for the citation data. The biggest limitation 
    is efficiency.

    | Filter | Time (Seconds) |
    | --- | --- |
    | any citation | 338.707 | 
    | citations from papers with >= 100 citations | 61.387 |

    """

    paper_id_to_idx_mapping = {}
    for row in conn.execute(CITATIONS_QUERY, [MIN_NUM_CITATIONS]):
        for paper_id in [row["paper_id"], row["cited_paper_id"]]:
            if paper_id not in paper_id_to_idx_mapping:
                paper_id_to_idx_mapping[paper_id] = len(paper_id_to_idx_mapping)

    N = len(paper_id_to_idx_mapping)
    print("The matrix is {:,} x {:,} ...".format(N, N))
    adjacency_matrix = lil_matrix((N, N), dtype="d")
    
    cursor = conn.execute(CITATIONS_QUERY, [MIN_NUM_CITATIONS])
    for row in cursor:
        i = paper_id_to_idx_mapping[row["cited_paper_id"]]
        j = paper_id_to_idx_mapping[row["paper_id"]]
        adjacency_matrix[i, j] = 1

    return adjacency_matrix, paper_id_to_idx_mapping

def run_nx_analysis():
    print("Fetching the citation graph...")
    G = get_citation_graph()

    print("\n---------------------------------\n")
    print("Here is the pagerank analysis...\n")
    pr = [(paper_id, rank) for (paper_id, rank) in nx.pagerank(G).items()]
    pr.sort(key=itemgetter(1), reverse=True)

    with open("../../assets/pagerank_publications.txt", "w") as fp:
        writer = DictWriter(
            fp, extrasaction="ignore", dialect="excel-tab", 
            fieldnames=["rank", "title", "author_name", "year", "n_citation"]
        )
        writer.writeheader()
        for i in range(3000):
            paper_node = G.nodes[pr[i][0]]
            paper_node["rank"] = i + 1
            writer.writerow(paper_node)

    print("\n---------------------------------\n")

if __name__ == "__main__":
    pr = Profile()
    pr.enable()

    # run_pagerank()
    run_nx_analysis()

    pr.disable()
    ps = Stats(pr)
    ps.sort_stats("cumulative")
    ps.print_stats(0.1)
