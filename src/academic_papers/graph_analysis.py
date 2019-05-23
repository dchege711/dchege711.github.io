import sqlite3
import networkx as nx

conn = sqlite3.connect("/Users/dchege711/datasets/citations_data/papers.db")
conn.row_factory = sqlite3.Row

def get_references_graph():
    """
    Generate a graph object where nodes are paper ids and edge `(u, v)` means 
    that paper `u` cited paper `v`
    """
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




