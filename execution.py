from pyswip import Prolog
import networkx as nx
import matplotlib.pyplot as plt
import re

# Initialize Prolog and consult the Prolog file
prolog = Prolog()
prolog.consult("p.pl")  # Ensure p.pl is in the same directory

# Query for one valid coloring solution
result = list(prolog.query("prolog_solution(Solve).", maxresult=1))[0]
solution = result['Solve']

# Convert Prolog result (e.g., hasColor(1, red)) into a Python dictionary
coloring = {}
pattern = re.compile(r"hasColor\((\d+),\s*(\w+)\)")

for term in solution:
    term_str = str(term)
    match = pattern.match(term_str)
    if match:
        city = int(match.group(1))
        color = match.group(2)
        coloring[city] = color

# Define the graph structure (edges)
edges = [
    (1, 2), (1, 5), (2, 3), (2, 5),
    (3, 4), (3, 5), (4, 5)
]

# Create and populate the graph
G = nx.Graph()
G.add_edges_from(edges)

# Define color mapping for consistent node coloring
color_map = {
    "red": "#e74c3c",
    "blue": "#3498db",
    "green": "#2ecc71"
}
node_colors = [color_map[coloring[n]] for n in G.nodes]

# Draw the graph
pos = nx.spring_layout(G, seed=42)
nx.draw(
    G, pos,
    with_labels=True,
    node_color=node_colors,
    node_size=1000,
    font_weight='bold',
    font_color='white'
)
plt.title("Graph Coloring from Prolog 🧠🎨")
plt.show()
