# %%
import os

from graphdatascience import GraphDataScience

# %%
host = os.environ["NEO4J_URI"]
user = os.environ["NEO4J_USER"]
password =os.environ["NEO4J_PASSWORD"]
# %%
gds = GraphDataScience(host, auth=(user, password), database="neo4j")
# %%
bham = gds.find_node_id(["Station"], {"name": "Birmingham New Street"})
eboro = gds.find_node_id(["Station"], {"name": "Edinburgh"})
# %%
graph = gds.graph.get("trains")
shortest_path = gds.shortestPath.dijkstra.stream(
    graph,
    sourceNode=bham,
    targetNode=eboro,
    relationshipWeightProperty="distance"
)
result = gds.betweenness.stream(graph)
highest_score = (
    result.sort_values(
        by="score",
        ascending=False
    )
    .iloc[0:1]
    .get("nodeId")
)
n = gds.run_cypher(
    f"""\
MATCH (s:Station)
WHERE ID(s) = {int(highest_score)}
RETURN s.name"""
)
gds.close()
# %%
shortest_path_cost = shortest_path.get("costs").get(0)[-1]
n
# %%
