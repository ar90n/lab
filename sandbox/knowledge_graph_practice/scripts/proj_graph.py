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
gds.graph.project.cypher(
    graph_name="trains",
    node_spec="MATCH (s:Station) RETURN id(s) AS id",
    relationship_spec="""\
MATCH (s1:Station)-[t:TRACK]->(s2:Station)
RETURN id(s1) AS source, id(s2) AS target, t.distance AS distance"""
)
gds.close()