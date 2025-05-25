# %%
import os
from pathlib import Path

from graphdatascience import GraphDataScience

# %%
host = os.environ["NEO4J_URI"]
user = os.environ["NEO4J_USER"]
password =os.environ["NEO4J_PASSWORD"]
# %%
nr_stations_all_csv_url = "https://raw.githubusercontent.com/jbarrasa/gc-2022/refs/heads/main/interop/data/nr-stations-all.csv"
nr_stations_links_csv_url = "https://raw.githubusercontent.com/jbarrasa/gc-2022/refs/heads/main/interop/data/nr-station-links.csv"
# %%
gds = GraphDataScience(host, auth=(user, password), database="neo4j")
gds.run_cypher(
    f"""\
LOAD CSV WITH HEADERS FROM '{nr_stations_all_csv_url}' AS station
CREATE (:Station {{name: station.name, crs: station.crs}})
"""
)
gds.run_cypher(
    f"""\
LOAD CSV WITH HEADERS FROM '{nr_stations_links_csv_url}' AS track
MATCH (from: Station {{crs: track.from}})
MATCH (to:Station {{crs: track.to}})
MERGE (from)-[:TRACK {{distance: round(toFloat(track.distance), 2)}}]->(to)
"""
)
gds.close()