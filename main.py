from src.GMEnv import GMEnv
from src.QueryEngine import QueryEngine
from src.dtg2sim import dtg2sim
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
    print(f"Secification: {filename}")
    print("Creating dtg2sim, GMEnv, QueryEngine objects c, g, and e")
    e = QueryEngine(filename)
    g = GMEnv(qEng = e)
    c = dtg2sim(g)
    

else:
    print("No filename provided.")
