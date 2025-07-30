import os

from src.GMEnv import GMEnv
from src.QueryEngine import QueryEngine
from src.dtg2sim import dtg2sim
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]

    if not os.path.exists(filename):
        print(f"Could not locate: {filename}")
        os._exit(0)
    else:
        e = QueryEngine(filename)
        g = GMEnv(qEng = e)
        c = dtg2sim(g)
        print(f"\nLoaded Specification: {filename}")
        print("Access dtg2sim via variable c, GMEnv via g, and QueryEngine via e.\n")
        print("To simulate..: c.simulate()")
        print("To train.....: c.train()")
        print("To test......: c.test()")
        print("To debug.....: g.reset() followed by repeated g.step(agentAction,[stochasticAction]).\n")
        print("For parameter info: help([dtg2sim or GMEnv].[method])")
else:
    print("No filename provided.")
    os._exit(0)
