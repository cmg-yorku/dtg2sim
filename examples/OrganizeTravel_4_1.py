from .test_engine import TestEngine
t = TestEngine(dtg=False)
t.exec_domain("./examples/4.1.OrganizeTravel.pl",
              [0.1, 0.4],
              [-1.0, 1.0], # Don't care as long as it runs.
              [0.90, +0.91],
              test_dtg=t.dtg)

