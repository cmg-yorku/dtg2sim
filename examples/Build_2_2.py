from .test_engine import TestEngine
t = TestEngine(dtg=False)
t.exec_domain("./examples/2.2.Build_3R_Discrete.pl",
              [2.8, 3.5],
              [1, 3])

