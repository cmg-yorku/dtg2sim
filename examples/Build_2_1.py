from .test_engine import TestEngine
t = TestEngine(dtg=True)
t.exec_domain("./examples/2.1.Build_1R_Discrete.pl",
                     [0.9, 1.1],
                     [1.3, 1.4],
                     [1.34, 1.35],
                     test_dtg=True)

