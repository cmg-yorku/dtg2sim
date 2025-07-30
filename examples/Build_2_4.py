from .test_engine import TestEngine
t = TestEngine(dtg=True)
t.exec_domain("./examples/2.4.Build_1R_Mixed.pl",
              [-3.1, -2.9],
              [-2.78, -2.68],
              [-2.76, -2.74],
              test_dtg=True)

