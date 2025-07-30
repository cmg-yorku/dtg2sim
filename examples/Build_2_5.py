from .test_engine import TestEngine
t = TestEngine(dtg=False)
t.exec_domain("./examples/2.5.Build_5R_Mixed.pl",
              [-3.1, -2.9],
              test_dtg=t.dtg)

