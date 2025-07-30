from .test_engine import TestEngine
t = TestEngine(dtg=True)
t.exec_domain("./examples/3.1.Heating_1R_Mixed.pl",
              [-3.0, -2.7],
              [-1.42, -1.40],
              [-1.42, -1.40],
              test_dtg=t.dtg)

