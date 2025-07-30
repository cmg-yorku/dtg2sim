from .test_engine import TestEngine
t = TestEngine(dtg=False)
t.exec_domain("./examples/3.2.Heating_10R_Mixed.pl",
              [-2.0, -1.7],
              [-1.1, -0.5],
              test_dtg=t.dtg)

