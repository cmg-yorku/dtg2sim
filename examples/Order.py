from .test_engine import TestEngine
t = TestEngine(dtg=True)
t.exec_domain("./examples/1.Order.pl",
                 [0.75, 0.8],
                 [0.9, 0.93],
                 [0.923, 0.924],
                 test_dtg=True)
