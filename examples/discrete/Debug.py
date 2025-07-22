# -*- coding: utf-8 -*-
"""
Created on Thu May  8 09:49:14 2025

@author: Sotirios
"""


import scripts.GMEnv as sim
import scripts.Tester as test

env = sim.GMEnv("examples/discrete/1Order.pl")
t = test.TestIt(env)
t.reset()
t.performAction(1)