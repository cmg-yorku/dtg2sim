# -*- coding: utf-8 -*-
"""
Created on Mon Sep 19 11:22:43 2022

@author: Anonymous
"""


import scripts.GMEnv as sim
import scripts.Tester as test
import unittest
#from stable_baselines3 import A2C


class TestSum(unittest.TestCase):

    def setUp(self):
        self.env = sim.GMEnv("./examples/discrete/9SoSymExample.pl")
        self.env.setDebug(False)
        self.env.setSeed(123)
        self.t = test.TestIt(self.env)
        self.t.debug = False
        self.inFeasiblePenalty = self.env.getInfeasiblePenalty()
        
    def takeStep(self,action,
                  stActionExp,
                  stateExp,
                  ALExp,
                  SLExp,
                  runExp,
                  rewardExp,
                  cumRewardExp,
                  doneExp,
                  achievedExp,
                  transState,
                  ID = -1,choice = -1):
   
       stateObs, rewardObs, cumRewardObs, doneObs, info = self.t.performAction(action,choice)
       
       self.assertEqual(stActionExp,
                        info["stAction"], 
                        msg = "\n (TestID: {}) - Wrong stochastic action: {} expected, {} observed".format(ID,stActionExp,info["stAction"]))
       self.assertEqual(stateExp,
                        stateObs, 
                        msg = "\n (TestID: {}) - Wrong state: {} expected, {} observed".format(ID,stateExp,stateObs))
       self.assertEqual(ALExp,
                        info["tH"], 
                        msg = "\n (TestID: {}) - Wrong tH: {} expected, {} observed".format(ID,ALExp,info["tH"]))
       self.assertEqual(SLExp,
                        info["eH"], 
                        msg = "\n (TestID: {}) - Wrong eH: {} expected, {} observed".format(ID,SLExp,info["eH"]))
       self.assertEqual(runExp,
                        info["Run"], 
                        msg = "\n (TestID: {}) - Wrong run: {} expected, {} observed".format(ID,runExp,info["Run"]))
       self.assertAlmostEqual(rewardExp,
                        rewardObs, 
                        places = 1,
                        msg = "\n (TestID: {}) - Wrong reward: {} expected, {} observed".format(ID,rewardExp,rewardObs))
       self.assertAlmostEqual(cumRewardExp,
                        cumRewardObs, 
                        places = 1,
                        msg = "\n (TestID: {}) - Wrong cummuative reward: {} expected, {} observed".format(ID,cumRewardExp,cumRewardObs))
       self.assertEqual(doneExp,
                        doneObs, 
                        msg = "\n (TestID: {}) - Wrong 'done' status: {} expected, {} observed".format(ID,doneExp,doneObs))
       self.assertEqual(achievedExp,
                        info["Achieved"], 
                        msg = "\n (TestID: {}) - Wrong 'achieved' status: {} expected, {} observed".format(ID,achievedExp,info["Achieved"]))
       self.assertEqual(transState,
                         info["TransState"], 
                         msg = "\n (TestID: {}) - Wrong 'TransState' status: {} expected, {} observed".format(ID,transState,info["TransState"]))

        

    def test_various(self):
        self.takeStep(action = 0,
            stActionExp = 0, stateExp = 8192, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = 0.2, cumRewardExp = 0.2,
            doneExp = False,
            achievedExp = False,
            transState = "[]",
            ID = 1)
			
        self.takeStep(action = 0,
            stActionExp = -1, stateExp = 8192, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 0.2 + 1*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 2)
			
        self.takeStep(action = 1,
            stActionExp = -1, stateExp = 8192, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 0.2 + 2*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 3)
			
        self.takeStep(action = 3,
            stActionExp = -1, stateExp = 8192, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 0.2 + 3*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 4)			

        self.takeStep(action = 2,
            stActionExp = -1, stateExp = 8192, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 0.2  + 4*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 5)
			
        self.takeStep(action = 5,
            stActionExp = -1, stateExp = 8192, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 0.2 + 5*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 6)
        
        self.env.reset()
        self.t.reset()
        self.env.setSeed(4)
        
        self.takeStep(action = 1,
            stActionExp = 3, stateExp = 1024, 
            ALExp = [[1]], SLExp = [[3]], runExp = 0, 
            rewardExp = 0.0, cumRewardExp = 0.0,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 7)
			
        self.takeStep(action = 3,
            stActionExp = -1, stateExp = 1024, 
            ALExp = [[1]], SLExp = [[3]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 0 + 1*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 8)			

        self.takeStep(action = 4,
            stActionExp = -1, stateExp = 1024, 
            ALExp = [[1]], SLExp = [[3]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 0 + 2*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 9)			

        self.takeStep(action = 5,
            stActionExp = -1, stateExp = 1024, 
            ALExp = [[1]], SLExp = [[3]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 0 + 3*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 10)
    			
        self.env.reset()
        self.t.reset()
        self.env.setSeed(23)
			
        self.takeStep(action = 1,
            stActionExp = 2, stateExp = 2048, 
            ALExp = [[1]], SLExp = [[2]], runExp = 0, 
            rewardExp = 0, cumRewardExp = 0,
            doneExp = False,
            achievedExp = False,
            transState = "[]",
            ID = 11)
			
        self.takeStep(action = 2,
            stActionExp = 6, stateExp = 2176, 
            ALExp = [[1,2]], SLExp = [[2,6]], runExp = 0, 
            rewardExp = 0, cumRewardExp = 0,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 12)

        self.takeStep(action = 3,
            stActionExp = -1, stateExp = 2176, 
            ALExp = [[1,2]], SLExp = [[2,6]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 0 + 1*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 13)
			
        self.takeStep(action = 4,
            stActionExp = -1, stateExp = 2176, 
            ALExp = [[1,2]], SLExp = [[2,6]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 0 + 2*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 14)
			
        self.takeStep(action = 5,
            stActionExp = -1, stateExp = 2176, 
            ALExp = [[1,2]], SLExp = [[2,6]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 0 + 3*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 15)
    

if __name__ == '__main__':
    unittest.main()

# import scripts.simulator as sim
# import scripts.tester as test

# env = sim.GoalModel("./Examples/9SoSymExample.pl")
# env.setDebug(True)

# t = test.TestIt(env)
# t.debug = False

# env.reset()
# env.setSeed(4)
# a,b,c,d,e = t.performAction(1)



# env.reset()
# env.setSeed(23)
# a,b,c,d,e = t.performAction(1)
# a,b,c,d,e = t.performAction(2)



# a,b,c,d,e = t.performAction(1)
# a,b,c,d,e = t.performAction(2)
# a,b,c,d,e = t.performAction(2)
# a,b,c,d,e = t.performAction(3)
# a,b,c,d,e = t.performAction(1)
# a,b,c,d,e = t.performAction(0)
# a,b,c,d,e = t.performAction(2)
# a,b,c,d,e = t.performAction(3)
# env.reset()
# env.setSeed(15)
# a,b,c,d,e = t.performAction(1)
# a,b,c,d,e = t.performAction(1)
# a,b,c,d,e = t.performAction(0)
# a,b,c,d,e = t.performAction(2)
# a,b,c,d,e = t.performAction(3)
# a,b,c,d,e = t.performAction(3)
# a,b,c,d,e = t.performAction(2)
# a,b,c,d,e = t.performAction(1)
# a,b,c,d,e = t.performAction(0)
# a,b,c,d,e = t.performAction(2)
# a,b,c,d,e = t.performAction(3)