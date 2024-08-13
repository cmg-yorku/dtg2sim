# -*- coding: utf-8 -*-
"""
Created on Mon Sep 19 11:22:43 2022

@author: Anonymous
"""


import scripts.GMEnv as sim
import scripts.Tester as test
import unittest


class TestSum(unittest.TestCase):
    
    def setUp(self):
        self.env = sim.GMEnv("./examples/discrete/3Build.pl")
        self.env.setDebug(False)
        self.env.setSeed(123)
        self.t = test.TestIt(self.env)
        self.t.debug = False
        self.inFeasiblePenalty = self.env.getInfeasiblePenalty()

    def tearDown(self):
        self.env.closeQE()

    def takeStep(self,action,
                   stActionExp,
                   stateExp,
                   ALExp,
                   SLExp,
                   runExp,
                   rewardExp,
                   cumRewardExp,
                   doneExp,
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
        self.assertEqual(transState,
                         info["TransState"], 
                         msg = "\n (TestID: {}) - Wrong 'TransState' status: {} expected, {} observed".format(ID,transState,info["TransState"]))
        

    def test_various(self):
        
        self.takeStep(action = 0,
            stActionExp = 0, stateExp = 512, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = 1.0*0.7, cumRewardExp = 1.0*0.7,
            doneExp = False,
            transState = "[]",
            ID = 1)
    
        self.takeStep(action = 0,
            stActionExp = -1, stateExp = 512, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 1.0*0.7 + self.inFeasiblePenalty,
            doneExp = True,
            transState = "[]",
            ID = 2)

        self.takeStep(action = 1,
            stActionExp = -1, stateExp = 512, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 1.0*0.7 + 2*self.inFeasiblePenalty,
            doneExp = True,
            transState = "[]",
            ID = 3)

        self.takeStep(action = 2,
            stActionExp = -1, stateExp = 512, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 1.0*0.7  + 3*self.inFeasiblePenalty,
            doneExp = True,
            transState = "[]",
            ID = 4)
    
        self.takeStep(action = 2,
            stActionExp = -1, stateExp = 512, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp =  1.0*0.7 + 4*self.inFeasiblePenalty,
            doneExp = True,
            transState = "[]",
            ID = 5)    
        
        self.takeStep(action = 3,
            stActionExp = -1, stateExp = 512, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 1.0*0.7  + 5*self.inFeasiblePenalty,
            doneExp = True,
            transState = "[]",
            ID = 6)    

        self.t.reset()
        self.env.setSeed(15)

        self.takeStep(action = 1,
            stActionExp = 4, stateExp = 32, 
            ALExp = [[1]], SLExp = [[4]], runExp = 0, 
            rewardExp = 0.3 - 1.0*0.7, 
            cumRewardExp = 0.3 - 1.0*0.7,
            doneExp = False,
            transState = "[]",
            ID = 11)  

        self.takeStep(action = 3,
            stActionExp = 8, stateExp = 34, 
            ALExp = [[1,3],[]], SLExp = [[4,8],[]], runExp = 1, 
            rewardExp = 1.0*0.7 + 0.5*0.3, 
            cumRewardExp = 0.3 - 1.0*0.7 + 1.0*0.7 + 0.5*0.3,
            doneExp = True,
            transState = "[]",
            ID = 15)

        self.takeStep(action = 3,
            stActionExp = -1, stateExp = 34, 
            ALExp = [[1,3],[]], SLExp = [[4,8],[]], runExp = 1, 
            rewardExp = self.inFeasiblePenalty, 
            cumRewardExp = 0.3 - 1.0*0.7 + 1.0*0.7 + 0.5*0.3 + 1*self.inFeasiblePenalty,
            doneExp = True,
            transState = "[]",
            ID = 16)

        self.takeStep(action = 2,
            stActionExp = -1, stateExp = 34, 
            ALExp = [[1,3],[]], SLExp = [[4,8],[]], runExp = 1, 
            rewardExp = self.inFeasiblePenalty, 
            cumRewardExp = 0.3 - 1.0*0.7 + 1.0*0.7 + 0.5*0.3 + 2*self.inFeasiblePenalty,
            doneExp = True,
            transState = "[]",
            ID = 17)

        self.t.reset()
        self.env.setSeed(15)
    
        self.takeStep(action = 1,
            stActionExp = 4, stateExp = int(0b0000100000), 
            ALExp = [[1]], SLExp = [[4]], runExp = 0, 
            rewardExp = 0.3 - 1.0*0.7, 
            cumRewardExp = 0.3 - 1.0*0.7,
            doneExp = False,
            transState = "[]",
            ID = 18)  
    
        self.takeStep(action = 2,
            stActionExp = -1, stateExp = int(0b0000100000), 
            ALExp = [[1]], SLExp = [[4]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, 
            cumRewardExp = 0.3 - 1.0*0.7 + self.inFeasiblePenalty,
            doneExp = True,
            transState = "[]",
            ID = 19, choice= 6)



        
if __name__ == '__main__':
    unittest.main()
    


# env = sim.GoalModel("./Examples/3Build.pl")
# env.setDebug(True)
# env.setSeed(123)
# t = test.TestIt(env)
# t.debug = False
# a,b,c,d,e = t.performAction(0)
# a,b,c,d,e = t.performAction(0)
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
