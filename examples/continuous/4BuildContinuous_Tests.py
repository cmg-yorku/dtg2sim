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
        self.env = sim.GMEnv("./examples/continuous/4BuildContinuous.pl")
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
        
        i=1
        self.takeStep(action = 0,
            stActionExp = 0, stateExp = [1,0], 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = 1.0*0.7, cumRewardExp = 1.0*0.7,
            doneExp = False,
            transState = "[reputation_fl(1.0), gain_fl(0)]",
            ID = i,choice = 0)

        i+=1
        self.takeStep(action = 0,
            stActionExp = -1, stateExp = [1,0], 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 1.0*0.7 + self.inFeasiblePenalty,
            doneExp = True,
            transState = "[reputation_fl(1.0), gain_fl(0)]",
            ID = i)
        i+=1
        self.takeStep(action = 1,
            stActionExp = -1, stateExp = [1,0], 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 1.0*0.7 + 2*self.inFeasiblePenalty,
            doneExp = True,
            transState = "[reputation_fl(1.0), gain_fl(0)]",
            ID = i)
        i+=1
        self.takeStep(action = 2,
            stActionExp = -1, stateExp = [1,0], 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 1.0*0.7  + 3*self.inFeasiblePenalty,
            doneExp = True,
            transState = "[reputation_fl(1.0), gain_fl(0)]",
            ID = i)
        i+=1    
        self.takeStep(action = 2,
            stActionExp = -1, stateExp = [1,0], 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp =  1.0*0.7 + 4*self.inFeasiblePenalty,
            doneExp = True,
            transState = "[reputation_fl(1.0), gain_fl(0)]",
            ID = i)    
        i+=1        
        self.takeStep(action = 3,
            stActionExp = -1, stateExp = [1,0], 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 1.0*0.7  + 5*self.inFeasiblePenalty,
            doneExp = True,
            transState = "[reputation_fl(1.0), gain_fl(0)]",
            ID = i)    

        self.t.reset()
        self.env.setSeed(15)
        i = 20
        self.takeStep(action = 1,
            stActionExp = 4, stateExp = [-1,1], 
            ALExp = [[1]], SLExp = [[4]], runExp = 0, 
            rewardExp = 0.3 - 1.0*0.7, 
            cumRewardExp = 0.3 - 1.0*0.7,
            doneExp = False,
            transState = "[reputation_fl(-1.0), gain_fl(1.0)]",
            ID = i,choice = 4)  
        i+=1
        self.takeStep(action = 3,
            stActionExp = 8, stateExp = [0,1.5], 
            ALExp = [[1,3],[]], SLExp = [[4,8],[]], runExp = 1, 
            rewardExp = 1.0*0.7 + 0.5*0.3, 
            cumRewardExp = 0.3 - 1.0*0.7 + 1.0*0.7 + 0.5*0.3,
            doneExp = True,
            transState = "[reputation_fl(0.0), gain_fl(1.5)]",
            ID = i, choice = 8)
        i+=1
        self.takeStep(action = 3,
            stActionExp = -1, stateExp = [0,1.5], # *** CC States RESETS with 1 step lag
            ALExp = [[1,3],[]], SLExp = [[4,8],[]], runExp = 1, 
            rewardExp = self.inFeasiblePenalty, 
            cumRewardExp = 0.3 - 1.0*0.7 + 1.0*0.7 + 0.5*0.3 + 1*self.inFeasiblePenalty,
            doneExp = True,
            transState = "[reputation_fl(0.0), gain_fl(1.5)]",
            ID = i)
        i+=1
        self.takeStep(action = 2,
            stActionExp = -1, stateExp = [0,1.5], 
            ALExp = [[1,3],[]], SLExp = [[4,8],[]], runExp = 1, 
            rewardExp = self.inFeasiblePenalty, 
            cumRewardExp = 0.3 - 1.0*0.7 + 1.0*0.7 + 0.5*0.3 + 2*self.inFeasiblePenalty,
            doneExp = True,
            transState = "[reputation_fl(0.0), gain_fl(1.5)]",
            ID = i)

        self.t.reset()
        self.env.setSeed(15)
        
        i = 30
        self.takeStep(action = 0,
            stActionExp = 1, stateExp = [-1,0], 
            ALExp = [[0]], SLExp = [[1]], runExp = 0, 
            rewardExp = - 0.7, 
            cumRewardExp = - 0.7,
            doneExp = False,
            transState = "[reputation_fl(-1.0), gain_fl(0)]",
            ID = i, choice = 1)  
        i+=1
        self.takeStep(action = 2,
            stActionExp = 6, stateExp = [0,0], # This is not reset, its co-incidence
            ALExp = [[0,2],[]], SLExp = [[1,6],[]], runExp = 1, 
            rewardExp = 0.7, 
            cumRewardExp = 0,
            doneExp = True,
            transState = "[reputation_fl(0.0), gain_fl(0)]",
            ID = i, choice= 6)

        i+=1
        self.takeStep(action = 2,
            stActionExp = -1, stateExp = [0,0], # This is not reset it is co-incidence
            ALExp = [[0,2],[]], SLExp = [[1,6],[]], runExp = 1, 
            rewardExp = self.inFeasiblePenalty, 
            cumRewardExp = self.inFeasiblePenalty,
            doneExp = True,
            transState = "[reputation_fl(0.0), gain_fl(0)]",
            ID = i)

        self.t.reset()
        self.env.setSeed(15)
        i = 40
        self.takeStep(action = 0,
            stActionExp = 0, stateExp = [1,0], 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp =  0.7, 
            cumRewardExp =  0.7,
            doneExp = False,
            transState = "[reputation_fl(1.0), gain_fl(0)]",
            ID = i, choice = 0)  
        i+=1
        self.takeStep(action = 3,
            stActionExp = 9, stateExp = [0,0.5], 
            ALExp = [[0,3],[]], SLExp = [[0,9],[]], runExp = 1, 
            rewardExp = -0.55, 
            cumRewardExp = 0.15,
            doneExp = True,
            transState = "[reputation_fl(0.0), gain_fl(0.5)]",
            ID = i, choice= 9)
        i+=1
        self.takeStep(action = 3,
            stActionExp = -1, stateExp = [0,0.5], # There is no reset of state variable is transcedental
            ALExp = [[0,3],[]], SLExp = [[0,9],[]], runExp = 1, 
            rewardExp = self.inFeasiblePenalty, 
            cumRewardExp = 0.15 + self.inFeasiblePenalty,
            doneExp = True,
            transState = "[reputation_fl(0.0), gain_fl(0.5)]",
            ID = i, choice= 8)

        self.t.reset()
        self.env.setSeed(15)
        i = 50
        self.takeStep(action = 1,
            stActionExp = 4, stateExp = [-1,1], 
            ALExp = [[1]], SLExp = [[4]], runExp = 0, 
            rewardExp =  -0.4, 
            cumRewardExp =  -0.4,
            doneExp = False,
            transState = "[reputation_fl(-1.0), gain_fl(1.0)]",
            ID = i, choice = 4)  
        i+=1
        self.takeStep(action = 2,
            stActionExp = -1, stateExp = [-1,1], 
            ALExp = [[1]], SLExp = [[4]], runExp = 0, 
            rewardExp = self.env.inFeasiblePenalty, 
            cumRewardExp = -0.4 + self.env.inFeasiblePenalty,
            doneExp = True,
            transState = "[reputation_fl(-1.0), gain_fl(1.0)]",
            ID = i, choice= 7)
        i+=1
        self.takeStep(action = 2,
            stActionExp = -1, stateExp = [-1,1], #State does not reset variables are transcedental.
            ALExp = [[1]], SLExp = [[4]], runExp = 0, 
            rewardExp = self.env.inFeasiblePenalty, 
            cumRewardExp = -0.4 + 2*self.env.inFeasiblePenalty,
            doneExp = True,
            transState = "[reputation_fl(-1.0), gain_fl(1.0)]",
            ID = i, choice= 7)

        self.t.reset()
        self.env.setSeed(15)
        i = 60
        self.takeStep(action = 0,
            stActionExp = 2, stateExp = [-5,0], 
            ALExp = [[0]], SLExp = [[2]], runExp = 0, 
            rewardExp =  -3.5, 
            cumRewardExp =  -3.5,
            doneExp = True,
            transState = "[reputation_fl(-5.0), gain_fl(0)]",
            ID = i, choice = 2)  
        i+=1
        self.takeStep(action = 0,
            stActionExp = -1, stateExp = [-5,0], # There is no progress we stay at same state
            ALExp = [[0]], SLExp = [[2]], runExp = 0, 
            rewardExp =  self.env.inFeasiblePenalty, 
            cumRewardExp = self.env.inFeasiblePenalty -3.5,
            doneExp = True,
            transState = "[reputation_fl(-5.0), gain_fl(0)]",
            ID = i, choice = 6)  


        self.t.reset()
        self.env.setSeed(15)
        i = 70
        self.takeStep(action = 1,
            stActionExp = 5, stateExp = [-5,1], 
            ALExp = [[1]], SLExp = [[5]], runExp = 0, 
            rewardExp =  -3.2, 
            cumRewardExp =  -3.2,
            doneExp = True,
            transState = "[reputation_fl(-5.0), gain_fl(1.0)]",
            ID = i, choice = 5)  
        i+=1
        self.takeStep(action = 2,
            stActionExp = -1, stateExp = [-5,1], # There is no progress we stay at same state
            ALExp = [[1]], SLExp = [[5]], runExp = 0, 
            rewardExp =  self.env.inFeasiblePenalty,
            cumRewardExp = self.env.inFeasiblePenalty - 3.2,
            doneExp = True,
            transState = "[reputation_fl(-5.0), gain_fl(1.0)]",
            ID = i, choice = 7)  

        
if __name__ == '__main__':
    unittest.main()
    
