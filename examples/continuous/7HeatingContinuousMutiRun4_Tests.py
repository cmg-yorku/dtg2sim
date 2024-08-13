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
        self.env = sim.GMEnv("./examples/continuous/7HeatingContinuousMultiRun4.pl")
        self.env.setDebug(False)
        self.env.setSeed(1223)
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
       return(stateObs, rewardObs, cumRewardObs, doneObs, info)
        

    def test_various(self):
        

        # TEST ONE
        # Heat three steps 
        # Cool last steps
        i=1
        self.takeStep(action = 0,
            stActionExp = 0, stateExp = [26, 10], 
             ALExp = [[0],[]], SLExp =[[0],[]], runExp = 1,
             rewardExp = -1.92, cumRewardExp = -1.92,
             doneExp = False,
             achievedExp = False,
             transState = "[roomTemp_Inst_fl(26.0), hvac_on_fl]",
             ID = i)
        i+=1
        # SignalOn fails
        self.takeStep(action = 0,
            stActionExp = 1, stateExp = [26.95, 10], 
            ALExp = [[0],[0],[]], SLExp =[[0],[1],[]], runExp = 2,
            rewardExp = -2.585, cumRewardExp = -1.92 -2.585,
            doneExp = False,
            achievedExp = False,
            transState = "[roomTemp_Inst_fl(26.95), hvac_on_fl]",
            ID = i,choice = 1)
        i+=1
        # SignalOff fails
        self.takeStep(action = 1,
            stActionExp = 3, stateExp = [27.8525, 10], 
            ALExp = [[0],[0],[1],[]], SLExp =[[0],[1],[3],[]], runExp = 3,
            rewardExp = -3.217, cumRewardExp = -1.92 -2.585-3.217,
            doneExp = False,
            achievedExp = False,
            transState = "[roomTemp_Inst_fl(27.8525), hvac_on_fl]",
            ID = i,choice = 3)
        i+=1
        # SignalOff finally works
        self.takeStep(action = 1,
             stActionExp = 2, stateExp = [25.567249999999998, 0], 
             ALExp = [[0],[0],[1],[1],[]], SLExp =[[0],[1],[3],[2],[]], runExp = 4,
             rewardExp = -1.797075, cumRewardExp = -1.92 -2.585-3.217-1.797075,
             doneExp = True,
             achievedExp = False,
             transState = "[roomTemp_Inst_fl(25.567249999999998)]",
             ID = i,choice = 2)
        i+=1
        # SignalOff finally works
        self.takeStep(action = 0,
             stActionExp = -1, stateExp = [25.567249999999998, 0], 
             ALExp = [[0],[0],[1],[1],[]], SLExp =[[0],[1],[3],[2],[]], runExp = 4,
             rewardExp = -100, cumRewardExp = -1.92 -2.585-3.217-1.797075-100,
             doneExp = True,
             achievedExp = False,
             transState = "[roomTemp_Inst_fl(25.567249999999998)]",
             ID = i)
	
        # TEST TWO
        # Heat one step 
        # Cool one step
        # Heat one step 
        # Cool one step
        self.t.reset()
        
        i=10
        # Turn On
        self.takeStep(action = 0,
            stActionExp = 0, stateExp = [26,10], 
             ALExp = [[0],[]], SLExp =[[0],[]], runExp = 1,
             rewardExp = -1.92, cumRewardExp = -1.92,
             doneExp = False,
             achievedExp = False,
             transState = "[roomTemp_Inst_fl(26.0), hvac_on_fl]",
             ID = i,choice = 0)
        i+=1
        # Turn Off
        self.takeStep(action = 1,
            stActionExp = 2, stateExp = [23.9,0], 
            ALExp = [[0],[1],[]], SLExp =[[0],[2],[]], runExp = 2,
            rewardExp = -0.63, cumRewardExp = -2.55,
            doneExp = False,
            achievedExp = False,
            transState = "[roomTemp_Inst_fl(23.9)]",
            ID = i,choice = 2)
        i+=1
        # Turn On
        self.takeStep(action = 0,
            stActionExp = 0, stateExp = [24.955,10], 
            ALExp = [[0],[1],[0],[]], SLExp =[[0],[2],[0],[]], runExp = 3,
            rewardExp = -1.1885, cumRewardExp =	-3.7385,
            doneExp = False,
            achievedExp = False,
            transState = "[roomTemp_Inst_fl(24.955), hvac_on_fl]",
            ID = i,choice = 0)
        i+=1
        # Turn off
        self.takeStep(action = 1,
             stActionExp = 2, stateExp = [22.9595,0], 
             ALExp = [[0],[1],[0],[1],[]], SLExp =[[0],[2],[0],[2],[]], runExp = 4,
             rewardExp = -0.02835, cumRewardExp = -3.76685,
             doneExp = True,
             achievedExp = False,
             transState = "[roomTemp_Inst_fl(22.9595)]",
             ID = i,choice = 2)
        i+=1
        # Runs are over
        self.takeStep(action = 1,
             stActionExp = -1, stateExp = [22.9595,0], 
             ALExp = [[0],[1],[0],[1],[]], SLExp =[[0],[2],[0],[2],[]], runExp = 4,
             rewardExp = -100, cumRewardExp = -3.76685-100,
             doneExp = True,
             achievedExp = False,
             transState = "[roomTemp_Inst_fl(22.9595)]",
             ID = i)
        i+=1

        # TEST THREE
        # Cool and never turn on.
        self.t.reset()
        
        i=20
        # Turn On
        self.takeStep(action = 1,
            stActionExp = 2, stateExp = [23,0], 
             ALExp = [[1],[]], SLExp =[[2],[]], runExp = 1,
             rewardExp = 0, cumRewardExp = 0,
             doneExp = False,
             achievedExp = False,
             transState = "[roomTemp_Inst_fl(23.0)]",
             ID = i,choice = 2)
        i+=1
       # Turn Off
        self.takeStep(action = 1,
            stActionExp = 3, stateExp = [21.2,0], 
            ALExp = [[1],[1],[]], SLExp =[[2],[3],[]], runExp = 2,
            rewardExp = -1.26, cumRewardExp = -1.26,
            doneExp = False,
            achievedExp = False,
            transState = "[roomTemp_Inst_fl(21.2)]",
            ID = i,choice = 3)
        i+=1
        # Turn On
        self.takeStep(action = 0,
            stActionExp = 1, stateExp = [19.58,0], 
            ALExp = [[1],[1],[0],[]], SLExp =[[2],[3],[1],[]], runExp = 3,
            rewardExp = -2.394, cumRewardExp =-3.654,
            doneExp = False,
            achievedExp = False,
            transState = "[roomTemp_Inst_fl(19.58)]",
            ID = i,choice = 1)
        i+=1
        # Turn off
        self.takeStep(action = 1,
             stActionExp = 3, stateExp = [18.122,0], 
             ALExp = [[1],[1],[0],[1],[]], SLExp =[[2],[3],[1],[3],[]], runExp = 4,
             rewardExp = -3.4146, cumRewardExp = -7.0686,
             doneExp = True,
             achievedExp = False,
             transState = "[roomTemp_Inst_fl(18.122)]",
             ID = i,choice = 3)
        i+=1
        # Runs are over
        self.takeStep(action = 0,
             stActionExp = -1, stateExp = [18.122,0], 
             ALExp = [[1],[1],[0],[1],[]], SLExp =[[2],[3],[1],[3],[]], runExp = 4,
             rewardExp = -100, cumRewardExp = -7.0686-100,
             doneExp = True,
             achievedExp = False,
             transState = "[roomTemp_Inst_fl(18.122)]",
             ID = i)
        i+=1
        


if __name__ == '__main__':
    unittest.main()
