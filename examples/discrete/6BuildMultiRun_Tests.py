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
        self.env = sim.GMEnv("./examples/discrete/6BuildMultiRun.pl")
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
            stActionExp = 0, stateExp = 524288, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = 1.0*0.7, cumRewardExp = 1.0*0.7,
            doneExp = False,
            achievedExp = False,
            transState = "[]",
            ID = 1)

    
        self.takeStep(action = 0,
            stActionExp = -1, stateExp = 524288, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 1.0*0.7 + self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 2)

        self.takeStep(action = 1,
            stActionExp = -1, stateExp = 524288, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 1.0*0.7 + 2*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 3)

        self.takeStep(action = 2,
            stActionExp = -1, stateExp = 524288, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 1.0*0.7 + 3*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 4)
    
        self.takeStep(action = 2,
            stActionExp = -1, stateExp = 524288, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 1.0*0.7 + 4*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 5)    
        
        self.takeStep(action = 3,
            stActionExp = -1, stateExp = 524288, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 1.0*0.7 + 5*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 6)    

        self.takeStep(action = 1,
            stActionExp = -1, stateExp = 524288, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 1.0*0.7 + 6*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 7)    

        self.takeStep(action = 1,
            stActionExp = -1, stateExp = 524288, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 1.0*0.7 + 7*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 8)  

        self.takeStep(action = 2,
            stActionExp = -1, stateExp = 524288, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 1.0*0.7 + 8*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 9)  


        self.takeStep(action = 3,
            stActionExp = -1, stateExp = 524288, 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, cumRewardExp = 1.0*0.7 + 9*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 10)  


        self.t.reset()
        self.env.setSeed(15)

        self.takeStep(action = 1,
            stActionExp = 4, stateExp = 32768, 
            ALExp = [[1]], SLExp = [[4]], runExp = 0, 
            rewardExp = 0.3 - 1.0*0.7, 
            cumRewardExp = 0.3 - 1.0*0.7,
            doneExp = False,
            achievedExp = False,
            transState = "[]",
            ID = 11)  

    #     self.takeStep(action = 1,
    #         stActionExp = -1, stateExp = 32768, 
    #         ALExp = [[1]], SLExp = [[4]], runExp = 0, 
    #         rewardExp = self.inFeasiblePenalty, 
    #         cumRewardExp = 0.3 - 1.0*0.7 + self.inFeasiblePenalty,
    #         doneExp = False,
    #         achievedExp = False,
    #         ID = 12) 

    #     self.takeStep(action = 0,
    #         stActionExp = -1, stateExp = 32768, 
    #         ALExp = [[1]], SLExp = [[4]], runExp = 0, 
    #         rewardExp = self.inFeasiblePenalty, 
    #         cumRewardExp = 0.3 - 1.0*0.7 + 2*self.inFeasiblePenalty,
    #         doneExp = False,
    #         achievedExp = False,
    #         ID = 13)
        
    #     self.takeStep(action = 2,
    #         stActionExp = -1, stateExp = 32768, 
    #         ALExp = [[1]], SLExp = [[4]], runExp = 0, 
    #         rewardExp = self.inFeasiblePenalty, 
    #         cumRewardExp = 0.3 - 1.0*0.7 + 3*self.inFeasiblePenalty,
    #         doneExp = False,
    #         achievedExp = False,
    #         ID = 14)

        self.takeStep(action = 3,
            stActionExp = 8, stateExp = 34816, 
            ALExp = [[1,3],[]], SLExp = [[4,8],[]], runExp = 1, 
            rewardExp = 1.0*0.7 + 0.5*0.3, 
            cumRewardExp = 0.3 - 1.0*0.7 + 1.0*0.7 + 0.5*0.3,
            doneExp = False,
            achievedExp = False,
            transState = "[]",
            ID = 15)

    #     self.takeStep(action = 3,
    #         stActionExp = -1, stateExp = 34816, 
    #         ALExp = [[1,3],[]], SLExp = [[4,8],[]], runExp = 1, 
    #         rewardExp = self.inFeasiblePenalty, 
    #         cumRewardExp = 0.3 - 1.0*0.7 + 1.0*0.7 + 0.5*0.3 + 4*self.inFeasiblePenalty,
    #         doneExp = False,
    #         achievedExp = False,
    #         ID = 16)

    #     self.takeStep(action = 2,
    #         stActionExp = -1, stateExp = 34816, 
    #         ALExp = [[1,3],[]], SLExp = [[4,8],[]], runExp = 1, 
    #         rewardExp = self.inFeasiblePenalty, 
    #         cumRewardExp = 0.3 - 1.0*0.7 + 1.0*0.7 + 0.5*0.3 + 5*self.inFeasiblePenalty,
    #         doneExp = False,
    #         achievedExp = False,
    #         ID = 17)

        self.takeStep(action = 1,
            stActionExp = 3, stateExp = 34880, 
            ALExp = [[1,3],[1]], SLExp = [[4,8],[3]], runExp = 1, 
            rewardExp = 1.0, 
            cumRewardExp = 0.3 - 1.0*0.7 + 1.0*0.7 + 0.5*0.3 + 1.0,
            doneExp = False,
            achievedExp = False,
            transState = "[]",
            ID = 18)

    #     self.takeStep(action = 0,
    #         stActionExp = -1, stateExp = 34880, 
    #         ALExp = [[1,3],[1]], SLExp = [[4,8],[3]], runExp = 1, 
    #         rewardExp = self.inFeasiblePenalty, 
    #         cumRewardExp = 0.3 - 1.0*0.7 + 1.0*0.7 + 0.5*0.3 + 1.0 + 6*self.inFeasiblePenalty,
    #         doneExp = False,
    #         achievedExp = False,
    #         ID = 19)

    #     self.takeStep(action = 2,
    #         stActionExp = -1, stateExp = 34880, 
    #         ALExp = [[1,3],[1]], SLExp = [[4,8],[3]], runExp = 1, 
    #         rewardExp = self.inFeasiblePenalty, 
    #         cumRewardExp = 0.3 - 1.0*0.7 + 1.0*0.7 + 0.5*0.3 + 1.0 + 7*self.inFeasiblePenalty,
    #         doneExp = False,
    #         achievedExp = False,
    #         ID = 20)
        
        self.takeStep(action = 3,
            stActionExp = 8, stateExp = 34882, 
            ALExp = [[1,3],[1,3],[]], SLExp = [[4,8],[3,8],[]], runExp = 2, 
            rewardExp = 1.0*0.7+0.5*0.3, 
            cumRewardExp = 0.3 - 1.0*0.7 + 1.0*0.7 + 0.5*0.3 + 1.0 + 1.0*0.7+0.5*0.3,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 21)
        
        
        self.takeStep(action = 2,
            stActionExp = -1, stateExp = 34882, 
            ALExp = [[1,3],[1,3],[]], SLExp = [[4,8],[3,8],[]], runExp = 2, 
            rewardExp = self.inFeasiblePenalty, 
            cumRewardExp = 0.3 - 1.0*0.7 + 1.0*0.7 + 0.5*0.3 + 1.0 + 1.0*0.7+0.5*0.3 + self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 20)
        
        self.t.reset()
        self.env.setSeed(321)

        self.takeStep(action = 1,
            stActionExp = 5, stateExp = 16384, 
            ALExp = [[1]], SLExp = [[5]], runExp = 0, 
            rewardExp = -3.2, 
            cumRewardExp = -3.2,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 22)

        self.takeStep(action = 0,
            stActionExp = -1, stateExp = 16384, 
            ALExp = [[1]], SLExp = [[5]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, 
            cumRewardExp = -3.2 + self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 23)
        
        self.takeStep(action = 1,
            stActionExp = -1, stateExp = 16384, 
            ALExp = [[1]], SLExp = [[5]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, 
            cumRewardExp = -3.2 + 2*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 24)

        self.takeStep(action = 2,
            stActionExp = -1, stateExp = 16384, 
            ALExp = [[1]], SLExp = [[5]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, 
            cumRewardExp = -3.2  + 3*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 25)

        self.takeStep(action = 3,
            stActionExp = -1, stateExp = 16384, 
            ALExp = [[1]], SLExp = [[5]], runExp = 0, 
            rewardExp = self.inFeasiblePenalty, 
            cumRewardExp = -3.2+ 4*self.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = 26)

        self.t.reset()
        self.env.setSeed(200)

        i = 100
        self.takeStep(action = 0,
            stActionExp = 0, stateExp = int(0b10000000000000000000), 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = 1.0*0.7, 
            cumRewardExp = 1.0*0.7,
            doneExp = False,
            achievedExp = False,
            transState = "[]",
            ID = i, choice = 0)
        i = i + 1

        self.takeStep(action = 2,
            stActionExp = 6, stateExp = int(0b10000010000000000000), 
            ALExp = [[0,2],[]], SLExp = [[0,6],[]], runExp = 1, 
            rewardExp = 1.0*0.7, 
            cumRewardExp = 1.0*0.7 + 1.0*0.7,
            doneExp = False,
            achievedExp = False,
            transState = "[]",
            ID = i, choice = 6)
        i = i + 1

        self.takeStep(action = 1,
            stActionExp = 4, stateExp = int(0b10000010000000100000), 
            ALExp = [[0,2],[1]], SLExp = [[0,6],[4]], runExp = 1, 
            rewardExp = -1.0*0.7 + 0.3, 
            cumRewardExp = 1.0*0.7 + 1.0*0.7 -1.0*0.7 + 0.3,
            doneExp = False,
            achievedExp = False,
            transState = "[]",
            ID = i, choice = 4)
        i = i + 1


        self.takeStep(action = 2,
            stActionExp = -1, stateExp = int(0b10000010000000100000), 
            ALExp = [[0,2],[1]], SLExp = [[0,6],[4]], runExp = 1, 
            rewardExp = self.env.inFeasiblePenalty, 
            cumRewardExp = 1.0*0.7 + 1.0*0.7 -1.0*0.7 + 0.3 + self.env.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = i, choice = 6)
        i = i + 1


        self.t.reset()
        self.env.setSeed(200)

        i = 200
        self.takeStep(action = 0,
            stActionExp = 0, stateExp = int(0b10000000000000000000), 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = 1.0*0.7, 
            cumRewardExp = 1.0*0.7,
            doneExp = False,
            achievedExp = False,
            transState = "[]",
            ID = i, choice = 0)
        i = i + 1

        self.takeStep(action = 2,
            stActionExp = 6, stateExp = int(0b10000010000000000000), 
            ALExp = [[0,2],[]], SLExp = [[0,6],[]], runExp = 1, 
            rewardExp = 1.0*0.7, 
            cumRewardExp = 1.0*0.7 + 1.0*0.7,
            doneExp = False,
            achievedExp = False,
            transState = "[]",
            ID = i, choice = 6)
        i = i + 1

        self.takeStep(action = 1,
            stActionExp = 4, stateExp = int(0b10000010000000100000), 
            ALExp = [[0,2],[1]], SLExp = [[0,6],[4]], runExp = 1, 
            rewardExp = -1.0*0.7 + 0.3, 
            cumRewardExp = 1.0*0.7 + 1.0*0.7 -1.0*0.7 + 0.3,
            doneExp = False,
            achievedExp = False,
            transState = "[]",
            ID = i, choice = 4)
        i = i + 1


        self.takeStep(action = 3,
            stActionExp = 9, stateExp = int(0b10000010000000100001), 
            ALExp = [[0,2],[1,3],[]], SLExp = [[0,6],[4,9],[]], runExp = 2, 
            rewardExp = 0.5*0.3 - 1.0*0.7, 
            cumRewardExp = 1.0*0.7 + 1.0*0.7 -1.0*0.7 + 0.3 + 0.5*0.3 - 1.0*0.7,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = i, choice = 9)
        i = i + 1

        self.t.reset()
        self.env.setSeed(200)

        i = 300
        self.takeStep(action = 0,
            stActionExp = 0, stateExp = int(0b10000000000000000000), 
            ALExp = [[0]], SLExp = [[0]], runExp = 0, 
            rewardExp = 1.0*0.7, 
            cumRewardExp = 1.0*0.7,
            doneExp = False,
            achievedExp = False,
            transState = "[]",
            ID = i, choice = 0)
        i = i + 1

        self.takeStep(action = 2,
            stActionExp = 6, stateExp = int(0b10000010000000000000), 
            ALExp = [[0,2],[]], SLExp = [[0,6],[]], runExp = 1, 
            rewardExp = 1.0*0.7, 
            cumRewardExp = 1.0*0.7 + 1.0*0.7,
            doneExp = False,
            achievedExp = False,
            transState = "[]",
            ID = i, choice = 6)
        i = i + 1

        self.takeStep(action = 1,
            stActionExp = 5, stateExp = int(0b10000010000000010000), 
            ALExp = [[0,2],[1]], SLExp = [[0,6],[5]], runExp = 1, 
            rewardExp = -5.0*0.7 + 0.3, 
            cumRewardExp = 1.0*0.7 + 1.0*0.7 -5.0*0.7 + 0.3,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = i, choice = 5)
        i = i + 1


        self.takeStep(action = 3,
            stActionExp = -1, stateExp = int(0b10000010000000010000), 
            ALExp = [[0,2],[1]], SLExp = [[0,6],[5]], runExp = 1, 
            rewardExp = self.env.inFeasiblePenalty, 
            cumRewardExp = 1.0*0.7 + 1.0*0.7 -5.0*0.7 + 0.3 + self.env.inFeasiblePenalty,
            doneExp = True,
            achievedExp = False,
            transState = "[]",
            ID = i, choice = 9)
        i = i + 1

if __name__ == '__main__':
    unittest.main()


# import scripts.simulator as sim
# import scripts.tester as test
# import unittest

# env = sim.GoalModel("./Examples/6BuildMultiRun.pl")
# env.setDebug(True)
# env.setSeed(123)
# t = test.TestIt(env)
# t.debug = False
# t.reset()
# env.setSeed(321)
# a,b,c,d,e = t.performAction(1)
# a,b,c,d,e = t.performAction(0)
# a,b,c,d,e = t.performAction(1)
# a,b,c,d,e = t.performAction(2)
# a,b,c,d,e = t.performAction(3)

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
