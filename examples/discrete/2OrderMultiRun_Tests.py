# -*- coding: utf-8 -*-
"""
Created on Mon Sep 19 11:22:43 2022

@author: Anonymous
"""


import scripts.GMEnv as sim
import scripts.Tester as test
import unittest


class TestSum(unittest.TestCase):
    simOptimal = 0 
    simRandom = 0
    learningOptimal = 0
    learningParams = 0
    dtGologOptimal = 1.708475


    def setUp(self):
        self.env = sim.GMEnv("./examples/discrete/2OrderMultiRun.pl")
        self.env.setDebug(False)
        self.env.setSeed(123)
        self.t = test.TestIt(self.env)
        self.t.debug = False
        self.t.reset()

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
         self.assertEqual(rewardExp,
                          rewardObs, 
                          msg = "\n (TestID: {}) - Wrong reward: {} expected, {} observed".format(ID,rewardExp,rewardObs))
         self.assertEqual(cumRewardExp,
                          cumRewardObs, 
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
         
    def test_debug(self):
        
        self.takeStep(action = 0,
            stActionExp = 0, stateExp = 2048, 
            ALExp = [[0],[]], SLExp = [[0],[]], runExp = 1, 
            rewardExp = 0.5*0.7+1.0*0.3, cumRewardExp = 0.5*0.7+1.0*0.3,
            doneExp = False, achievedExp = False,
            transState= "[]",
            ID = 1)

        self.takeStep(action = 0,
            stActionExp = 0, stateExp = 2080, 
            ALExp = [[0],[0],[]], SLExp =[[0],[0],[]], runExp = 2, 
            rewardExp = 0.5*0.7+1.0*0.3, cumRewardExp = (0.5*0.7+1.0*0.3)*2,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = 2)

        self.takeStep(action = 1,
            stActionExp = -1, stateExp = 2080, 
            ALExp = [[0],[0],[]], SLExp = [[0],[0],[]], runExp = 2, 
            rewardExp = self.env.inFeasiblePenalty, cumRewardExp = (0.5*0.7+1.0*0.3)*2 + self.env.inFeasiblePenalty,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = 3)

        self.takeStep(action = 0,
            stActionExp = -1, stateExp = 2080, 
            ALExp = [[0],[0],[]], SLExp = [[0],[0],[]], runExp = 2, 
            rewardExp = self.env.inFeasiblePenalty, cumRewardExp = (0.5*0.7+1.0*0.3)*2 + 2*self.env.inFeasiblePenalty,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = 4)

        self.takeStep(action = 2,
            stActionExp = -1, stateExp = 2080, 
            ALExp = [[0],[0],[]], SLExp = [[0],[0],[]], runExp = 2, 
            rewardExp = self.env.inFeasiblePenalty, 
            cumRewardExp = (0.5*0.7+1.0*0.3)*2 + 3*self.env.inFeasiblePenalty,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = 5)
        
        self.t.reset()
        self.env.setSeed(15)
        
        self.takeStep(action = 1,
            stActionExp = 4, stateExp = 128, 
            ALExp = [[1],[]], SLExp = [[4],[]], runExp = 1, 
            rewardExp = 1.0*0.7+0.7*0.3, cumRewardExp = 1.0*0.7+0.7*0.3,
            doneExp = False, achievedExp = False,
            transState= "[]",
            ID = 6)

        self.takeStep(action = 0,
            stActionExp = 0, stateExp = 160, 
            ALExp = [[1],[0],[]], SLExp = [[4],[0],[]], runExp = 2, 
            rewardExp = 0.5*0.7+1.0*0.3, cumRewardExp = 1.0*0.7+0.7*0.3 + 0.5*0.7+1.0*0.3,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = 7)

        self.takeStep(action = 1,
            stActionExp = -1, stateExp = 160, 
            ALExp = [[1],[0],[]], SLExp = [[4],[0],[]], runExp = 2, 
            rewardExp = self.env.inFeasiblePenalty, 
            cumRewardExp = 1.0*0.7+0.7*0.3 + 0.5*0.7+1.0*0.3 + self.env.inFeasiblePenalty,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = 8)
    
        self.t.reset()
        self.env.setSeed(321)
        
        self.takeStep(action = 1,
            stActionExp = 5, stateExp = 64, 
            ALExp = [[1]], SLExp = [[5]], runExp = 0, 
            rewardExp = 0.7, cumRewardExp = 0.7,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = 9)
    
        self.takeStep(action = 0,
            stActionExp = -1, stateExp = 64, 
            ALExp = [[1]], SLExp = [[5]], runExp = 0, 
            rewardExp = self.env.inFeasiblePenalty, cumRewardExp = 0.7 + self.env.inFeasiblePenalty,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = 10)

        self.takeStep(action = 1,
            stActionExp = -1, stateExp = 64, 
            ALExp = [[1]], SLExp = [[5]], runExp = 0, 
            rewardExp = self.env.inFeasiblePenalty, cumRewardExp = 0.7 + 2*self.env.inFeasiblePenalty,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = 11)

    def test_random(self):
        
        #Test 1
        self.t.reset()
        i = 100
        self.takeStep(action = 0,
            stActionExp = 0, stateExp = 2048, 
            ALExp = [[0],[]], SLExp = [[0],[]], runExp = 1, 
            rewardExp = 0.5*0.7+0.3, cumRewardExp = 0.5*0.7+0.3,
            doneExp = False, achievedExp = False,
            transState= "[]",
            ID = i,choice = 0)
        i += 1
        self.takeStep(action = 1,
            stActionExp = 3, stateExp = 2048+4, 
            ALExp = [[0],[1],[]], SLExp = [[0],[3],[]], runExp = 2, 
            rewardExp = 1.0, cumRewardExp = 0.5*0.7+0.3 +1,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = i,choice = 3)
        i += 1
        self.takeStep(action = 0,
            stActionExp = -1, stateExp = 2048+4, 
            ALExp = [[0],[1],[]], SLExp = [[0],[3],[]], runExp = 2, 
            rewardExp = self.env.inFeasiblePenalty, cumRewardExp = 0.5*0.7+0.3 +1+self.env.inFeasiblePenalty,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = i,choice = 0)
        i += 1                

        
        #Test 2
        self.t.reset()
        i = 200
        self.takeStep(action = 0,
            stActionExp = 0, stateExp = 2048, 
            ALExp = [[0],[]], SLExp = [[0],[]], runExp = 1, 
            rewardExp = 0.5*0.7+0.3, cumRewardExp = 0.5*0.7+0.3,
            doneExp = False, achievedExp = False,
            transState= "[]",
            ID = i,choice = 0)
        i += 1
        self.takeStep(action = 0,
            stActionExp = 2, stateExp = 2048+8, 
            ALExp = [[0],[0]], SLExp = [[0],[2]], runExp = 1, 
            rewardExp = 0.5*0.7 , cumRewardExp = 0.5*0.7+0.3 + 0.5*0.7,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = i,choice = 2)
        i += 1
        self.takeStep(action = 0,
            stActionExp = -1, stateExp = 2048+8, 
            ALExp = [[0],[0]], SLExp = [[0],[2]], runExp = 1, 
            rewardExp = self.env.inFeasiblePenalty, cumRewardExp = 0.5*0.7+0.3 + 0.5*0.7 + self.env.inFeasiblePenalty,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = i,choice = 0)
        i += 1

        #Test 3
        self.t.reset()
        i = 300
        self.takeStep(action = 0,
            stActionExp = 1, stateExp = 1024, 
            ALExp = [[0],[]], SLExp = [[1],[]], runExp = 1, 
            rewardExp = 0.5*0.7+0.3*0.7, cumRewardExp = 0.5*0.7+0.3*0.7,
            doneExp = False, achievedExp = False,
            transState= "[]",
            ID = i,choice = 1)
        i += 1
        self.takeStep(action = 0,
            stActionExp = 0, stateExp = 1024+32, 
            ALExp = [[0],[0],[]], SLExp = [[1],[0],[]], runExp = 2, 
            rewardExp = 0.5*0.7+0.3, cumRewardExp = 0.5*0.7+0.3*0.7 + 0.5*0.7+0.3,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = i,choice = 0)
        i += 1
        self.takeStep(action = 1,
            stActionExp = -1, stateExp = 1024+32, 
            ALExp = [[0],[0],[]], SLExp = [[1],[0],[]], runExp = 2, 
            rewardExp = self.env.inFeasiblePenalty, cumRewardExp = 0.5*0.7+0.3*0.7 + 0.5*0.7+0.3 + self.env.inFeasiblePenalty,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = i,choice = 4)
        i += 1
        
        #Test 4
        self.t.reset()
        i = 400
        self.takeStep(action = 0,
            stActionExp = 1, stateExp = 1024, 
            ALExp = [[0],[]], SLExp = [[1],[]], runExp = 1, 
            rewardExp = 0.5*0.7+0.3*0.7, cumRewardExp = 0.5*0.7+0.3*0.7,
            doneExp = False, achievedExp = False,
            transState= "[]",
            ID = i,choice = 1)
        i += 1
        self.takeStep(action = 0,
            stActionExp = 2, stateExp = 1024 + 8, 
            ALExp = [[0],[0]], SLExp = [[1],[2]], runExp = 1, 
            rewardExp = 0.5*0.7, cumRewardExp = 0.5*0.7+0.3*0.7 + 0.5*0.7,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = i,choice = 2)
        i += 1
        self.takeStep(action = 0,
            stActionExp = -1, stateExp = 1024 + 8, 
            ALExp = [[0],[0]], SLExp = [[1],[2]], runExp = 1, 
            rewardExp = self.env.inFeasiblePenalty, cumRewardExp = 0.5*0.7+0.3*0.7 + 0.5*0.7 + self.env.inFeasiblePenalty,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = i,choice = 6)
        i += 1

        #Test 5
        self.t.reset()
        i = 500
        self.takeStep(action = 0,
            stActionExp = 2, stateExp = 512, 
            ALExp = [[0]], SLExp = [[2]], runExp = 0, 
            rewardExp = 0.5*0.7, cumRewardExp = 0.5*0.7,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = i,choice = 2)
        i += 1
        self.takeStep(action = 0,
            stActionExp = -1, stateExp = 512, 
            ALExp = [[0]], SLExp = [[2]], runExp = 0, 
            rewardExp = self.env.inFeasiblePenalty, cumRewardExp = 0.5*0.7 + self.env.inFeasiblePenalty,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = i,choice = 1)
        i += 1
        self.takeStep(action = 0,
            stActionExp = -1, stateExp = 512, 
            ALExp = [[0]], SLExp = [[2]], runExp = 0, 
            rewardExp = self.env.inFeasiblePenalty, cumRewardExp = 0.5*0.7 + 2*self.env.inFeasiblePenalty,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = i,choice = 2)
        i += 1

        #Test 6
        self.t.reset()
        i = 600
        self.takeStep(action = 0,
            stActionExp = 2, stateExp = 512, 
            ALExp = [[0]], SLExp = [[2]], runExp = 0, 
            rewardExp = 0.5*0.7, cumRewardExp = 0.5*0.7,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = i,choice = 2)
        i += 1
        self.takeStep(action = 0,
            stActionExp = -1, stateExp = 512, 
            ALExp = [[0]], SLExp = [[2]], runExp = 0, 
            rewardExp = self.env.inFeasiblePenalty, cumRewardExp = 0.5*0.7 + self.env.inFeasiblePenalty,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = i,choice = 6)
        i += 1
        self.takeStep(action = 1,
            stActionExp = -1, stateExp = 512, 
            ALExp = [[0]], SLExp = [[2]], runExp = 0, 
            rewardExp =self.env.inFeasiblePenalty, cumRewardExp = 0.5*0.7 + 2*self.env.inFeasiblePenalty,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = i,choice = 3)

        #Test 7
        self.t.reset()
        i = 700
        self.takeStep(action = 1,
            stActionExp = 3, stateExp = 256, 
            ALExp = [[1],[]], SLExp = [[3],[]], runExp = 1, 
            rewardExp = 1.0*0.7+1.0*0.3, cumRewardExp = 1.0*0.7+1.0*0.3,
            doneExp = False, achievedExp = False,
            transState= "[]",
            ID = i,choice = 3)
        i += 1
        self.takeStep(action = 1,
            stActionExp = 5, stateExp = 256+1, 
            ALExp = [[1],[1]], SLExp = [[3],[5]], runExp = 1, 
            rewardExp = 0.7, cumRewardExp = 1.0*0.7+1.0*0.3+0.7,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = i,choice = 5)
        i += 1
        self.takeStep(action = 0,
            stActionExp = -1, stateExp = 256+1, 
            ALExp = [[1],[1]], SLExp = [[3],[5]], runExp = 1, 
            rewardExp = self.env.inFeasiblePenalty, cumRewardExp = 1.0*0.7+1.0*0.3+0.7 + self.env.inFeasiblePenalty,
            doneExp = True, achievedExp = False,
            transState= "[]",
            ID = i,choice = 2)
        i += 1
        
        #Test 8
        self.t.reset()
        i = 800
        self.takeStep(action = 1,
                      stActionExp = 3, stateExp = 256, 
                      ALExp = [[1],[]], SLExp = [[3],[]], runExp = 1, 
                      rewardExp = 1.0*0.7+1.0*0.3, cumRewardExp = 1.0*0.7+1.0*0.3,
                      doneExp = False, achievedExp = False,
                      transState= "[]",
                      ID = i,choice = 3)
        i += 1
        self.takeStep(action = 0,
                      stActionExp = 1, stateExp = 256+16, 
                      ALExp = [[1],[0],[]], SLExp = [[3],[1],[]], runExp = 2, 
                      rewardExp = 0.5*0.7+0.7*0.3, cumRewardExp = 1.0*0.7+1.0*0.3 + 0.5*0.7+0.7*0.3,
                      doneExp = True, achievedExp = False,
                      transState= "[]",
                      ID = i,choice = 1)
        i += 1
        self.takeStep(action = 0,
                      stActionExp = -1, stateExp = 256+16, 
                      ALExp = [[1],[0],[]], SLExp = [[3],[1],[]], runExp = 2, 
                      rewardExp = self.env.inFeasiblePenalty, cumRewardExp = 1.0*0.7+1.0*0.3 + 0.5*0.7+0.7*0.3 + self.env.inFeasiblePenalty,
                      doneExp = True, achievedExp = False,
                      transState= "[]",
                      ID = i,choice = 6)
        i += 1

        #Test 9
        self.t.reset()
        i = 900
        self.takeStep(action = 1,
                      stActionExp = 4, stateExp = 128, 
                      ALExp = [[1],[]], SLExp = [[4],[]], runExp = 1, 
                      rewardExp = 1.0*0.7+0.7*0.3, cumRewardExp = 1.0*0.7+0.7*0.3,
                      doneExp = False, achievedExp = False,
                      transState= "[]",
                      ID = i,choice = 4)
        i += 1
        self.takeStep(action = 0,
                      stActionExp = 0, stateExp = 128+32, 
                      ALExp = [[1],[0],[]], SLExp = [[4],[0],[]], runExp = 2, 
                      rewardExp = 0.5*0.7+1.0*0.3, cumRewardExp = 1.0*0.7+0.7*0.3 + 0.5*0.7+1.0*0.3,
                      doneExp = True, achievedExp = False,
                      transState= "[]",
                      ID = i,choice = 0)
        i += 1
        self.takeStep(action = 1,
                      stActionExp = -1, stateExp = 128+32, 
                      ALExp = [[1],[0],[]], SLExp = [[4],[0],[]], runExp = 2, 
                      rewardExp = self.env.inFeasiblePenalty, cumRewardExp = 1.0*0.7+0.7*0.3 + 0.5*0.7+1.0*0.3 + self.env.inFeasiblePenalty,
                      doneExp = True, achievedExp = False,
                      transState= "[]",
                      ID = i,choice = 9)

        #Test 10
        self.t.reset()
        i = 1000
        self.takeStep(action = 1,
                      stActionExp = 4, stateExp = 128, 
                      ALExp = [[1],[]], SLExp = [[4],[]], runExp = 1, 
                      rewardExp = 1.0*0.7+0.7*0.3, cumRewardExp = 1.0*0.7+0.7*0.3,
                      doneExp = False, achievedExp = False,
                      transState= "[]",
                      ID = i,choice = 4)    
        i+=1
        self.takeStep(action = 0,
                      stActionExp = 2, stateExp = 128+8, 
                      ALExp = [[1],[0]], SLExp = [[4],[2]], runExp = 1, 
                      rewardExp = 0.5*0.7+0.0*0.3, cumRewardExp = 1.0*0.7+0.7*0.3 + 0.5*0.7+0.0*0.3,
                      doneExp = True, achievedExp = False,
                      transState= "[]",
                      ID = i,choice = 2)
        i+=1
        self.takeStep(action = 0,
                      stActionExp = -1, stateExp = 128+8, 
                      ALExp = [[1],[0]], SLExp = [[4],[2]], runExp = 1, 
                      rewardExp = self.env.inFeasiblePenalty, cumRewardExp = 1.0*0.7+0.7*0.3 + 0.5*0.7+0.0*0.3 + self.env.inFeasiblePenalty,
                      doneExp = True, achievedExp = False,
                      transState= "[]",
                      ID = i,choice = 9)

        #Test 11
        self.t.reset()
        i = 1100
        self.takeStep(action = 1,
                      stActionExp = 5, stateExp = 64, 
                      ALExp = [[1]], SLExp = [[5]], runExp = 0, 
                      rewardExp = 1.0*0.7+0.0*0.3, cumRewardExp = 1.0*0.7+0.0*0.3,
                      doneExp = True, achievedExp = False,
                      transState= "[]",
                      ID = i,choice = 5) 
        i+=1
        self.takeStep(action = 1,
                      stActionExp = -1, stateExp = 64, 
                      ALExp = [[1]], SLExp = [[5]], runExp = 0, 
                      rewardExp = self.env.inFeasiblePenalty, cumRewardExp = 1.0*0.7+0.0*0.3 + self.env.inFeasiblePenalty,
                      doneExp = True, achievedExp = False,
                      transState= "[]",
                      ID = i,choice = 10) 
        i+=1
        self.takeStep(action = 1,
                      stActionExp = -1, stateExp = 64, 
                      ALExp = [[1]], SLExp = [[5]], runExp = 0, 
                      rewardExp = self.env.inFeasiblePenalty, cumRewardExp = 1.0*0.7+0.0*0.3 + 2*self.env.inFeasiblePenalty,
                      doneExp = True, achievedExp = False,
                      transState= "[]",
                      ID = i,choice = 11) 
        #Test 12
        self.t.reset()
        i = 1200
        self.takeStep(action = 1,
                      stActionExp = 5, stateExp = 64, 
                      ALExp = [[1]], SLExp = [[5]], runExp = 0, 
                      rewardExp = 1.0*0.7+0.0*0.3, cumRewardExp = 1.0*0.7+0.0*0.3,
                      doneExp = True, achievedExp = False,
                      transState= "[]",
                      ID = i,choice = 5) 
        i+=1
        self.takeStep(action = 1,
                      stActionExp = -1, stateExp = 64, 
                      ALExp = [[1]], SLExp = [[5]], runExp = 0, 
                      rewardExp = self.env.inFeasiblePenalty, cumRewardExp = 1.0*0.7+0.0*0.3 + self.env.inFeasiblePenalty,
                      doneExp = True, achievedExp = False,
                      transState= "[]",
                      ID = i,choice = 10) 
        i+=1
        self.takeStep(action = 0,
                      stActionExp = -1, stateExp = 64, 
                      ALExp = [[1]], SLExp = [[5]], runExp = 0, 
                      rewardExp = self.env.inFeasiblePenalty, cumRewardExp = 1.0*0.7+0.0*0.3 + 2*self.env.inFeasiblePenalty,
                      doneExp = True, achievedExp = False,
                      transState= "[]",
                      ID = i,choice = 0)


if __name__ == '__main__':
    unittest.main()




    # def test_learning(self):
    #     model = A2C("MlpPolicy", self.env, verbose=1)
    #     model.learn(total_timesteps=10_000)
    #     vec_env = model.get_env()
    #     obs = vec_env.reset()
        
    #     totalReward = 0
    #     totalIter = 10000
    #     print("Learning Complete. Starting testing..")
    #     for i in range(totalIter):
    #         sys.stdout.write("\r\t%d%%" % ((i/totalIter)*100))
    #         sys.stdout.flush()
    #         obs = vec_env.reset()
    #         episodeDone = False
    #         episodeReward = 0 
    #         while (not(episodeDone)):
    #             action, _state = model.predict(obs, deterministic=True)
    #             obs, reward, done, info = vec_env.step(action)
    #             episodeReward = episodeReward + reward[0]
    #             episodeDone = done[0]
    #         totalReward  = totalReward + episodeReward 

    #     result = totalReward/totalIter
    #     self.assertAlmostEqual(result,
    #                       self.dtGologOptimal, 
    #                       places = 1,
    #                       msg = "\n Learning failed: {} expected, {} observed".format(self.dtGologOptimal,result))
    #     print('Learning: optimal policy reward: {}'.format(result))
    #     params = model.get_parameters().get("policy.optimizer").get("param_groups")
    #     print(params)


# env = sim.GMEnv("./examples2/2OrderMultiRun.pl")

# model = A2C("MlpPolicy", env, verbose=1)
# model.learn(total_timesteps=1_000)
# vec_env = model.get_env()
# obs = vec_env.reset()

        


# import scripts.simulator as sim
# import scripts.tester as test

# env = sim.GoalModel("./Examples/2OrderMultiRun.pl")
# env.setDebug(True)
# env.setSeed(123)
# t = test.TestIt(env)
# t.debug = False
# t.reset()
# s,r,d,i = t.env.step(0,0)
# s,r,d,i = t.env.step(1)

# t.reset()
# env.setSeed(321)
# a,b,c,d,e = t.performAction(1)

# t.simulate(100)

