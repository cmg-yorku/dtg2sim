# -*- coding: utf-8 -*-
"""
Created on Mon Sep 19 11:22:43 2022

@author: Anonymous
"""


import scripts.GMEnv as sim
import scripts.Tester as test
import unittest

class TestSum(unittest.TestCase):
    simRandom = 0 
    simCustom = 0 
    simOptimal = 0 
    learningOptimal = 0
    learningParams = 0
    dtGologOptimal = -1.20853
    
    def setUp(self):
        self.env = sim.GMEnv("./examples/discrete/7HeatingMultiRun4.pl")
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
            stActionExp = 0, stateExp = int(0b1000000000000000), 
             ALExp = [[0],[]], SLExp =[[0],[]], runExp = 1,
             rewardExp = -1.92, cumRewardExp = -1.92,
             doneExp = False,
             achievedExp = False,
             transState = "[roomTemp_Inst_fl(26.0), hvac_on_fl]",
             ID = i)
        i+=1
        # SignalOn fails
        self.takeStep(action = 0,
            stActionExp = 1, stateExp = int(0b1000010000000000), 
            ALExp = [[0],[0],[]], SLExp =[[0],[1],[]], runExp = 2,
            rewardExp = -2.585, cumRewardExp = -1.92 -2.585,
            doneExp = False,
            achievedExp = False,
            transState = "[roomTemp_Inst_fl(26.95), hvac_on_fl]",
            ID = i,choice = 1)
        i+=1
        # SignalOff fails
        self.takeStep(action = 1,
            stActionExp = 3, stateExp = int(0b1000010000010000), 
            ALExp = [[0],[0],[1],[]], SLExp =[[0],[1],[3],[]], runExp = 3,
            rewardExp = -3.217, cumRewardExp = -1.92 -2.585-3.217,
            doneExp = False,
            achievedExp = False,
            transState = "[roomTemp_Inst_fl(27.8525), hvac_on_fl]",
            ID = i,choice = 3)
        i+=1
        # SignalOff finally works
        self.takeStep(action = 1,
             stActionExp = 2, stateExp = int(0b1000010000010010), 
             ALExp = [[0],[0],[1],[1],[]], SLExp =[[0],[1],[3],[2],[]], runExp = 4,
             rewardExp = -1.797075, cumRewardExp = -1.92 -2.585-3.217-1.797075,
             doneExp = True,
             achievedExp = False,
             transState = "[roomTemp_Inst_fl(25.567249999999998)]",
             ID = i,choice = 2)
        i+=1
        # SignalOff finally works
        self.takeStep(action = 0,
             stActionExp = -1, stateExp = int(0b1000010000010010), 
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
            stActionExp = 0, stateExp = int(0b1000000000000000), 
             ALExp = [[0],[]], SLExp =[[0],[]], runExp = 1,
             rewardExp = -1.92, cumRewardExp = -1.92,
             doneExp = False,
             achievedExp = False,
             transState = "[roomTemp_Inst_fl(26.0), hvac_on_fl]",
             ID = i,choice = 0)
        i+=1
        # Turn Off
        self.takeStep(action = 1,
            stActionExp = 2, stateExp = int(0b1000001000000000), 
            ALExp = [[0],[1],[]], SLExp =[[0],[2],[]], runExp = 2,
            rewardExp = -0.63, cumRewardExp = -2.55,
            doneExp = False,
            achievedExp = False,
            transState = "[roomTemp_Inst_fl(23.9)]",
            ID = i,choice = 2)
        i+=1
        # Turn On
        self.takeStep(action = 0,
            stActionExp = 0, stateExp = int(0b1000001010000000), 
            ALExp = [[0],[1],[0],[]], SLExp =[[0],[2],[0],[]], runExp = 3,
            rewardExp = -1.1885, cumRewardExp =	-3.7385,
            doneExp = False,
            achievedExp = False,
            transState = "[roomTemp_Inst_fl(24.955), hvac_on_fl]",
            ID = i,choice = 0)
        i+=1
        # Turn off
        self.takeStep(action = 1,
             stActionExp = 2, stateExp = int(0b1000001010000010), 
             ALExp = [[0],[1],[0],[1],[]], SLExp =[[0],[2],[0],[2],[]], runExp = 4,
             rewardExp = -0.02835, cumRewardExp = -3.76685,
             doneExp = True,
             achievedExp = False,
             transState = "[roomTemp_Inst_fl(22.9595)]",
             ID = i,choice = 2)
        i+=1
        # Runs are over
        self.takeStep(action = 1,
             stActionExp = -1, stateExp = int(0b1000001010000010), 
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
            stActionExp = 2, stateExp = int(0b0010000000000000), 
             ALExp = [[1],[]], SLExp =[[2],[]], runExp = 1,
             rewardExp = 0, cumRewardExp = 0,
             doneExp = False,
             achievedExp = False,
             transState = "[roomTemp_Inst_fl(23.0)]",
             ID = i,choice = 2)
        i+=1
       # Turn Off
        self.takeStep(action = 1,
            stActionExp = 3, stateExp = int(0b0010000100000000), 
            ALExp = [[1],[1],[]], SLExp =[[2],[3],[]], runExp = 2,
            rewardExp = -1.26, cumRewardExp = -1.26,
            doneExp = False,
            achievedExp = False,
            transState = "[roomTemp_Inst_fl(21.2)]",
            ID = i,choice = 3)
        i+=1
        # Turn On
        self.takeStep(action = 0,
            stActionExp = 1, stateExp = int(0b0010000101000000), 
            ALExp = [[1],[1],[0],[]], SLExp =[[2],[3],[1],[]], runExp = 3,
            rewardExp = -2.394, cumRewardExp =-3.654,
            doneExp = False,
            achievedExp = False,
            transState = "[roomTemp_Inst_fl(19.58)]",
            ID = i,choice = 1)
        i+=1
        # Turn off
        self.takeStep(action = 1,
             stActionExp = 3, stateExp = int(0b0010000101000001), 
             ALExp = [[1],[1],[0],[1],[]], SLExp =[[2],[3],[1],[3],[]], runExp = 4,
             rewardExp = -3.4146, cumRewardExp = -7.0686,
             doneExp = True,
             achievedExp = False,
             transState = "[roomTemp_Inst_fl(18.122)]",
             ID = i,choice = 3)
        i+=1
        # Runs are over
        self.takeStep(action = 0,
             stActionExp = -1, stateExp = int(0b0010000101000001), 
             ALExp = [[1],[1],[0],[1],[]], SLExp =[[2],[3],[1],[3],[]], runExp = 4,
             rewardExp = -100, cumRewardExp = -7.0686-100,
             doneExp = True,
             achievedExp = False,
             transState = "[roomTemp_Inst_fl(18.122)]",
             ID = i)
        i+=1
        
        
        

if __name__ == '__main__':
    unittest.main()


        
    # def test_semiRandonSim(self):
    #     result = self.t.simulate(10000,[1,0,1,0])
    #     TestSum.simCustom = result
    #     self.assertAlmostEqual(result,
    #               self.dtGologOptimal, 
    #               places = 1,
    #               msg = "\n Wrong simulaton result status: {} expected, {} observed".format(TestSum.dtGologOptimal,result))
            
    # def test_randonSim(self):
    #     result = self.t.simulate(10000)
    #     TestSum.simRandom = result




    # def test_learning(self):
    #     result, params = self.t.test_learning(10000,10000)
    #     TestSum.learningOptimal = result
    #     TestSum.learningParams = params
    #     self.assertAlmostEqual(result,
    #                       self.dtGologOptimal, 
    #                       places = 1,
    #                       msg = "\n Learning failed: {} expected, {} observed".format(TestSum.dtGologOptimal,result))
            
            #print(params)
        
        
    # def test_simulation(self):
    #      result = self.t.simulate(1000,[1,0,1,0,1,0])
    #      #result = self.t.simulate(1000,[0,0,0,0,0,0])
    #      correct = -3.505
    #      self.assertAlmostEqual(result,
    #                correct, 
    #                places = 1,
    #                msg = "\n Wrong simulaton result status: {} expected, {} observed".format(correct,result))



    # def concretepolicy(self):
    #     """
    #     Policy constructed based on the optimal (discrete-state) policy identified by DT-Golog.

    #     Returns
    #     -------
    #     None.

    #     """
    #     self.t.reset()
    #     tR = 0
    #     s, r, cR, d, info = self.t.performAction(1) #run1 = keepOff
    #     tR += r
    #     if info["stAction"] == 2: #run1 is success
    #         s, r, cR, d, info = self.t.performAction(0) #run2 = keepOn
    #         tR += r
    #         if info["stAction"] == 4: #run2 is success
    #             s, r, cR, d, info = self.t.performAction(1) #run3 = keepOff
    #             tR += r
    #             if info["stAction"] == 10: # run3 is success
    #                 s, r, cR, d, info = self.t.performAction(0) #run4 = keepOn
    #                 tR += r
    #                 if info["stAction"] == 12: #run4 is success
    #                     s, r, cR, d, info = self.t.performAction(1) #run5 = keepOff
    #                     tR += r
    #                     if info["stAction"] == 18: #run5 is success
    #                         s, r, cR, d, info = self.t.performAction(0) #run6 = keepOn
    #                         tR += r
    #                     else:
    #                         s, r, cR, d, info = self.t.performAction(1) #run6 = keepOff
    #                         tR += r
    #                 else: # run 4
    #                     s, r, cR, d, info = self.t.performAction(0) #run5 = keepOn
    #                     tR += r
    #                     if info["stAction"] == 16: #run5 is success
    #                         s, r, cR, d, info = self.t.performAction(0) #run6 = keepOn
    #                         tR += r
    #                     else:
    #                         s, r, cR, d, info = self.t.performAction(0) #run6 = keepOn
    #                         tR += r
    #             else: # run 3
    #                 s, r, cR, d, info = self.t.performAction(1) #run4 = keepOff
    #                 tR += r
    #                 if info["stAction"] == 14: #run4 is success
    #                     s, r, cR, d, info = self.t.performAction(0) #run5 = keepOn
    #                     tR += r
    #                     if info["stAction"] == 16: #run5 is success
    #                         s, r, cR, d, info = self.t.performAction(1) #run6 = keepOff
    #                         tR += r
    #                     else:
    #                         s, r, cR, d, info = self.t.performAction(0) #run6 = keepOn
    #                         tR += r
    #                 else:
    #                     s, r, cR, d, info = self.t.performAction(1) #run5 = keepOff
    #                     tR += r
    #                     if info["stAction"] == 18: #run5 is success
    #                         s, r, cR, d, info = self.t.performAction(1) #run6 = keepOff
    #                         tR += r
    #                     else: 
    #                         s, r, cR, d, info = self.t.performAction(1) #run6 = keepOff
    #                         tR += r
    #         else: # run2 fails
    #             s, r, cR, d, info = self.t.performAction(0) #run3 = keepOn
    #             tR += r
    #             if info["stAction"] == 8: #run3 is success
    #                 s, r, cR, d, info = self.t.performAction(0) #run4 = keepOn
    #                 tR += r
    #                 if info["stAction"] == 12: #run4 is success
    #                     s, r, cR, d, info = self.t.performAction(1) #run5 = keepOff
    #                     tR += r
    #                     if info["stAction"] == 18: #run5 is success
    #                         s, r, cR, d, info = self.t.performAction(0) #run6 = keepOn
    #                         tR += r
    #                     else:
    #                         s, r, cR, d, info = self.t.performAction(1) #run6 = keepOff
    #                         tR += r
    #                 else:
    #                     s, r, cR, d, info = self.t.performAction(0) #run5 = keepOn
    #                     tR += r
    #                     if info["stAction"] == 16: #run5 is sucecess
    #                         s, r, cR, d, info = self.t.performAction(0) #run6 = keepOn
    #                         tR += r
    #                     else:
    #                         s, r, cR, d, info = self.t.performAction(0) #run6 = keepOn
    #                         tR += r
    #             else:
    #                 s, r, cR, d, info = self.t.performAction(0) #run4 = keepOn
    #                 tR += r
    #                 if info["stAction"] == 12: #run4 is success
    #                     s, r, cR, d, info = self.t.performAction(0) #run5 = keepOn
    #                     tR += r
    #                     if info["stAction"] == 16: #run5 is success
    #                         s, r, cR, d, info = self.t.performAction(0) #run6 = keepOn
    #                         tR += r
    #                     else:
    #                         s, r, cR, d, info = self.t.performAction(0) #run6 = keepOn
    #                         tR += r
    #                 else:
    #                     s, r, cR, d, info = self.t.performAction(0) #run5 = keepOn
    #                     tR += r
    #                     if info["stAction"] == 16: #run5 is success
    #                         s, r, cR, d, info = self.t.performAction(0) #run6 = keepOn
    #                         tR += r
    #                     else:
    #                         s, r, cR, d, info = self.t.performAction(0) #run6 = keepOn
    #                         tR += r
    #     else: # run1 is fail
    #         s, r, cR, d, info = self.t.performAction(1) #run2 = keepOff
    #         tR += r
    #         if info["stAction"] == 6: #run2 is success
    #             s, r, cR, d, info = self.t.performAction(1) #run3 = keepOff
    #             tR += r
    #             if info["stAction"] == 10: #run3 is success
    #                 s, r, cR, d, info = self.t.performAction(0) #run4 = keepOn
    #                 tR += r
    #                 if info["stAction"] == 12: #run4 is success
    #                     s, r, cR, d, info = self.t.performAction(0) #run5 = keepOn
    #                     tR += r
    #                     if info["stAction"] == 16: #run5 is success
    #                         s, r, cR, d, info = self.t.performAction(1) #run6 = keepOff
    #                         tR += r
    #                     else:
    #                         s, r, cR, d, info = self.t.performAction(0) #run6 = keepOn
    #                         tR += r
    #                 else: # run 4
    #                     s, r, cR, d, info = self.t.performAction(0) #run5 = keepOn
    #                     tR += r
    #                     if info["stAction"] == 16: #run5 is success
    #                         s, r, cR, d, info = self.t.performAction(0) #run6 = keepOn
    #                         tR += r
    #                     else:
    #                         s, r, cR, d, info = self.t.performAction(0) #run6 = keepOn
    #                         tR += r
    #             else: # run 3
    #                 s, r, cR, d, info = self.t.performAction(1) #run4 = keepOff
    #                 tR += r
    #                 if info["stAction"] == 14: #run4 is success
    #                     s, r, cR, d, info = self.t.performAction(0) #run5 = keepOn
    #                     tR += r
    #                     if info["stAction"] == 16: #run5 is success
    #                         s, r, cR, d, info = self.t.performAction(1) #run6 = keepOff
    #                         tR += r
    #                     else:
    #                         s, r, cR, d, info = self.t.performAction(0) #run6 = keepOn
    #                         tR += r
    #                 else:
    #                     s, r, cR, d, info = self.t.performAction(1) #run5 = keepOff
    #                     tR += r
    #                     if info["stAction"] == 18: #run5 is success
    #                         s, r, cR, d, info = self.t.performAction(1) #run6 = keepOff
    #                         tR += r
    #                     else:
    #                         s, r, cR, d, info = self.t.performAction(1) #run6 = keepOff
    #                         tR += r
    #         else: # run2
    #             s, r, cR, d, info = self.t.performAction(1) #run3 = keepOff
    #             tR += r
    #             if info["stAction"] == 10: #run4 is success
    #                 s, r, cR, d, info = self.t.performAction(1) #run4 = keepOff
    #                 tR += r
    #                 if info["stAction"] == 14: #run4 is success
    #                     s, r, cR, d, info = self.t.performAction(0) #run5 = keepOn
    #                     tR += r
    #                     if info["stAction"] == 16: #run5 is success
    #                         s, r, cR, d, info = self.t.performAction(1) #run6 = keepOff
    #                         tR += r
    #                     else:
    #                         s, r, cR, d, info = self.t.performAction(0) #run6 = keepOn
    #                         tR += r
    #                 else:
    #                     s, r, cR, d, info = self.t.performAction(1) #run5 = keepOff
    #                     tR += r
    #                     if info["stAction"] == 18: #run5 is success
    #                         s, r, cR, d, info = self.t.performAction(1) #run6 = keepOff
    #                         tR += r
    #                     else:
    #                         s, r, cR, d, info = self.t.performAction(1) #run6 = keepOff
    #                         tR += r
    #             else:
    #                 s, r, cR, d, info = self.t.performAction(1) #run4 = keepOff
    #                 tR += r
    #                 if info["stAction"] == 14: #run4 is success
    #                     s, r, cR, d, info = self.t.performAction(1) #run5 = keepOff
    #                     tR += r
    #                     if info["stAction"] == 18: #run5 is success
    #                         s, r, cR, d, info = self.t.performAction(0) #run6 = keepOn
    #                         tR += r
    #                     else:
    #                         s, r, cR, d, info = self.t.performAction(1) #run6 = keepOff
    #                         tR += r
    #                 else:
    #                     s, r, cR, d, info = self.t.performAction(1) #run5 = keepOff
    #                     tR += r
    #                     if info["stAction"] == 18: #run5 is success
    #                         s, r, cR, d, info = self.t.performAction(1) #run6 = keepOff
    #                         tR += r
    #                     else:
    #                         s, r, cR, d, info = self.t.performAction(1) #run6 = keepOff
    #                         tR += r
    #     #print("Total Reward: {} and {}".format(cR,tR))
    #     # Returs the total reward from one execution of the policy. 
    #     # May vary due to the stochastic nature of 'performAction(task)'.
    #     return(tR)



    # def test_learning(self):
    #     #env = sim.GMEnv("./Examples/7Heating.pl")
    #     #model = A2C("MlpPolicy", env, verbose=1)
    #     model = A2C("MlpPolicy", self.env, verbose=1)
    #     params = model.get_parameters().get("policy.optimizer").get("param_groups")
    #     model.learn(total_timesteps=100_000)

    #     vec_env = model.get_env()
    #     obs = vec_env.reset()
    #     totalReward = 0
    #     totalIter = 10000
    #     for i in range(totalIter):
    #         obs = vec_env.reset()
    #         episodeDone = False
    #         episodeReward = 0
    #         #print("Iteration:{}".format(i))
    #         while (not(episodeDone)):
    #             action, _state = model.predict(obs, deterministic=True)
    #             obs, reward, done, info = vec_env.step(action)
    #             episodeReward = episodeReward + reward[0]
    #             episodeDone = done[0]
    #         episodeReward
    #         totalReward  = totalReward + episodeReward 
    #     result = totalReward/totalIter
    #     TestSum.learningOptimal = result
    #     TestSum.learningParams = params
        
    #     self.assertAlmostEqual(result,
    #                       TestSum.dtGologOptimal, 
    #                       places = 1,
    #                       msg = "\n Learning failed: {} expected, {} observed".format(TestSum.dtGologOptimal,result))
 



    # def test_advancedpolicy(self):
    #     totalR = 0
    #     episodes = 10000
    #     for i in range(1,episodes) :
    #         totalR += self.concretepolicy()
    #     result = totalR/episodes
    #     TestSum.simOptimal = result
    #     self.assertAlmostEqual(TestSum.dtGologOptimal,
    #                      result,
    #                      places = 2,
    #                      msg = "\n (Hardcoded Policy) - Wrong reward: {} expected, {} observed".format(TestSum.dtGologOptimal,result))


    # @classmethod
    # def tearDownClass(cls):
    #     print('DT-Golog - calculated policy reward: {}'.format(cls.dtGologOptimal))
    #     print('DT-Golog - simulated policy reward: {}'.format(cls.simOptimal))
    #     print('Learned policy reward..............: {}'.format(cls.learningOptimal))
    #     print('--> Learning Parameters: \n {}'.format(cls.learningParams))





# import scripts.simulator as sim
# import scripts.tester as test

#import scripts2.GMEnv as sim
#import scripts2.Tester as test

#env = sim.GMEnv("./Examples2/7HeatingMultiRun4.pl")
#env.setDebug(True)
#t = test.TestIt(env)
#t.debug = False
#t.performAction(13)

# s,r,d,i = env.step(1)
# s,r,d,i = env.step(0)
# s,r,d,i = env.step(1)
# s,r,d,i = env.step(0)


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