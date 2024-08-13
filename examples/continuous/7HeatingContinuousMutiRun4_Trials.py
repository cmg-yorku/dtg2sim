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
    
    simRandomIter = 100
    simCustomIter = 100
    simOptimalIter = 100
    testingIter = 100
    
   
    trainingIter = 1000
    learningAlgorithm = "A2C"
    learningLoggingInterval = 500
    
    learningOptimal = 0
    learningParams = 0
    #dtGologOptimal = -1.20853
    dtGologOptimal = -1.360857084
    
    def setUp(self):
        self.env = sim.GMEnv("./examples/continuous/7HeatingContinuousMultiRun4.pl")
        self.env.setDebug(False)
        self.env.setSeed(1223)
        self.t = test.TestIt(self.env)
        self.t.debug = False
        self.inFeasiblePenalty = self.env.getInfeasiblePenalty()

    def tearDown(self):
        self.env.closeQE()
        
    def test_semiRandonSim(self):
        """
        A simulation based on a crude policy that assumes certain success of actions.

        Returns
        -------
        None.

        """
        result = self.t.simulate(self.simCustomIter,[1,0,1,0])
        TestSum.simCustom = result
            
    def test_randonSim(self):
        """
        A random policy (actions are picked randomly)

        Returns
        -------
        None.

        """
        result = self.t.simulate(self.simRandomIter)
        TestSum.simRandom = result
    
    
    def test_learning(self):
        """
        Training and testing an RL agent

        Returns
        -------
        None.

        """
        result, params = self.t.test_learning(self.trainingIter,self.testingIter, logging = self.learningLoggingInterval, algo = self.learningAlgorithm)
        TestSum.learningOptimal = result
        TestSum.learningParams = params
        self.assertAlmostEqual(result,
                          self.dtGologOptimal, 
                          places = 0,
                          msg = "\n Learning failed: {} expected, {} observed".format(TestSum.dtGologOptimal,result))
            

    @classmethod
    def tearDownClass(cls):
        print('DT-Golog - calculated policy reward: {}'.format(cls.dtGologOptimal))
        #print('DT-Golog - simulated policy reward : {}'.format(cls.simOptimal))
        print('Partially optimal policy reward....: {}'.format(cls.simCustom))
        print('Random policy reward.............. : {}'.format(cls.simRandom))
        print('Learned policy reward..............: {}'.format(cls.learningOptimal))
        print('--> Learning Parameters: \n {}'.format(cls.learningParams))







if __name__ == '__main__':
    unittest.main()




            

        
        
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