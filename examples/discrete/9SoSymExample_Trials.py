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
    simRandom = 0 
    simRandomForgive = 0 
    simCustom = 0 
    simOptimal = 0 
    
    simRandomIter = 10
    simCustomIter = 10
    simOptimalIter = 10
    testingIter = 1000
    
   
    trainingIter = 1000
    learningAlgorithm = "PPO"
    learningLoggingInterval = 500
    
    learningOptimal = 0
    learningParams = 0
    dtGologOptimal = 0.7910615000000001    

    def setUp(self):
        self.env = sim.GMEnv("./examples/discrete/9SoSymExample.pl")
        self.env.setDebug(False)
        self.env.setSeed(123)
        self.t = test.TestIt(self.env)
        self.t.debug = False
        self.inFeasiblePenalty = self.env.getInfeasiblePenalty()
        
    def tearDown(self):
        self.env.closeQE()
        
    def test_optimalSim(self):
        """
        A simulation based on a crude policy that assumes certain success of actions.
  
        Returns
        -------
        None.
  
        """
        result = self.t.simulate(self.simOptimalIter,[0,3,5])
        TestSum.simOptimal = result
            
    def test_randonSimForgive(self):
       """
       A random policy (actions are picked randomly)

       Returns
       -------
       None.

       """
       result = self.t.simulate(self.simRandomIter)
       TestSum.simRandom = result
   
    def test_randonSim(self):
       """
       A random policy (actions are picked randomly)

       Returns
       -------
       None.

       """
       result = self.t.simulate(self.simRandomIter,forgivePenalty=False)
       TestSum.simRandomForgive = result
    
    
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
        print('DT-Golog - simulated policy reward.: {}'.format(cls.simOptimal))
        print('Random simulated policy reward.....: {}'.format(cls.simRandom))
        print('Random simulated policy reward (fg): {}'.format(cls.simRandomForgive))
        print('Learned policy reward..............: {}'.format(cls.learningOptimal))
        print('--> Learning Parameters: \n {}'.format(cls.learningParams))


if __name__ == '__main__':
    unittest.main()

			
    # def test_simulation(self):
    #     result = self.t.simulate(1000,[0,3,5])
    #     TestSum.simOptimal = result
    #     self.assertAlmostEqual(result,
    #               self.dtGologOptimal, 
    #               places = 1,
    #               msg = "\n Wrong simulaton result status: {} expected, {} observed".format(TestSum.dtGologOptimal,result))

    # def test_randomSim(self):
    #     result = self.t.simulate(1000)
    #     TestSum.simRandom = result


    # def test_learning(self):
    #     result, params = self.t.test_learning(10000,10000)
    #     TestSum.learningOptimal = result
    #     TestSum.learningParams = params
    #     self.assertAlmostEqual(result,
    #                       self.dtGologOptimal, 
    #                       places = 1,
    #                       msg = "\n Learning failed: {} expected, {} observed".format(TestSum.dtGologOptimal,result))
        
    #     #print(params)


    # @classmethod
    # def tearDownClass(cls):
    #     print('DT-Golog - calculated policy reward: {}'.format(cls.dtGologOptimal))
    #     print('DT-Golog - simulated policy reward.: {}'.format(cls.simOptimal))
    #     print('Random policy reward...............: {}'.format(cls.simRandom))
    #     print('Learned policy reward..............: {}'.format(cls.learningOptimal))
    #     print('--> Learning Parameters: \n {}'.format(cls.learningParams))


    

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