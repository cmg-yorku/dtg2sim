# -*- coding: utf-8 -*-
"""
Created on Wed Sep 14 13:16:39 2022

@author: Anonymous User
"""

from stable_baselines3 import A2C
from stable_baselines3 import DQN
from stable_baselines3 import PPO
from stable_baselines3.common.monitor import Monitor
from src.GMEnv import GMEnv

import sys
import time

class dtg2sim():
    def __init__(self,environment=None,spec=None):
      if environment is None:
        if spec is None:
          raise ValueError("Either 'environment' or 'domainFile' must be provided.")
        else: #
          self.env = GMEnv(spec)
      else:
        self.env = environment
      self.score = 0
      self.debug = False

    def setDebug(self,status):
        self.debug = status

    def setEnv(self,environment):
        self.env = environment
        
    def reset(self):
        self.env.reset()
        self.score = 0

    def close(self):
        self.env.closeQE()

    def performAction(self,action,choice = -1):
        n_state, reward, done, info = self.env.step(action, choice)
        self.score += reward
        
        if self.debug:
            print(' ')
            print('New Action Attempt:')
            print('--> Action: {}'.format(action))
            print('--> St. Action: {}'.format(info["stAction"]))
            print('--> State: {}'.format(n_state))
            print('--> Reward: {}'.format(reward))
            print('--> Cum. Reward: {}'.format(self.score))
        
        return(n_state,reward,self.score,done,info)

        
    def simulate(self,episodes = 1_000,policy = [],debug = False, forgivePenalty = True):
        """
        Performs a fixed number of simulation runs.

        Parameters:
            episodes (int): The number of episodes to simulate (deafult is 1,000).
            policy (list of integers): A list of integers representing the actions to be 
                performed in sequence. Action indexes [0,...,n-1] are mapped to the list
                appearing as an argment in agentActionList() of the PL file. E.g., assuming:
                
                    agentActionList([orderFromSupplierA,orderFromSupplierB]).
                 
                orderFromSupplierA maps to 0
                orderFromSupplierB maps to 1
                 
                For single-run episodes policy [0] is the policy to always perform orderFromSupplierA.
                For four-run episodes policy [0,1,0,1] describes alternating decisions.
            
                If policy is empty (default) random choice of actions is performed.

            debug (boolean): True for debug information (default is False)
            
            forgivePenalty (boolean): If true (default), exclude user-defined negative penalty from 
                average reward calculation.
            
        Returns:
            float: The average reward.
        """
        totalScore = 0
        if policy:  
            print("Starting simulations on extraneously defined policy:")
        else:
            print("Starting random simulations:")
        for episode in range(1,episodes + 1):
            sys.stdout.write("\r\t%d%%" % ((episode/episodes)*100))
            sys.stdout.flush()
            self.env.reset()
            done = False
            self.score = 0
            pol = policy.copy()
            
            while not done:
                self.env.render()
                if not policy: # Policy is empty
                    # Executing Random Policy
                    action = self.env.action_space.sample()
                elif pol: 
                    # Executing given policy
                    action = pol.pop(0)
                else: 
                    print("     Error: Failed to end deterministic policy")
                    print("     Requested policy: {}".format(policy))
                    print("     Actions left in policy: {}".format(pol))
                    print("     State: {}".format(n_state))
                    print("     Episode done: {}".format(done))

                n_state, reward, done, info = self.env.step(action)
                
                if (reward != self.env.getInfeasiblePenalty()) or (not forgivePenalty):
                    self.score = reward
                    
                if self.debug: 
                    print(' ')
                    print('New Action Attempt:')
                    print('--> Action: {}'.format(action))
                    print('--> St. Action: {}'.format(info))
                    print('--> State: {}'.format(n_state))
                    print('--> Reward: {}'.format(reward))
                    print('--> Is Done: {}'.format(done))
                    print('--> Cum. Reward: {}'.format(self.score))

                if (policy and (not pol) and (not done)):
                    print("**** Error: Failed to end deterministic policy ***")
            
            totalScore += self.score
            if debug:
                print('Episode:{} Score:{}'.format(episode,self.score))
        #print('Simulation: average reward: {}'.format(totalScore/episodes))
        print("")
        print("Simulations complete.")
        print("Average Reward: {}".format(totalScore/episodes))
        return(totalScore/episodes)


    def train(self, learn_iter = 1_000, test_iter = 1_000,logging = 1_000, algo = "A2C"):
        """
        Trains an RL agent against the specification

        Parameters:
            learn_iter (int) : The number of learing iterations (deafult is 1,000).
            test_iter (int)  : The number of testing iterations (deafult is 1,000).
            logging (int)    : How often to log (deafult is 1,000).
            algo (String)    : One of "A2C" (default), "PPO", "DQN".
            
        Returns:
            float: The average reward.
        """
        st = time.process_time()
        print("Attempting {} model construction.".format(algo))
        
        if (algo == "A2C"):
            self.envm = Monitor(self.env,info_keywords=("is_success",))
            self.model = A2C("MlpPolicy", self.envm, verbose=1)
            print("Model Constructed. Learning starts...")
            self.model.learn(total_timesteps=learn_iter,log_interval = logging)
        elif (algo == "DQN"):
            self.envm = Monitor(self.env,info_keywords=("is_success",))
            self.model = DQN("MlpPolicy", self.envm, verbose=1)
            print("Model Constructed. Learning starts...")
            self.model.learn(total_timesteps=learn_iter,log_interval = logging)
        elif (algo == "PPO"):
            self.envm = Monitor(self.env,info_keywords=("is_success",))
            self.model = PPO("MlpPolicy", self.envm, verbose=1)
            print("Model Constructed. Learning starts...")
            self.model.learn(total_timesteps=learn_iter,log_interval = logging)
        else:
            print("Uknown learning algorithm")
            return
        self.vec_env = self.model.get_env()
        obs = self.vec_env.reset()
        params = self.model.get_parameters().get("policy.optimizer").get("param_groups")
        
        totalReward = 0
        totalRewardwPenalty = 0
        totalIter = test_iter
        
        # Time
        et = time.process_time()
        res = et - st
        print('Learning Complete. Leargning CPU Execution time:', res, 'seconds')
        print("Starting testing..")
        for i in range(totalIter):
            sys.stdout.write("\r\t%d%%" % ((i/totalIter)*100))
            sys.stdout.flush()
            obs = self.vec_env.reset()
            episodeDone = False
            episodeReward = 0 
            while (not(episodeDone)):
                action, _state = self.model.predict(obs, deterministic=True)
                obs, reward, done, info = self.vec_env.step(action)
                episodeReward = reward[0]
                episodeDone = done[0]
                
            totalRewardwPenalty  = totalRewardwPenalty + episodeReward
            if (info[0]['is_success']):
              totalReward  = totalReward + episodeReward
            
            # print("Episode {} Reward: {}".format(i,episodeReward))
        print("")
        print("Learning simulations complete.")
        result = totalReward/totalIter
        resultwp = totalRewardwPenalty/totalIter
        
        print("Average Reward: {}".format(result))
        print("Average Reward (including penalties): {}".format(resultwp))
        return result, params
      
    def getModel(self):
        return(self.model)
 
    def getEnvironment(self):
        return(self.vec_env)
