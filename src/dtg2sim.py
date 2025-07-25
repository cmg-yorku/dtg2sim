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

      self.vec_env = None
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
        n_state, reward, done,_ , info = self.env.step(action, choice)
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

        
    def simulate(self, episodes = 1_000, policy=None, debug = False, forgivePenalty = True):
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
        if policy is None:
            policy = []
        totalScore = 0
        if policy:  
            print("Starting simulations on extraneously defined policy:")
        else:
            print("Starting random simulations:")

        skippedEpisodes = 0
        totalScoreWPenalties = 0
        for episode in range(1,episodes + 1):
            sys.stdout.write("\r\t%d%%" % ((episode/episodes)*100))
            sys.stdout.flush()
            self.env.reset()
            done = False
            self.score = 0
            scoreWPenalties = 0
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

                n_state, reward, done, _ , info = self.env.step(action)

                scoreWPenalties += reward

                if (reward != self.env.getInfeasiblePenalty()) or (not forgivePenalty):
                    self.score += reward
                else:
                    skippedEpisodes += 1
                    self.score = 0

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
                if done and self.debug:
                    print(f"**** Episode {info['eH']} done with reward: {reward} and final state {n_state}")

            totalScore += self.score
            totalScoreWPenalties += scoreWPenalties
            if debug:
                print('Episode:{} Score:{}'.format(episode,self.score))
        #print('Simulation: average reward: {}'.format(totalScore/episodes))
        print("")
        print("Simulations complete.")
        print("Average Reward                       : {}".format(totalScore/(episodes - skippedEpisodes)))
        print("Average Reward (including penalties) : {}".format(totalScoreWPenalties/episodes))
        print("Skipped deadlock episodes            : {}".format(skippedEpisodes))
        return totalScore/(episodes - skippedEpisodes)

    def train(self, learn_iter = 1_000, logging = 1_000, algo = "A2C"):
        """
        Trains an RL agent against the specification

        Parameters:
            learn_iter (int) : The number of learning iterations (default is 1,000).
            logging (int)    : How often to log (default is 1,000).
            algo (str)    : One of "A2C" (default), "PPO", "DQN".
            
        Returns:
            float: The average reward.
        """
        st = time.process_time()
        print("Attempting {} model construction.".format(algo))
        
        if algo == "A2C":
            self.envm = Monitor(self.env,info_keywords=("is_success",))
            self.model = A2C("MlpPolicy", self.envm, verbose=1)
            print("Model Constructed. Learning starts...")
            self.model.learn(total_timesteps=learn_iter,log_interval = logging)
        elif algo == "DQN":
            self.envm = Monitor(self.env,info_keywords=("is_success",))
            self.model = DQN("MlpPolicy", self.envm, verbose=1)
            print("Model Constructed. Learning starts...")
            self.model.learn(total_timesteps=learn_iter,log_interval = logging)
        elif algo == "PPO":
            self.envm = Monitor(self.env,info_keywords=("is_success",))
            self.model = PPO("MlpPolicy", self.envm, verbose=1)
            print("Model Constructed. Learning starts...")
            self.model.learn(total_timesteps=learn_iter,log_interval = logging)
        else:
            print("Unknown learning algorithm")
            return -1
        self.vec_env = self.model.get_env()
        obs = self.vec_env.reset()
        params = self.model.get_parameters().get("policy.optimizer").get("param_groups")

        # Time
        et = time.process_time()
        res = et - st
        print('Learning Complete.')
        print('Learning CPU Execution time:', res, 'seconds')
        print('Use test() to test.')

        return params

    def test(self, test_iter = 1_000):
        """
        Tests trained an RL agent against the specification

        Parameters:
            test_iter (int) : The number of testing iterations (default is 1,000).
        Returns:
            float: The average reward.
        """
        totalReward = 0
        totalRewardwPenalty = 0
        skippedEpisodes = 0
        totalIter = test_iter

        if self.vec_env is None:
            print("No trained environment to test")
            return -1

        print("Testing starts...")
        for i in range(totalIter):
            sys.stdout.write("\r\t%d%%" % ((i/totalIter)*100))
            sys.stdout.flush()
            obs = self.vec_env.reset()
            episodeDone = False
            episodeReward = 0 
            while (not(episodeDone)):
                action, _state = self.model.predict(obs, deterministic=True)
                obs, reward, done, info = self.vec_env.step(action)
                episodeReward += reward[0]
                episodeDone = done[0]
                
            totalRewardwPenalty  = totalRewardwPenalty + episodeReward
            if info[0]['is_success']:
                totalReward  = totalReward + episodeReward
            else:
                skippedEpisodes += 1
            

        print("")
        print("Testing Complete.")
        if totalIter-skippedEpisodes != 0:
            result = totalReward/(totalIter-skippedEpisodes)
        else:
            result = 'N/A'
        resultwp = totalRewardwPenalty/totalIter
        
        print("Average Reward                       : {}".format(result))
        print("Average Reward (including penalties) : {}".format(resultwp))
        print("Skipped Episodes (deadlocks)         : {}".format(skippedEpisodes))
        return result
      
    def getModel(self):
        return(self.model)
 
    def getEnvironment(self):
        return(self.vec_env)
