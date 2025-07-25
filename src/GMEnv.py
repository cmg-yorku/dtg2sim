# -*- coding: utf-8 -*-
"""
Created on Fri Jun  3 14:55:13 2022

@author: Anonymous
"""

from gymnasium import Env
from gymnasium.spaces import Discrete, Box
import numpy as np
from src.QueryEngine import QueryEngine

class GMEnv(Env):

    def __init__(self, qEng=None, file=None):

      if qEng is None:
        if file is None:
          raise ValueError("Either 'qEng' or 'file' must be provided.")
        else: #
          self.qmi  = QueryEngine(file)
      else:
        self.qmi = qEng

       
        # Consider the following goal model:
        # Root
        #   AND
        #       SubA
        #            OR
        #                TaskA1 (0) --(eff)--> TA0S (0), TA0F (1)
        #                TaskA2 (1) --(eff)--> TA1S (2), TA1F (3)
        #       SubB      
        #            OR
        #                TaskB1 (2) --(eff)--> TA2S (4), TA2F (5)
        #                TaskB2 (3) --(eff)--> TA2S (6), TA3F (7)
        # There are hence the following fluents:
        # TA0S_fl, TA0F_fl, TA1S_fl, TA1F_fl, TA1S_fl, ...
        # The numbering below corresponds to numbers in parentheses above.

        # tH: List of lists of agent actions that have been performed. Each list corresponds to a signle run.
        # e.g. [[]], [[1]], [[0,3],[]] or [[0,3],[1]], 
        self.tH = [[]]
                
        # eH: List of of lists of nature actions that have been performed
        # e.g. [[]], [[3]], [[3,7],[]],  [[3,7],[2]] (consistent with above)
        self.eH = [[]]

        # A binary list of lists in which every position represents a fluent.
        # It is 1 if the fluent is true, 0 otherwise
        # e.g., [0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0]], 
        # [0,0,0,1,0,0,0,0],[0,0,0,0,0,0,0,0]],
        # [[0,0,0,1,0,0,0,1],[0,0,0,0,0,0,0,0]]  
        # [[0,0,0,1,0,0,0,1],[0,0,1,0,0,0,0,0]] (consistent with above - 2 run problem)
        self.bitState = [[]] # To be revised/initialized below.
    
        # A float array with the values of all continuous variables
        self.ccState = [] # To be revised/initialized below.

        # A list of floats combining both discrete and continuous state
        self.allState = []

        # As above, to store the result of a step
        self.newState = None

        # The goal run currently in progress (from zero to runNo-1)
        self.run = 0

        # The accrued reward of the current episode
        self.reward = 0

        # Previous state of the accrued reward
        self.reward_pre = 0

        # The amount of penalty to apply if the agent tries an 
        # infeasible action.
        self.inFeasiblePenalty = self.qmi.getInfeasibleActionPenalty()

        # Should the episode be terminated when infeasible action is tried?
        self.episodeTerminationPolicyOn = True #NOT to be tweaked
        self.terminateEpisode = False #is the current episode to be terminated?

        # Keep the hard-coded trans (cross-run) state for resetting.
        self.initTransState = self.qmi.getTransState(self.eHString())

        # Set the default seed for np.
        self.defaultSeed = 123
        np.random.seed(self.defaultSeed)

        self.debug = False
        
        # Obtain domain parameters from Query Engine
        self.actionSize, self.stateSize, self.initBitState, self.obsType, self.runsNum = self.qmi.getDomainParams()
        self.bitState = self.initBitState.copy()

        #  A C T I O N    S P A C E 
        self.action_space = Discrete(self.actionSize)     
        
        # O B S E R V A T I O N     S P A C E
        if self.obsType == "continuous":
            shapeInfo = self.qmi.getStateShapeInfo()
            self.obsMins = shapeInfo['Min']
            self.obsMaxs = shapeInfo['Max']
            self.observation_space = Box(
                                    low = np.array(self.obsMins),
                                    high = np.array(self.obsMaxs)
                                    )
        else: # OBSOLETE
            self.observation_space = Discrete(self.stateSize)
            self.obsMins = -1
            self.obsMaxs = -1

    def reset(self, seed = None, options = None):
        # Reset the episode
        super().reset(seed = None)
        self.eH = [[]]
        self.tH = [[]]
        self.bitState = self.initBitState.copy()
        self.qmi.setTransState(self.initTransState)
        self.terminateEpisode = False
        self.run = 0
        self.reward = 0
        self.reward_pre = 0
        self.newState = self.qmi.getAllState(self.eHString(),self.run)

        return self.newState, None

    def possible(self,action):
        # If the episode is done, accept no more actions
        if self.done():
            return False
        # If the action has been attempted before, no.
        if action in self.tH[self.run]:
            return False
        else:
            # Check if the action is possible in this run
            return self.qmi.possibleAt(action, self.eHString())

    def step(self, action, choice = -1):
        """
        Attempts an action.

        Parameters:
            action (int): An integer representing the action to be 
                performed. Action indexes [0,...,n-1] are mapped to the list
                appearing as an argment in agentActionList() of the PL file. 
                E.g., assuming:
                
                    agentActionList([orderFromSupplierA,orderFromSupplierB]).
                 
                orderFromSupplierA maps to 0
                orderFromSupplierB maps to 1
                 
            choice (int): Enforce a specific stochastic action. Indexing is done 
                As above, but the indexed list is now the argument of
                stochasticActionList(). For example in:
            
                    stochasticActionList([deliveredInTimeA_Eff,neverDeliveredA_Eff,
                                  deliveredLateA_Eff,deliveredInTimeB_Eff,
                                  neverDeliveredB_Eff,deliveredLateB_Eff]).
                                  
                deliveredLateA_Eff is represented by 2
            
                If choice == -1 (default) step assumes a random choice of stochastic
                action based on the chosen agent action and the spec 
                (see nondetActions/3 and prob/3 predicates).

        Returns:
            newState (float): An integer representing the new state.
            reward (float): the reward collected
            done (boolean): whether the episode is done
            inf (dictionary): other state information.
        """
        stAction = -1

        if self.possible(action):
            # Get the outcomes and probabilities (stochastic actions) of the agent action
            #realAction = self.getCopy(action)
            possStochActions, probs = self.qmi.getOutcomes(action,self.eHString())
            
            # Pick one of the choices according to the probability
            if choice == -1:
                stAction = np.random.choice(possStochActions,1,p=probs)[0]
            else:
                stAction = choice
            # Append both the agent and the stochastic action to the list
            # of performed actions for the run
            self.tH[self.run].append(action)
            self.eH[self.run].append(stAction)

            # Calculate incremental reward
            self.reward = self.qmi.reward(self.eHString()) - self.reward_pre
            self.reward_pre = self.qmi.reward(self.eHString())

            self.bitState[self.run] = self.qmi.getState(self.eHString())
                
        else: # The action is not possible
            self.reward = self.inFeasiblePenalty
            if self.episodeTerminationPolicyOn:
                self.terminateEpisode = True

        self.newState = self.qmi.getAllState(self.eHString(),self.run)
        
        if self.runConcluded():
            if self.run <= self.runsNum - 1:
                self.advanceRun()    
            
        if self.obsType == "discrete": # OBSOLETE!
            self.newState = self.constructStateInt(self.bitState)

        inf = {"stAction": stAction,
               "bitState": self.bitState,
               "allState": self.newState,
               "tH": self.tH,
               "eH": self.eH,
               "Run": self.run,
               "Root achieved": self.achieved(),
               "Episode Done": self.done(),
               "Terminate Episode": self.terminateEpisode,
               "TransState": self.qmi.getTransState(self.eHString()),
               "is_success": (self.run == self.runsNum)
               }

        if self.debug:
            print(' ')
            print('New Action Attempt:')
            for key, value in inf.items():
                print(f"--> {key}: {value}")
        
        return self.newState, self.reward, self.done(), False, inf

    def done(self):
        assert(self.run <= self.runsNum)
        # DO NOT TOUCH BELOW
        return self.qmi.done(self.eHString()) or (self.run == self.runsNum) or self.terminateEpisode

    def runConcluded(self):
        return self.qmi.runDone(self.eHString())

    def achieved(self):
        return self.qmi.achieved(self.eHString())

    def render(self):
        # Visualization not implemented
        pass

    def advanceRun(self):
        # Grab trans values from the latest eH state and assert them to the new
        #print("Copying Transstate {}".format(self.qmi.getTransState(self.eHString())))
        self.qmi.setTransState(self.qmi.getTransState(self.eHString()))
        self.run = self.run + 1
        self.tH.append([])
        self.eH.append([])

    
    
    #
    # M I S C    H E L P E R S
    #

    def setSeed(self,newSeed):
        np.random.seed(newSeed)
        
    def bitToNum(self, l = None):
        # binary list to integer conversion
        if l is None:
            l = []
        result = 0
        for digits in l:
            result = (result << 1) | digits
        return result

    # Returns eH of the latest run in form of a string (for Prolog interfacing).
    def eHString(self):
        eHstr = [str(x) for x in self.eH[self.run]]
        eHstr = ",".join(eHstr)
        return eHstr

    def getCopy(self,action):
        realAction = action + self.run*self.actionSize
        return realAction
        
    def getInfeasiblePenalty(self):
        return self.qmi.getInfeasibleActionPenalty()

    def setImpossibleActionPenalty(self,penalty):
        self.inFeasiblePenalty = penalty

    def flatten(self,l):
        return [item for sublist in l for item in sublist]

    # Construct State Integer from bitState, run and stateSize
    def constructStateInt(self, bS):
        return self.bitToNum(self.flatten(bS))

    def closeQE(self):
        self.qmi.close()

    #
    #
    # D E B U G    R E L A T E D
    #
    def get_possible_actions(self):
        possible_actions = []
        for action in range(self.actionSize):
            if self.possible(action):
                possible_actions.append(action)
        return possible_actions


    def printDebug(self):
        print("**** ** tH: {}".format(self.tH))
        print("**** ** eH: {}".format(self.eH))
        print("**** ** Actual eH: {}".format(self.eHString()))
        print("**** ** Poss: {}".format(self.get_possible_actions()))
        print("**** ** Curr State: {}".format(self.newState))
        print("**** ** Bit State: {}".format(self.bitState))
        print('**** ** Run: {}'.format(self.run))
        print('**** ** Reward: {}'.format(self.reward))
        print('**** ** Run Concluded: {}'.format(self.runConcluded()))
        print('**** ** No Action Possible: {}'.format(self.done()))
        print('**** ** Root Achieved: {}'.format(self.achieved()))

    def setDebug(self,status):
        self.debug = status
