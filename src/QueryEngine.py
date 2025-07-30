# -*- coding: utf-8 -*-
"""
Created on Tue Jan 31 11:39:07 2023

@author: Sotirios Liaskos
"""

from pyswip import Prolog
from src.QMI import QMI


class QueryEngine:
    
    
    def __init__(self,file=None):
      # super().__init__(file)
      self.prolog = Prolog()
      self.prolog.consult("src/DT-Golog-Iface.pl")
      if file is not None:
          self.prolog.consult(file)
        
    def setFile(self,file):
        """
        Sets the prolog file with the extended DT-Golog specification.

        Parameters
        ----------
        file : str
             The file Path.

        Returns
        -------
        None.
        """
        self.prolog.consult(file)
        
    def possibleAt(self,t, eH):
        """
        Checks is action t is possible after effect history eH

        Parameters
        ----------
        t : int
            An integer representing the agent (not nature) action in question AFTER correction for multiple runs.
        eH : str
            A comma-separated string of integers, where each integer represents an effect (i.e., a nature action) in the goal model.

        Returns
        -------
        result : bool
            True if task is possible false otherwise.
        """
        query = "possibleAt([" + eH + "], " + str(t) + ")."
        if list(self.prolog.query(query)):
            result = True
        else:
            result = False
        return result
        
    def getOutcomes(self,t,eH):
        """
        Returns the effects associated with task t in history eH

        Parameters
        ----------
        t : int
            The task in question
        eH : str
            A comma-separated string of integers, where each integer represents an effect (i.e., a nature action) in the goal model

        Returns
        -------
        list of int
            A list of integers indicating possible effects (nature actions) that can happen after attempt of t
        list of float
            A list of probabilities indicating the probability of each of the effects identified above.
        """
        query = "getActionOutcomes(" + str(t) + ",[" + eH + "],SActs,Probs)."
        possStochActions = list(self.prolog.query(query))[0]['SActs']
        probs = list(self.prolog.query(query))[0]['Probs']
        return possStochActions, probs
    
    def getProbs(self,t,eH):
        """
        Synonym for getOutcomes(self,t,eH).
        """
        # query = "getActionOutcomes(" + str(t) + ",[" + eH + "],SActs,Probs)."
        # possStochActions = list(self.prolog.query(query))[0]['SActs']
        # probs = list(self.prolog.query(query))[0]['Probs']
        return self.getOutcomes(t,eH)
        
    def reward(self,eH):
        """
        Returns the reward in situation indicated by history eH.
        NOTE: Reward is the total accumulated reward for the (partial) run (not episode) given the initial state.
        It is the responsibility of GMEnv to return the differential reward (current reward - previous reward)
        to the agent, as the reward obtain by the latest action alone.

        Parameters
        ----------
         eH : str
             A comma-separated string of integers, where each integer represents an effect (i.e., a nature action) in the goal model.

        Returns
        -------
        float
            The reward at eH
        """
        query = "getRewardRL([" + eH + "],R)."
        reward = list(self.prolog.query(query))[0]['R']
        return reward

    def getState(self,eH):
        """
        Returns the state (truth values of domain fluents) corresponding to a given execution history.

        Parameters
        ----------
        eH : str
            A comma-separated string of integers, e.g., "1, 2, 5", where each integer represents
            an effect (a nature action) in the goal model.

        Returns
        -------
        list of bool
            A list of boolean values representing the truth value of each domain fluent.
            The order is defined by the domain specification's `fluentList(...)`.
        """
        query = "getState([" + eH + "],State)."
        bitState = list(self.prolog.query(query))[0]['State']
        return bitState

    def getAllState(self,eH,run):
        """
         Returns the state (truth value of state features) corresponding to history eH.

         Parameters
         ----------
            eH : String
                A comma-separated string of integers, e.g., "1, 2, 5", where each integer represents
                an effect (a nature action) in the goal model.
            run : Integer
                Current run.
         Returns
         -------
         a list of the following form [[run] [discrete state] [continuous var 1] [continuous var 2] ...].
            Where:
                [run] is the current run [0,runs-1]
                [discrete state] is the integer representing the configuration of truth values of discrete state features [0,2**preds-1]
	            [continuous var X] value of continuous variable (e.g., quality)
	        Each of the three is optional and based on whether domain is multi-run and has discrete/continuous features.
	        (getCCState below will return empty if no continuous exist)
         """
        query = "getAllState([" + eH + "]," + str(run) + ",State)."
        allState = list(self.prolog.query(query))[0]['State']
        return allState

    def done(self,eH):
        """
        Checks whether the given execution history marks the end of an episode.

        Parameters
        ----------
        eH : str
            A comma-separated string of integers, e.g., "1, 2, 5", where each integer represents
            an effect (i.e., a nature action) in the goal model.

        Returns
        -------
        bool
            True if the episode is done (i.e., no further actions are possible), False otherwise.
        """
        s = "done([" + eH + "])."
        if (list(self.prolog.query(s))):
            result = True
        else:
            result = False
        return result            
    
    def runDone(self,eH):
        """
        Checks if eH marks the end a run

        Parameters
        ----------
        eH  : str
            A comma-separated string of integers, e.g., "1, 2, 5", where each integer represents
            an effect (i.e., a nature action) in the goal model.

        Returns
        -------
        bool
            True if the run is complete. May be due to no action possible OR goal achievement.
        """
        s = "runDone([" + eH + "])."
        if (list(self.prolog.query(s))):
            result = True
        else:
            result = False
        return result   

    def achieved(self,eH):
        """
        Returns if the goal is achieved after effect history eH.

        Parameters
        ----------
        eH  : str
            A comma-separated string of integers, e.g., "1, 2, 5", where each integer represents
            an effect (i.e., a nature action) in the goal model.
        Returns
        -------
        result : bool
            True if the root goal is achieved at eH, false otherwise.
        """
        s = "achieved([" + eH + "])."
        if list(self.prolog.query(s)):
            result = True
        else:
            result = False
        return result


    def getDomainParams(self):
        """
        Returns various size parameters of the domain.

        Returns
        -------
        actionSize : int
            The number of agent actions available.
        stateSize : int
            The size of discrete part of the state space. (queued for deprecation)
        bitState : list of int
            A list of as many items as the domain predicates. Initialized to zero. (queued for deprecation)
        obsType : {discrete, continuous}
            Returns a string on whether the state is to be discrete (a bit list) or continuous (a list of real values).
            (queued for deprecation - domains must always be "continuous")
        """
        
        actionSize = list(self.prolog.query("actionSize(L)."))[0]['L']
        stateSize = list(self.prolog.query("stateSizeBits(S)."))[0]['S']
        numRuns = list(self.prolog.query("getNumRuns(R)."))[0]['R']
        bitState = list(self.prolog.query("getState([],S)."))[0]['S']
        obsType = list(self.prolog.query("getObsType(X)."))[0]['X']
        actualStateSize = 2**(stateSize*numRuns)
        return actionSize,actualStateSize,[bitState]*numRuns,obsType,numRuns

    def getStateShapeInfo(self):
        """
        Returns the shape info of any kind of state shape.

        Returns
        -------
        shapeInfo : A list containing three lists.
            T: a list containing the identifiers ["(runs)", "(discreteSpace)", [quality name 1], [quality name 2],...]
            Min: a list of minimum values for each identifier in the T list.
            Max: a list of maximum values for each identifier in the T list.
            Where:
                (runs) is between 1 and N st. getNumRuns(N)
                (discreteSpace) is between 0 and D-1 where D st. stateSize(D)
                [quality name X] is as hardcoded in continuousExportedSet/1
        """
        shapeInfo = list(self.prolog.query("getCompleteShapeInfo(T,Min,Max)."))[0]
        return shapeInfo
 
    def getTransState(self, eH):
        """
        Retrieves the cross-run state at history the latest run of eH

        Parameters
        ----------
        eH  : str
            A comma-separated string of integers, e.g., "1, 2, 5", where each integer represents
            an effect (i.e., a nature action) in the goal model.

        Returns
        -------
        The cross-run state in the form of a list of predicates.

        """
        s = "getTransState([" + eH + "],X)."
        ts = str(list(self.prolog.query(s))[0]['X'])
        return ts.replace("'", "")

    def setTransState(self, tS):
        """
        Sets the cross-run state via asserting the initial state of the corresponding fluents.

        Parameters
        ----------
        tS : str
            The cross-run state exactly as retrieved from the getTransState.

        Returns
        -------
            None.
        """
        self.prolog.retractall("init(_)")
        s = "init(" + tS + ")"
        #print("Asserting: {}".format(s))
        self.prolog.assertz(s)
    
    def getInfeasibleActionPenalty (self):
        """
        Retrieves the reward penalty for invoking an infeasible action.

        Returns
        -------
        penalty : float
            The penalty incurred by the attempt of the infeasible action.

        """
        s = "getInfeasiblePenalty(P)."
        penalty = list(self.prolog.query(s))[0]['P']
        return penalty

    def getDTGCalc (self):
        """
        Retrieves the DT-golog.

        Returns
        -------
        penalty : float
            The penalty incurred by the attempt of the infeasible action.

        """
        s = "dtgRun(L,U,P)."
        policy = list(self.prolog.query(s))[0]['L']
        utility = list(self.prolog.query(s))[0]['U']
        probability = list(self.prolog.query(s))[0]['P']
        return utility,probability, policy


    def close(self):
        self.prolog.retractall("init(_)")
        del self.prolog
