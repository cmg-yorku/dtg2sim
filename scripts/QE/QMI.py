# -*- coding: utf-8 -*-
"""
Created on Tue Jan 31 11:44:06 2023

@author: Anonymous
"""

class QMI:
    def __init__(self,file):
        """ Something """
        pass
    
    def setFile(self,file):
        """
        Sets the prolog file with the extended DT-Golog specification.

        Parameters
        ----------
        file : String
             The file Path.

        Returns
        -------
        None.

        """
        pass
    
    def possibleAt(self,t,eH) -> bool: 
        """
        Checks is action t is possible after effect histrory eF

        Parameters
        ----------
        t : Integer
            An integer representing the agent (not nature) action in question AFTER correction for multiple runs.
        eH : String
             A string of the form "i_1, i_2, ...", each i being an integer representing an effect (nature action) in the goal model (after multi-run correction).

        Returns
        -------
        result : bool
            True is task is possible false otherwise.
        """
        pass
    def getOutcomes(self,t,eH) -> tuple[list[int],list[float]]:
        """
        Retruns the effects assicated with task t in history eH

        Parameters
        ----------
        t : integer
            The task in question 
        eH : String
             A string of the form "i_1, i_2, ...", each i being an integer representing an effect (nature action) in the goal model (after multi-run correction).

        Returns
        -------
        list of integers
            A list of integers indicating possible effects (nature actions) that can happen after attempt of t
        list of floats
            A list of probabilities indicating the probability of each of the effects identified above.
            
        """
        pass
    def getProbs(self,t,eH) -> tuple[list[int],list[float]]:
        """
        Synonym for getOutcomes(self,t,eH). 

         Parameters
         ----------
         t : integer
             The task in question 
         eH : String
             A string of the form "i_1, i_2, ...", each i being an integer representing an effect (nature action) in the goal model (after multi-run correction).

        Returns
        -------
        (tuple[list[int],list[float]])
         list of integers
             A list of integers indicating possible effects (nature actions) that can happen after attempt of t
         list of floats
             A list of probabilities indicating the probability of each of the effects identified above.
        """
        pass
    def reward(self,eH) -> float:
        """
        Returns the reward in situation indicated by history eH. The reward is instant (the reward obtained by the last action - the default) or cummulative, the reward accrued since the first action. To switch to cummulative (assuming you know what you are doing) uncomment predicate "getRewardMode(cummulative)." in the domain specification.

        Parameters
        ----------
         eH : String
             A string of the form "i_1, i_2, ...", each i being an integer representing an effect (nature action) in the goal model (after multi-run correction).

        Returns
        -------
        float
            The reward in eH

        """
        pass
    def getState(self,eH) -> list[bool]: 
        """
        Returns the state (truth value of domain predicates / fluents) corresponding to history eH.

        Parameters
        ----------
         eH : String
             A string of the form "i_1, i_2, ...", each i being an integer representing an effect (nature action) in the goal model (after multi-run correction).

        Returns
        -------
        list[bool]
            A list of boolean values "b_1, b_2, ...", each b_i representing the state of each of the domain predicates/fluents in the domain. Order defined in domain spec's "fluentList(...)."
        """
        pass
    def getConState(self,eH) -> list[float]:
        """
        Returns the continuous state (the value of qualities / continuous fluents) corresponding to history eH.
        

        Parameters
        ----------
         eH : String
             A string of the form "i_1, i_2, ...", each i being an integer representing an effect (nature action) in the goal model (after multi-run correction).

        Returns
        -------
        list[float]
            A list of floats "f_1, f_2, ...", each f_i representing the value of the corresponding quality. Order is defined in domain spec predicate "ccStateShapeInfo(...)."
        """
        pass
    def getRun(self,eH) -> int:
        """
        Returns the run representing the run history eH is on. Run turns to i+1 when the last action of eH triggers done of the sub-root goal associated with run i.

        Parameters
        ----------
         eH : String
             A string of the form "i_1, i_2, ...", each i being an integer representing an effect (nature action) in the goal model (after multi-run correction).

        Returns
        -------
        int
            An integer representing the run (starting from 0).
        """
        pass
    def done(self,eH) -> bool:
        """
        Checks if eH marks the end of an episode.        

        Parameters
        ----------
         eH : String
             A string of the form "i_1, i_2, ...", each i being an integer representing an effect (nature action) in the goal model (after multi-run correction).

        Returns
        -------
        bool
            True if the episode is done.
        """
        pass
