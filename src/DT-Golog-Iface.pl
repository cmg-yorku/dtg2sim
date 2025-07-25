:-style_check(-discontiguous).
:-style_check(-singleton).
:-consult("DT-Golog.pl").
:-consult("DT-Golog-Ext.pl").
:- multifile getRewardMode/1.
:- multifile getRewardModeDTG/1.
:- multifile deadlockPenalty/1.
:- multifile getInfeasiblePenalty/1.
:- multifile val/2.


/*
Default (recommended) Configuration. 
*/
getRewardMode(instant).
getRewardModeDTG(episodic).
getInfeasiblePenalty(-100).
deadlockPenalty(0).



/* 
QMI   Call: possibleAt(self,t, eH)
**********************************

possibleAt(+SituationNum,+Action)
+SituationNum: a list of indexes of Stochastic Actions from the first to the last
+Action: an index to the action in question.
*/
possibleAt(SituationNum,ANum) :- 
						constructSituation(SituationNum,S),
						agentActionList(Pool),
						nth0(ANum,Pool,A),
						poss(A,S).



/*
QMI Call: getOutcomes(self,t,eH)
********************************

getActionOutcomes(+AgentActionNum,+SituationNum,-StochActionsListNum,-ProbList).
+AgentActionNum: the index of an agent action in the agent actions list
+SituationNum: a list of indexes of Stochastic Actions from the first to the last, representing the situation
-StochActionsListNum: a list of indexes of Stochastic Actions from the first to the last, representing the outcomes of AgentActionNum.
- Problist: a list of probabilities corresponding to the StochActionsListNum under situation SituationNum.
*/
getActionOutcomes(AgentActionNum, SituationNum, StochActionsListNum, ProbList):-
	agentActionList(AgentA),
	stochasticActionList(StochA),
	constructSituation(SituationNum,S),
	nth0(AgentActionNum,AgentA,AgentActionTerm),
	nondetActions(AgentActionTerm,S,StochActionsListTerm),
	getProbs(StochActionsListTerm,S,ProbList),
	fromItemsToIndex(StochActionsListTerm,StochA,StochActionsListNum).
	



/*
QMI Call: reward(self,eH)
*************************

getRewardRL(+SNum,-R).
+SNum: a list of indexes of stochastic actions, representing the current situation.
-R: a reward value for the current situation. 

NOTE 1: it returns the total accumulated reward for the run (not episode) given the initial state. 
It is the responsibility of GMEnv to return the differential reward (current reward - previous reward)
to the agent.
NOTE 2: if deadlockPenatly([penalty reward]) is defined, it is added every time a deadlock is reached
the penalty is going to be added to the reward. See DT-Golog-Ext.pl for the definition of deadlock.
*/
getRewardRL(SNum,R) :- constructSituation(SNum,S),deadlock(S), 
						current_predicate(deadlockPenalty/1),!,
						getRewardRL_(SNum,R1),
						deadlockPenalty(P),
						R is R1 + P.
getRewardRL(SNum,R) :- getRewardRL_(SNum,R).




/*
QMI Call: getState(self,eH) - Legacy / to be hidden/removed - (still used by Query Engine and here)

getState(+SNum,-Res)
From an indexed situation S returns a binary list marking the fluents that are true.
+SNum: a list of indexes of stochastic actions, representing the current situation.
-Res: a binary list representing the state of each of the fluents.
NOTE: Applies to discrete states only. Has been replaced by getAllState which supports
mixed continuous and discrete states.
*/
getState(SNum,Res) :- constructSituation(SNum,S),
					getStateG(S,Res).


/*
QMI Call: getCCState(self,eH) - Legacy / to be hidden/removed - (not used by Query Engine, used here as helper)

getCCState(+SNum,-Res) 
From an indexed situation S returns a list with the value of continuous fluents.
+SNum: a list of indexes of stochastic actions, representing the current situation.
-Res: a list representing the value of each of the continuous fluents.
*/
getCCState(SNum,Res) :- constructSituation(SNum,S),
						getStateShapeInfo(Fs,_,_),
						trueCCFluents(Fs,S,ResF),
						extractValues(ResF,Res).





/*
QMI Call: getAllState(self,eH,run)
**********************************

getAllState(+SNum,+RunNum,-Res)
From an indexed situation S and a current run number, returns a list with the value of all state features.
+SNum: a list of indexes of stochastic actions, representing the current situation.
+RunNum: the current run.
-Res: a list of the following form [[run] [discrete state] [continuous var 1] [continuous var 2] ...].
Where: [run] is the current run [0,runs-1]
       [discrete state] is the integer representing the configuration of truth values of discrete state features [0,2**preds-1]
	   [continuous var X] value of continuous variable (e.g., quality) 
	   Each of the three is optional and based on whether domain is multi-run and has discrete/continuous features.
	   (getCCState below will return empty if no continuous exist)
*/

% Case 1: Non-empty run and discrete exported state
getAllState(SNum,RunNum,Res) :-
			getNumRuns(Runs), Runs > 1, % Run is part of the state
			stateSize(D),D > 1, % Non empty discrete state
			getState(SNum,BinState),binary_list_to_int(BinState,IntState),
			getCCState(SNum,CState),
			append([IntState],CState,Res1),
			append([RunNum],Res1,Res).

% Case 2: Non-empty run, empty discrete exported state
getAllState(SNum,RunNum,Res) :-
			getNumRuns(Runs), Runs > 1, % Run is part of the state
			stateSize(1), % Empty discrete state
			getCCState(SNum,CState),
			append([RunNum],CState,Res).

% Case 3: Non-empty run, empty discrete exported state
getAllState(SNum,RunNum,Res) :-
			getNumRuns(1), % Not multi-run scenario
			stateSize(D),D > 1, % Non empty discrete state
			getState(SNum,BinState),binary_list_to_int(BinState,IntState),
			getCCState(SNum,CState),
			append([IntState],CState,Res).

% Case 4: Empty run and discrete exported state
getAllState(SNum,RunNum,Res) :-
			getNumRuns(1), % Not multi-run scenario
			stateSize(1), % Empty discrete state
			getCCState(SNum,Res),
			append([IntState],CState,Res).


/*
QMI Call: done(self,eH)
***********************

done(+SNum)
Decides if a situation signifies the end of an episode by virtue of there not being any action possible.
+SNum: a list of indexes of stochastic actions, representing the current situation.
*/
done(SNum) :- constructSituation(SNum,S),noActionPossible(S),!.


/*
QMI Call: runDone(self,eH)
**************************

runDone(+SNum)
Decides if a situation signifies the end of a run (can be due to goal achievement). 
It follows that done(X) ==> runDone(X)
+SNum: a list of indexes of stochastic actions, representing the current situation.
*/
runDone(SNum) :- constructSituation(SNum,S),(noActionPossible(S);goalAchieved(S)).


/*
QMI Call: achieved(self,eH)
***************************

achieved(+SNum)
Holds if in the situation SNum the root goal is satisfied.
It follows that achieved(X) ==> runDone(X)
+SNum: a list of indexes of stochastic actions, representing the current situation.
*/
achieved(SNum) :- constructSituation(SNum,S),goalAchieved(S).


/*
QMI Call: getDomainParams(self)
*******************************

actionSize(-L)
Returns the number of agent actions in the current domain. Used for array memory allocation.
-L: the number of agent actions in the current domain.
*/
actionSize(L) :- agentActionList(As), length(As,L).

/*
stateSizeBits(L)
Returns the number of distinct boolean state features of interest. Used for array memory allocation.
-S: the number of possible states.

stateSize(-S)
Returns the number of possible discrete states given the number of boolean state features of interest. Used for array memory allocation.
-S: the number of possible states.
*/
stateSizeBits(L) :- discreteExportedSet(Fs), length(Fs,L).
stateSize(S) :- stateSizeBits(L), S is 2**L.

/*
QMI Call: getDomainParams(self) - Other calls
*******************************

The following:
getNumRuns(-N)
getObsType(-O) - DEPRECATED, all obs types are now continuous

are defined within the domain file.

*/



/*
QMI Call: getStateShapeInfo(self)
*********************************

getCompleteShapeInfo(-Terms,-Mins,-Maxs)
Returns a list of identifiers (Terms) as well as the minimum (Mins) and maximum (Maxs) values allowed for these signatures.
-Terms: a list containing the identifiers ["(runs)", "(discreteSpace)", [quality name 1], [quality name 2],...]
-Mins: a list of minimum values for each identifier in the Terms list.
-Maxs: a list of maximum values for each identifier in the Terms list.
(runs) is between 1 and N st. getNumRuns(N)
(discreteSpace) is between 0 and D-1 where D st. stateSize(D)
[quality name X] is as hardcoded in continuousExportedSet/1
*/
getCompleteShapeInfo(Terms,Mins,Maxs):-
			getNumRuns(Runs),Runs > 1,
			stateSize(D),D > 1,
			DSize is D - 1,
			getStateShapeInfo(Terms2,Mins2,Maxs2),
			append(["(runs)", "(discreteSpace)"],Terms2,Terms),
			append([1,0],Mins2,Mins),
			append([Runs,DSize],Maxs2,Maxs).

getCompleteShapeInfo(Terms,Mins,Maxs):-
			getNumRuns(1),
			stateSize(D),D > 1,
			DSize is D - 1,
			getStateShapeInfo(Terms2,Mins2,Maxs2),
			append(["(discreteSpace)"],Terms2,Terms),
			append([0],Mins2,Mins),
			append([DSize],Maxs2,Maxs).

getCompleteShapeInfo(Terms,Mins,Maxs):-
			getNumRuns(Runs),Runs > 1,
			stateSize(1),
			getStateShapeInfo(Terms2,Mins2,Maxs2),
			append(["(runs)"],Terms2,Terms),
			append([1],Mins2,Mins),
			append([Runs],Maxs2,Maxs).

getCompleteShapeInfo(Terms,Mins,Maxs):-
			getNumRuns(1),
			stateSize(1),
			getStateShapeInfo(Terms,Mins,Maxs).

/* HELPER for getCompleteShapeInfo/3
getStateShapeInfo(Terms,Mins,Maxs)
Gets the continuous features min and max as declared in the domain file.
*/
getStateShapeInfo(Terms,Mins,Maxs) :- 
			continuousExportedSet(X),unwrapStateShapeInfo(X,Terms,Mins,Maxs).




/*
QMI Call: getTransState(self, eH)
*********************************

getTransState(+SNum,-Res)
Get the state of the trascendent fluents.
+SNum: a list of indexes of stochastic actions, representing the current situation.
-Res: a list of transcendent predicates unified with the values at the given situation.
*/
/* If inintial state has been ommited, just return an empty list */
getTransState(_,[]) :- \+ current_predicate(transStateStructure/1), 
                        \+ current_predicate(init/1),!.
/* Default (no trans-states defined) is to return the hardcoded initial state */
getTransState(_,Res) :- \+ current_predicate(transStateStructure/1), 
                            init(Res),!.
/* If both are defined work as follows */
getTransState(SNum,Res) :- current_predicate(transStateStructure/1), 
                        constructSituation(SNum,S),
						transStateStructure(Fs),
						trueCCFluents(Fs,S,Res),!.



/*
QMI Call: getInfeasibleActionPenalty(self)
******************************************

User defined option. See begining of file.
*/



/*
MISC for FUTURE USE
*/


getReadableState(SNum,Res):- discreteExportedSet(D),once(getState(SNum,S)),once(filter_by_mask(D,S,Res)).
getReadableHistory(SNum,Res):- stochasticActionList(L),items_at_indexes(L,SNum,Res).



