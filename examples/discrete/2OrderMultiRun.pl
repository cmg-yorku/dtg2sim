:-consult("../../scripts/QE/DT-Golog-Iface.pl").
:-style_check(-discontiguous).
:-style_check(-singleton).
:- multifile getRewardMode/1.
:- multifile getRewardModeDTG/1.
:- multifile penalizeDeadlock/1.
:- multifile deadlockPenalty/1.
:- multifile getInfeasiblePenalty/1.
:-dynamic(init/1).

%
% OPTIONS 
%

% Override default options
%getRewardMode(cummulative).
%getRewardModeDTG(cummulative).
getObsType(discrete).
getNumRuns(2).



%
% TRANSCEDENTAL STATE
%

% Transcendental State Structure: fluents that retain their values across runs.
% transStateStructure([roomTemp_Inst_fl(_), hvac_on_fl]).

%
% Initialize Transcendental Fluents
%

% The initial state to be re-asserted by the calling envioronment from one run 
% to the next
init([]).


%
% LISTS: Agent Actions, Stochastic Actions, Fluents
%

agentActionList([orderSup1A,orderSup2A]).
agentAction(orderSup1A).
agentAction(orderSup2A).

stochasticActionList([orderSup1InTimeST,orderSup1LateST,orderSup1NeverST,
					  orderSup2InTimeST,orderSup2LateST,orderSup2NeverST]).

nondetActions(orderSup1A,_,[orderSup1InTimeST,orderSup1LateST,orderSup1NeverST]).
nondetActions(orderSup2A,_,[orderSup2InTimeST,orderSup2LateST,orderSup2NeverST]).

fluentList([orderSup1InTime_fl,orderSup1Late_fl,orderSup1Never_fl,
		orderSup2InTime_fl,orderSup2Late_fl,orderSup2Never_fl]).



%
% PROCEDURES and ATTAINMENT FORMULAE
%
%Run with: bp(orderMaterial,10,Pol,Util,Prob,out).
proc(orderMaterial, orderSup1A # orderSup2A).

materialOrdered(S) :- orderSup1InTime_fl(S);orderSup1Late_fl(S);
						orderSup2InTime_fl(S);orderSup2Late_fl(S).
materialOrderedAtt(S) :- orderSup1InTime_fl(S);orderSup1Late_fl(S);orderSup1Never_fl(S);
						orderSup2InTime_fl(S);orderSup2Late_fl(S);orderSup2Never_fl(S).

goalAchieved(S) :- materialOrdered(S).




%
% PROBABILITIES
%
prob(orderSup1InTimeST,0.75,_). prob(orderSup1LateST,0.2,_). prob(orderSup1NeverST,0.05,_).
prob(orderSup2InTimeST,0.5,_). prob(orderSup2LateST,0.35,_). prob(orderSup2NeverST,0.15,_).


%
% ACTION PRECONDITION AXIOMS
%
poss(orderSup1A,S) :- 	\+ (orderSup2InTime_fl(S);
							orderSup2Late_fl(S);
							orderSup2Never_fl(S)),
						\+ (orderSup1InTime_fl(S);
							orderSup1Late_fl(S);
							orderSup1Never_fl(S)).
poss(orderSup2A,S) :- 	\+ (orderSup1InTime_fl(S);
							orderSup1Late_fl(S);
							orderSup1Never_fl(S)),
						\+ (orderSup2InTime_fl(S);
							orderSup2Late_fl(S);
							orderSup2Never_fl(S)).
							
poss(orderSup1InTimeST,S) :- poss(orderSup1A,S).
poss(orderSup1LateST,S) :- poss(orderSup1A,S).
poss(orderSup1NeverST,S) :- poss(orderSup1A,S).

poss(orderSup2InTimeST,S) :- poss(orderSup2A,S).
poss(orderSup2LateST,S) :- poss(orderSup2A,S).
poss(orderSup2NeverST,S) :- poss(orderSup2A,S).


%
% SUCCESSOR STATE AXIOMS
%
orderSup1InTime_fl(do(A,S)) :-orderSup1InTime_fl(S); A=orderSup1InTimeST.
orderSup2InTime_fl(do(A,S)) :-orderSup2InTime_fl(S); A=orderSup2InTimeST.

orderSup1Late_fl(do(A,S)) :-orderSup1Late_fl(S); A=orderSup1LateST.
orderSup2Late_fl(do(A,S)) :-orderSup2Late_fl(S); A=orderSup2LateST.

orderSup1Never_fl(do(A,S)) :-orderSup1Never_fl(S); A=orderSup1NeverST.
orderSup2Never_fl(do(A,S)) :-orderSup2Never_fl(S); A=orderSup2NeverST.


%
% SENSE CONDITIONS
%
senseCondition(orderSup1InTimeST,orderSup1InTime_fl).
senseCondition(orderSup2InTimeST,orderSup2InTime_fl).

senseCondition(orderSup1LateST,orderSup1Late_fl).
senseCondition(orderSup2LateST,orderSup2Late_fl).

senseCondition(orderSup1NeverST,orderSup1Never_fl).
senseCondition(orderSup2NeverST,orderSup2Never_fl).


%
% Argument Restoration - Helpers
%
restoreSitArg(orderSup1InTime_fl,S,orderSup1InTime_fl(S)).
restoreSitArg(orderSup2InTime_fl,S,orderSup2InTime_fl(S)).

restoreSitArg(orderSup1Late_fl,S,orderSup1Late_fl(S)).
restoreSitArg(orderSup2Late_fl,S,orderSup2Late_fl(S)).

restoreSitArg(orderSup1Never_fl,S,orderSup1Never_fl(S)).
restoreSitArg(orderSup2Never_fl,S,orderSup2Never_fl(S)).




/*====================*****==================================*/

/* Rewards & Softgoals */

rewardCost_Inst(0,s0).
rewardCost_Inst(R,do(A,S)) :- 
		(A \= orderSup1InTimeST,A \= orderSup1LateST,A \= orderSup1NeverST, 
		 A \= orderSup2InTimeST, A \= orderSup2LateST,A \= orderSup2NeverST, 
		 R is 0);
		(A = orderSup1InTimeST, R = 0.5);
		(A = orderSup2InTimeST, R = 1.0);
		(A = orderSup1LateST, R = 0.5);
		(A = orderSup2LateST, R = 1.0);
		(A = orderSup1NeverST, R = 0.5);
		(A = orderSup2NeverST, R = 1.0).

rewardCost(R,S) :- 
	(orderSup1InTime_fl(S),R is 0.5);
	(orderSup1Late_fl(S),R is 0.5);
	(orderSup1Never_fl(S),R is 0.5);
	(orderSup2InTime_fl(S),R is 1.0);
	(orderSup2Late_fl(S),R is 1.0);
	(orderSup2Never_fl(S),R is 1.0);
    R is 0.


rewardCustSat_Inst(0,s0).
rewardCustSat_Inst(R,do(A,S)) :- 
		(A \= orderSup1InTimeST,A \= orderSup1LateST,A \= orderSup1NeverST, 
 		 A \= orderSup2InTimeST, A \= orderSup2LateST,A \= orderSup2NeverST, 
		 R is 0);
		(A = orderSup1InTimeST, R = 1.0);
		(A = orderSup2InTimeST, R = 1.0);
		(A = orderSup1LateST, R = 0.7);
		(A = orderSup2LateST, R = 0.7);
		(A = orderSup1NeverST, R = 0.0);
		(A = orderSup2NeverST, R = 0.0).

rewardCustSat(R,S) :- 	
	(orderSup1InTime_fl(S),R is 1.0);
	(orderSup1Late_fl(S),R is 0.7);
	(orderSup1Never_fl(S),R is 0.0);
	(orderSup2InTime_fl(S),R is 1.0);
	(orderSup2Late_fl(S),R is 0.7);
	(orderSup2Never_fl(S),R is 0.0);
    R is 0.




%
% Instant reward after the action
%
rewardInst(R,S) :- rewardCost_Inst(RCost,S),
               rewardCustSat_Inst(RVal,S),
               R is 0.7*RCost + 0.3*RVal. 


%
% Cummulative Reward (normally not used)
%
rewardCum(R,S) :- rewardCost(RCost,S),
               rewardCustSat(RVal,S),
               R is 0.7*RCost + 0.3*RVal. 





