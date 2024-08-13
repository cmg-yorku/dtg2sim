:-consult("../../scripts/QE/DT-Golog.pl").
:-style_check(-discontiguous).
:-style_check(-singleton).
:- multifile proc/2.
:- multifile agentAction/1.
:- multifile nondetActions/3.
:- multifile prob/3.
:- multifile poss/2.
:- multifile senseCondition/2.
:- multifile getRewardMode/2.
:- multifile getRewardModeDTG/2.

                   %  THE SIMPLE CONTRACTOR

/* :- discontiguous(whatever / 1). */
/* A = agent action, S = successful, F = failed, R = relation, AF = att. formula */
/*======================================================*/

/* Action Lists */

realAgentActionList([orderSup1A,orderSup2A]).

numOfEpisodes(2).

agentActionList([orderSup1A_run1,
				orderSup2A_run1,
				orderSup1A_run2,
				orderSup2A_run2]).

stochasticActionList([orderSup1InTimeST_run1,orderSup1LateST_run1,orderSup1NeverST_run1,
					  orderSup2InTimeST_run1,orderSup2LateST_run1,orderSup2NeverST_run1,
					  orderSup1InTimeST_run2,orderSup1LateST_run2,orderSup1NeverST_run2,
					  orderSup2InTimeST_run2,orderSup2LateST_run2,orderSup2NeverST_run2]).

fluentList([orderSup1InTime_fl_run1,
		orderSup1Late_fl_run1,
		orderSup1Never_fl_run1,
		orderSup2InTime_fl_run1,
		orderSup2Late_fl_run1,
		orderSup2Never_fl_run1,
		orderSup1InTime_fl_run2,
		orderSup1Late_fl_run2,
		orderSup1Never_fl_run2,
		orderSup2InTime_fl_run2,
		orderSup2Late_fl_run2,
		orderSup2Never_fl_run2]).


/*======================================================

Definitions of Complex Control Actions/Procedures (AND/OR Decomp.) */

%Run with: bp(orderMaterial,10,Pol,Util,Prob,out).
proc(orderMaterial, orderMaterial_run1 :  orderMaterial_run2).


/*======================================================*/

/* Att. Formulae */
goalAchieved(S) :- runDone_run1(S),runDone_run2(S).

runCounter(C,S) :- runDone_run1(S),C = 1. 
runCounter(C,S) :- \+ runDone_run1(S),C = 0.



/*
reward(R,S) :- rewardCost(RCost,S),
               rewardCustSat(RVal,S),
               R is 0.7*RCost + 0.3*RVal. 
*/


reward(R,S) :- rewardInst(R,S).


%
% Instant reward after the action
%
rewardInst(R,S) :- rewardInst_run1(R1,S),rewardInst_run2(R2,S),
					R is R1 + R2. 


%
% Cummulative Reward for episode
%
rewardCum(R,S) :- rewardCum_run1(R1,S),rewardCum_run2(R2,S),
					R is R1 + R2.  



%
% Load Runs
%
:-consult("2OrderMultRun-Runs/2OrderMultiRun-Run1__.pl").
:-consult("2OrderMultRun-Runs/2OrderMultiRun-Run2__.pl").

