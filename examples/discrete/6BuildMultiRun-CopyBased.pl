:-consult("../../scripts/QE/DT-Golog-Iface.pl").
:-style_check(-discontiguous).
:-style_check(-singleton).
:- multifile proc/2.
:- multifile agentAction/1.
:- multifile nondetActions/3.
:- multifile prob/3.
:- multifile poss/2.
:- multifile senseCondition/2.
:- multifile restoreSitArg/3.
:- multifile getRewardMode/2.
:- multifile getRewardModeDTG/2.



                   %  THE SIMPLE CONTRACTOR

/* :- discontiguous(whatever / 1). */
/* A = agent action, S = successful, F = failed, R = relation, AF = att. formula */
/*======================================================*/



/* Agent actions, one for each task */

realAgentActionList([	orderSup1,orderSup2,
						assignSub1,assignSub2]).


agentActionList([	orderSup1_run1,orderSup2_run1,assignSub1_run1,assignSub2_run1,
					orderSup1_run2,orderSup2_run2,assignSub1_run2,assignSub2_run2]).


stochasticActionList([	orderSup1_InTime_run1, orderSup1_Late_run1, orderSup1_Never_run1,
						orderSup2_InTime_run1, orderSup2_Late_run1, orderSup2_Never_run1,
						assignSub1_Good_run1, assignSub1_Bad_run1, 
						assignSub2_Good_run1, assignSub2_Bad_run1,
						
						orderSup1_InTime_run2, orderSup1_Late_run2, orderSup1_Never_run2,
						orderSup2_InTime_run2, orderSup2_Late_run2, orderSup2_Never_run2,
						assignSub1_Good_run2, assignSub1_Bad_run2, 
						assignSub2_Good_run2, assignSub2_Bad_run2
						]).

fluentList([orderSup1InTime_fl_run1, orderSup1Late_fl_run1, orderSup1Never_fl_run1,
			orderSup2InTime_fl_run1, orderSup2Late_fl_run1, orderSup2Never_fl_run1,
			assignSub1Good_fl_run1, assignSub1Bad_fl_run1, 
			assignSub2Good_fl_run1, assignSub2Bad_fl_run1,
						
			orderSup1InTime_fl_run2, orderSup1Late_fl_run2, orderSup1Never_fl_run2,
			orderSup2InTime_fl_run2, orderSup2Late_fl_run2, orderSup2Never_fl_run2,
			assignSub1Good_fl_run2, assignSub1Bad_fl_run2, 
			assignSub2Good_fl_run2, assignSub2Bad_fl_run2
			]).


/*======================================================

Definitions of Complex Control Actions/Procedures (AND/OR Decomp.) */

proc(buildRoof, buildRoof_run1 : buildRoof_run2).

goalAchieved(S) :- runDone_run1(S),runDone_run2(S).

runCounter(C,S) :- runDone_run1(S),C = 1. 
runCounter(C,S) :- \+ runDone_run1(S),C = 0.

% State Information
%ccStateShapeInfo([[reputation_fl(_), -20, 10], [gain_fl(_), 0, 5]]).
ccStateShapeInfo([[reputation_fl(_), -20, 10]]).

%
% State fluents require their own successor state axiom as the span accross runs.
% In this case we simply add the cummulative reputation measures of the two runs
% However, in reality the axiom can be arbitrarily complex.
reputation_fl(R,S) :- reputation_fl_run1(R1,S),reputation_fl_run2(R2,S), R is R1 + R2. 
restoreSitArg(reputation_fl(N),S,reputation_fl(N,S)).

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
:-consult("6BuildMultiRun-Runs/6BuildMultiRun-Run1__.pl").
:-consult("6BuildMultiRun-Runs/6BuildMultiRun-Run2__.pl").


