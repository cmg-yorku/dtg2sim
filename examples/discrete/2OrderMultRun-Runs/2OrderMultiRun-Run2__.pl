:-style_check(-discontiguous).
:-style_check(-singleton).

:- multifile proc/2.
:- multifile agentAction/1.
:- multifile nondetActions/3.
:- multifile prob/3.
:- multifile poss/2.
:- multifile senseCondition/2.
:- multifile restoreSitArg/3.

                   %  THE SIMPLE CONTRACTOR

/* :- discontiguous(whatever / 1). */
/* A = agent action, S = successful, F = failed, R = relation, AF = att. formula */
/*======================================================*/

/* Agent actions, one for each task */

agentAction(orderSup1A_run2).
agentAction(orderSup2A_run2).

/*======================================================

Definitions of Complex Control Actions/Procedures (AND/OR Decomp.) */

%Run with: bp(orderMaterial,10,Pol,Util,Prob,out).
proc(orderMaterial_run2, orderSup1A_run2 # orderSup2A_run2).


/*======================================================*/

/* Stochastic actions, two for each task/agent action; suffix S = succeeds, F = fails */


nondetActions(orderSup1A_run2,_,
		[orderSup1InTimeST_run2,orderSup1LateST_run2,orderSup1NeverST_run2]).
nondetActions(orderSup2A_run2,_,
		[orderSup2InTimeST_run2,orderSup2LateST_run2,orderSup2NeverST_run2]).


/*======================================================*/

/* Att. Formulae */
materialOrdered_run2(S) :- 	orderSup1InTime_fl_run2(S);
							orderSup1Late_fl_run2(S);
							orderSup2InTime_fl_run2(S);
							orderSup2Late_fl_run2(S).
							
materialOrderedAtt_run2(S) :- 	orderSup1InTime_fl_run2(S);
								orderSup1Late_fl_run2(S);
								orderSup1Never_fl_run2(S);
								orderSup2InTime_fl_run2(S);
								orderSup2Late_fl_run2(S);
								orderSup2Never_fl_run2(S).
						
runDone_run2(S):- materialOrdered_run2(S).

/*======================================================*/

/* Using predicate  prob(Outcome,Probability,Situation)
   we specify numerical values of probabilities for each outcome 
*/


prob(orderSup1InTimeST_run2,0.75,_). 
prob(orderSup1LateST_run2,0.2,_). 
prob(orderSup1NeverST_run2,0.05,_).
prob(orderSup2InTimeST_run2,0.5,_). 
prob(orderSup2LateST_run2,0.35,_). 
prob(orderSup2NeverST_run2,0.15,_).


/* We formulate precondition axioms using the predicate poss(Outcome, Situation).
The right-hand side of precondition axioms provides conditions under which Outcome 
is possible in Situation. These are given for stochastic actions only.
*/


poss(orderSup1A_run2, S) :- runDone_run1(S),\+ materialOrderedAtt_run2(S).
poss(orderSup2A_run2, S) :- runDone_run1(S),\+ materialOrderedAtt_run2(S).

poss(orderSup1InTimeST_run2,S) :- runDone_run1(S),\+ materialOrderedAtt_run2(S).
poss(orderSup1LateST_run2,S) :- runDone_run1(S),\+ materialOrderedAtt_run2(S).
poss(orderSup1NeverST_run2,S) :- runDone_run1(S),\+ materialOrderedAtt_run2(S).

poss(orderSup2InTimeST_run2,S) :- runDone_run1(S),\+ materialOrderedAtt_run2(S).
poss(orderSup2LateST_run2,S) :- runDone_run1(S),\+ materialOrderedAtt_run2(S).
poss(orderSup2NeverST_run2,S) :- runDone_run1(S),\+ materialOrderedAtt_run2(S).

/*======================================================*/

/* For each task, we have a fluent, uniquely identifying the actual outcome of the action. 
Here are the Successor-State axioms for these (again, these only involve stochastic actions).
*/

orderSup1InTime_fl_run2(do(A,S)) :-orderSup1InTime_fl_run2(S); A=orderSup1InTimeST_run2.
orderSup2InTime_fl_run2(do(A,S)) :-orderSup2InTime_fl_run2(S); A=orderSup2InTimeST_run2.

orderSup1Late_fl_run2(do(A,S)) :-orderSup1Late_fl_run2(S); A=orderSup1LateST_run2.
orderSup2Late_fl_run2(do(A,S)) :-orderSup2Late_fl_run2(S); A=orderSup2LateST_run2.

orderSup1Never_fl_run2(do(A,S)) :-orderSup1Never_fl_run2(S); A=orderSup1NeverST_run2.
orderSup2Never_fl_run2(do(A,S)) :-orderSup2Never_fl_run2(S); A=orderSup2NeverST_run2.




/*====================*****==================================*/

/* Rewards & Softgoals */
rewardCost_Inst_run2(0,s0).
rewardCost_Inst_run2(R,do(A,S)) :- 
		(A \= orderSup1InTimeST_run2,A \= orderSup1LateST_run2,A \= orderSup1NeverST_run2, 
		 A \= orderSup2InTimeST_run2, A \= orderSup2LateST_run2,A \= orderSup2NeverST_run2, 
		 R is 0);
		(A = orderSup1InTimeST_run2, R = 0.5);
		(A = orderSup2InTimeST_run2, R = 1.0);
		(A = orderSup1LateST_run2, R = 0.5);
		(A = orderSup2LateST_run2, R = 1.0);
		(A = orderSup1NeverST_run2, R = 0.5);
		(A = orderSup2NeverST_run2, R = 1.0).



rewardCost_run2(R,S) :- 
	(orderSup1InTime_fl_run2(S),R is 0.5);
	(orderSup1Late_fl_run2(S),R is 0.5);
	(orderSup1Never_fl_run2(S),R is 0.5);
	(orderSup2InTime_fl_run2(S),R is 1.0);
	(orderSup2Late_fl_run2(S),R is 1.0);
	(orderSup2Never_fl_run2(S),R is 1.0);
    R is 0.


rewardCustSat_Inst_run2(0,s0).
rewardCustSat_Inst_run2(R,do(A,S)) :- 
		(A \= orderSup1InTimeST_run2,A \= orderSup1LateST_run2,A \= orderSup1NeverST_run2, 
 		 A \= orderSup2InTimeST_run2, A \= orderSup2LateST_run2,A \= orderSup2NeverST_run2, 
		 R is 0);
		(A = orderSup1InTimeST_run2, R = 1.0);
		(A = orderSup2InTimeST_run2, R = 1.0);
		(A = orderSup1LateST_run2, R = 0.7);
		(A = orderSup2LateST_run2, R = 0.7);
		(A = orderSup1NeverST_run2, R = 0.0);
		(A = orderSup2NeverST_run2, R = 0.0).


rewardCustSat_run2(R,S) :- 	
	(orderSup1InTime_fl_run2(S),R is 1.0);
	(orderSup1Late_fl_run2(S),R is 0.7);
	(orderSup1Never_fl_run2(S),R is 0.0);
	(orderSup2InTime_fl_run2(S),R is 1.0);
	(orderSup2Late_fl_run2(S),R is 0.7);
	(orderSup2Never_fl_run2(S),R is 0.0);
    R is 0.



/*
reward(R,S) :- rewardCost(RCost,S),
               rewardCustSat(RVal,S),
               R is 0.7*RCost + 0.3*RVal. 
*/


%
% Instant reward after the action
%
rewardInst_run2(R,S) :- rewardCost_Inst_run2(RCost,S),
               rewardCustSat_Inst_run2(RVal,S),
               R is 0.7*RCost + 0.3*RVal. 


%
% Cummulative Reward for episode
%
rewardCum_run2(R,S) :- rewardCost_run2(RCost,S),
               rewardCustSat_run2(RVal,S),
               R is 0.7*RCost + 0.3*RVal. 


/*======================================================*/

/*  The predicate  senseCondition(Outcome,Psi) describes what logical
    formula Psi (in our case, fluent) should be evaluated to determine 
    Outcome uniquely
*/


senseCondition(orderSup1InTimeST_run2,orderSup1InTime_fl_run2).
senseCondition(orderSup2InTimeST_run2,orderSup2InTime_fl_run2).

senseCondition(orderSup1LateST_run2,orderSup1Late_fl_run2).
senseCondition(orderSup2LateST_run2,orderSup2Late_fl_run2).

senseCondition(orderSup1NeverST_run2,orderSup1Never_fl_run2).
senseCondition(orderSup2NeverST_run2,orderSup2Never_fl_run2).


/*======================================================*/

/* Syntactic Suger: Restore suppressed situation arguments. */

restoreSitArg(orderSup1InTime_fl_run2,S,orderSup1InTime_fl_run2(S)).
restoreSitArg(orderSup2InTime_fl_run2,S,orderSup2InTime_fl_run2(S)).

restoreSitArg(orderSup1Late_fl_run2,S,orderSup1Late_fl_run2(S)).
restoreSitArg(orderSup2Late_fl_run2,S,orderSup2Late_fl_run2(S)).

restoreSitArg(orderSup1Never_fl_run2,S,orderSup1Never_fl_run2(S)).
restoreSitArg(orderSup2Never_fl_run2,S,orderSup2Never_fl_run2(S)).

