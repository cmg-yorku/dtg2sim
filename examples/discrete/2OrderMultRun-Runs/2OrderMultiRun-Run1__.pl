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

agentAction(orderSup1A_run1).
agentAction(orderSup2A_run1).

/*======================================================

Definitions of Complex Control Actions/Procedures (AND/OR Decomp.) */

%Run with: bp(orderMaterial,10,Pol,Util,Prob,out).
proc(orderMaterial_run1, orderSup1A_run1 # orderSup2A_run1).


/*======================================================*/

/* Stochastic actions, two for each task/agent action; suffix S = succeeds, F = fails */

nondetActions(orderSup1A_run1,_,
		[orderSup1InTimeST_run1,orderSup1LateST_run1,orderSup1NeverST_run1]).
nondetActions(orderSup2A_run1,_,
		[orderSup2InTimeST_run1,orderSup2LateST_run1,orderSup2NeverST_run1]).


/*======================================================*/

/* Att. Formulae */
materialOrdered_run1(S) :- 	orderSup1InTime_fl_run1(S);
							orderSup1Late_fl_run1(S);
							orderSup2InTime_fl_run1(S);
							orderSup2Late_fl_run1(S).
							
materialOrderedAtt_run1(S) :- 	orderSup1InTime_fl_run1(S);
								orderSup1Late_fl_run1(S);
								orderSup1Never_fl_run1(S);
								orderSup2InTime_fl_run1(S);
								orderSup2Late_fl_run1(S);
								orderSup2Never_fl_run1(S).
						
runDone_run1(S):- materialOrdered_run1(S).



/*======================================================*/

/* Using predicate  prob(Outcome,Probability,Situation)
   we specify numerical values of probabilities for each outcome 
*/


prob(orderSup1InTimeST_run1,0.75,_). 
prob(orderSup1LateST_run1,0.2,_). 
prob(orderSup1NeverST_run1,0.05,_).
prob(orderSup2InTimeST_run1,0.5,_). 
prob(orderSup2LateST_run1,0.35,_). 
prob(orderSup2NeverST_run1,0.15,_).


/* We formulate precondition axioms using the predicate poss(Outcome, Situation).
The right-hand side of precondition axioms provides conditions under which Outcome 
is possible in Situation. These are given for stochastic actions only.
*/


% Should not appear like that in automated
poss(orderSup1A_run1, S) :- \+ materialOrderedAtt_run1(S).
poss(orderSup2A_run1, S) :- \+ materialOrderedAtt_run1(S).

poss(orderSup1InTimeST_run1, S) :- \+ materialOrderedAtt_run1(S).
poss(orderSup1LateST_run1, S) :- \+ materialOrderedAtt_run1(S).
poss(orderSup1NeverST_run1, S) :- \+ materialOrderedAtt_run1(S).

poss(orderSup2InTimeST_run1, S) :- \+ materialOrderedAtt_run1(S).
poss(orderSup2LateST_run1, S) :- \+ materialOrderedAtt_run1(S).
poss(orderSup2NeverST_run1, S) :- \+ materialOrderedAtt_run1(S).

/*======================================================*/

/* For each task, we have a fluent, uniquely identifying the actual outcome of the action. 
Here are the Successor-State axioms for these (again, these only involve stochastic actions).
*/


orderSup1InTime_fl_run1(do(A,S)) :-orderSup1InTime_fl_run1(S); A=orderSup1InTimeST_run1.
orderSup2InTime_fl_run1(do(A,S)) :-orderSup2InTime_fl_run1(S); A=orderSup2InTimeST_run1.

orderSup1Late_fl_run1(do(A,S)) :-orderSup1Late_fl_run1(S); A=orderSup1LateST_run1.
orderSup2Late_fl_run1(do(A,S)) :-orderSup2Late_fl_run1(S); A=orderSup2LateST_run1.

orderSup1Never_fl_run1(do(A,S)) :-orderSup1Never_fl_run1(S); A=orderSup1NeverST_run1.
orderSup2Never_fl_run1(do(A,S)) :-orderSup2Never_fl_run1(S); A=orderSup2NeverST_run1.




/*====================*****==================================*/

/* Rewards & Softgoals */

rewardCost_Inst_run1(0,s0).
rewardCost_Inst_run1(R,do(A,S)) :- 
		(A \= orderSup1InTimeST_run1,A \= orderSup1LateST_run1,A \= orderSup1NeverST_run1, 
		 A \= orderSup2InTimeST_run1, A \= orderSup2LateST_run1,A \= orderSup2NeverST_run1, 
		 R is 0);
		(A = orderSup1InTimeST_run1, R = 0.5);
		(A = orderSup2InTimeST_run1, R = 1.0);
		(A = orderSup1LateST_run1, R = 0.5);
		(A = orderSup2LateST_run1, R = 1.0);
		(A = orderSup1NeverST_run1, R = 0.5);
		(A = orderSup2NeverST_run1, R = 1.0).



rewardCost_run1(R,S) :- 
	(orderSup1InTime_fl_run1(S),R is 0.5);
	(orderSup1Late_fl_run1(S),R is 0.5);
	(orderSup1Never_fl_run1(S),R is 0.5);
	(orderSup2InTime_fl_run1(S),R is 1.0);
	(orderSup2Late_fl_run1(S),R is 1.0);
	(orderSup2Never_fl_run1(S),R is 1.0);
    R is 0.


rewardCustSat_Inst_run1(0,s0).
rewardCustSat_Inst_run1(R,do(A,S)) :- 
		(A \= orderSup1InTimeST_run1,A \= orderSup1LateST_run1,A \= orderSup1NeverST_run1, 
 		 A \= orderSup2InTimeST_run1, A \= orderSup2LateST_run1,A \= orderSup2NeverST_run1, 
		 R is 0);
		(A = orderSup1InTimeST_run1, R = 1.0);
		(A = orderSup2InTimeST_run1, R = 1.0);
		(A = orderSup1LateST_run1, R = 0.7);
		(A = orderSup2LateST_run1, R = 0.7);
		(A = orderSup1NeverST_run1, R = 0.0);
		(A = orderSup2NeverST_run1, R = 0.0).


rewardCustSat_run1(R,S) :- 	
	(orderSup1InTime_fl_run1(S),R is 1.0);
	(orderSup1Late_fl_run1(S),R is 0.7);
	(orderSup1Never_fl_run1(S),R is 0.0);
	(orderSup2InTime_fl_run1(S),R is 1.0);
	(orderSup2Late_fl_run1(S),R is 0.7);
	(orderSup2Never_fl_run1(S),R is 0.0);
    R is 0.


%
% Instant reward after the action
%
rewardInst_run1(R,S) :- rewardCost_Inst_run1(RCost,S),
               rewardCustSat_Inst_run1(RVal,S),
               R is 0.7*RCost + 0.3*RVal. 


%
% Cummulative Reward for episode
%
rewardCum_run1(R,S) :- rewardCost_run1(RCost,S),
               rewardCustSat_run1(RVal,S),
               R is 0.7*RCost + 0.3*RVal. 



/*======================================================*/

/*  The predicate  senseCondition(Outcome,Psi) describes what logical
    formula Psi (in our case, fluent) should be evaluated to determine 
    Outcome uniquely
*/


senseCondition(orderSup1InTimeST_run1,orderSup1InTime_fl_run1).
senseCondition(orderSup2InTimeST_run1,orderSup2InTime_fl_run1).

senseCondition(orderSup1LateST_run1,orderSup1Late_fl_run1).
senseCondition(orderSup2LateST_run1,orderSup2Late_fl_run1).

senseCondition(orderSup1NeverST_run1,orderSup1Never_fl_run1).
senseCondition(orderSup2NeverST_run1,orderSup2Never_fl_run1).


/*======================================================*/

/* Syntactic Suger: Restore suppressed situation arguments. */

restoreSitArg(orderSup1InTime_fl_run1,S,orderSup1InTime_fl_run1(S)).
restoreSitArg(orderSup2InTime_fl_run1,S,orderSup2InTime_fl_run1(S)).

restoreSitArg(orderSup1Late_fl_run1,S,orderSup1Late_fl_run1(S)).
restoreSitArg(orderSup2Late_fl_run1,S,orderSup2Late_fl_run1(S)).

restoreSitArg(orderSup1Never_fl_run1,S,orderSup1Never_fl_run1(S)).
restoreSitArg(orderSup2Never_fl_run1,S,orderSup2Never_fl_run1(S)).

