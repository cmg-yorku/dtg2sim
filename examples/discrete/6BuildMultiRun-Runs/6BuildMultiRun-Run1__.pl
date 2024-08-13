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
agentAction(orderSup1_run1).
agentAction(orderSup2_run1).
agentAction(assignSub1_run1).
agentAction(assignSub2_run1).

/*======================================================

Definitions of Complex Control Actions/Procedures (AND/OR Decomp.) */

proc(buildRoof_run1, orderMaterial_run1 : assignWork_run1).
proc(orderMaterial_run1, orderSup1_run1 # orderSup2_run1).
proc(assignWork_run1, assignSub1_run1 # assignSub2_run1).

/*======================================================*/

/* Stochastic actions, two for each task/agent action; suffix S = succeeds, F = fails */
nondetActions(orderSup1_run1,_,[orderSup1_InTime_run1, orderSup1_Late_run1, orderSup1_Never_run1]).
nondetActions(orderSup2_run1,_,[orderSup2_InTime_run1, orderSup2_Late_run1, orderSup2_Never_run1]).
nondetActions(assignSub1_run1,_,[assignSub1_Good_run1, assignSub1_Bad_run1]).
nondetActions(assignSub2_run1,_,[assignSub2_Good_run1, assignSub2_Bad_run1]).


/*======================================================*/

/* Att. Formulae */
materialOrdered_run1(S) :- orderSup1InTime_fl_run1(S);orderSup1Late_fl_run1(S);
						orderSup2InTime_fl_run1(S);orderSup2Late_fl_run1(S).
						
materialOrderedFailed_run1(S) :- orderSup1Never_fl_run1(S); orderSup2Never_fl_run1(S).

workAssigned_run1(S) :- assignSub1Good_fl_run1(S); assignSub1Bad_fl_run1(S);
						assignSub2Good_fl_run1(S); assignSub2Bad_fl_run1(S).
						
roofBuilt_run1(S) :- materialOrdered_run1(S),workAssigned_run1(S).

runDone_run1(S) :- roofBuilt_run1(S).

/*======================================================*/

/* Using predicate  prob(Outcome,Probability,Situation)
   we specify numerical values of probabilities for each outcome 
*/


prob(orderSup1_InTime_run1,0.75,_). 
prob(orderSup1_Late_run1,0.2,_).
prob(orderSup1_Never_run1,0.05,_).

prob(orderSup2_InTime_run1,0.5,_). 
prob(orderSup2_Late_run1,0.35,_). 
prob(orderSup2_Never_run1,0.15,_).

prob(assignSub1_Good_run1,0.7,_).
prob(assignSub1_Bad_run1,0.3,_).
prob(assignSub2_Good_run1,0.5,_).
prob(assignSub2_Bad_run1,0.5,_).


/* We formulate precondition axioms using the predicate poss(Outcome, Situation).
The right-hand side of precondition axioms provides conditions under which Outcome 
is possible in Situation. These are given for stochastic actions only.
*/


% The other supplier has not been chosen
poss(orderSup1_run1, S):- 	\+ (orderSup2InTime_fl_run1(S);
								orderSup2Late_fl_run1(S);
								orderSup2Never_fl_run1(S)),
							\+ (orderSup1InTime_fl_run1(S);
								orderSup1Late_fl_run1(S);
								orderSup1Never_fl_run1(S)).
								
poss(orderSup2_run1, S):- 	\+ (orderSup1InTime_fl_run1(S);
								orderSup1Late_fl_run1(S);
								orderSup1Never_fl_run1(S)),
								\+ (orderSup2InTime_fl_run1(S);
								orderSup2Late_fl_run1(S);
								orderSup2Never_fl_run1(S)).

% The material has been ordered and the other guy has not been chosen.
poss(assignSub1_run1,S) :- 	materialOrdered_run1(S),
							 \+(assignSub2Good_fl_run1(S);
								assignSub2Bad_fl_run1(S)),
							\+(assignSub1Good_fl_run1(S);
								assignSub1Bad_fl_run1(S)),
							% NPR - Cannot assign to subcontractor A if material is ordered from B.
							\+ (orderSup2InTime_fl_run1(S);
								orderSup2Late_fl_run1(S);
								orderSup2Never_fl_run1(S)).

% The material has been order and the other guy has not been chosen.
poss(assignSub2_run1,S) :- 	materialOrdered_run1(S),
							 \+(assignSub1Good_fl_run1(S);
								assignSub1Bad_fl_run1(S)),
							\+(assignSub2Good_fl_run1(S);
								assignSub2Bad_fl_run1(S)).

% Same as above but for stochastic fluents
poss(orderSup1_InTime_run1,S):- poss(orderSup1_run1,S).
poss(orderSup1_Late_run1,S):- 	poss(orderSup1_run1,S).
poss(orderSup1_Never_run1,S):- 	poss(orderSup1_run1,S).

poss(orderSup2_InTime_run1,S):- poss(orderSup2_run1,S).
poss(orderSup2_Late_run1,S) :- 	poss(orderSup2_run1,S).
poss(orderSup2_Never_run1,S) :- poss(orderSup2_run1,S).

poss(assignSub1_Good_run1,S) :- poss(assignSub1_run1,S).
poss(assignSub1_Bad_run1,S) :-  poss(assignSub1_run1,S).

poss(assignSub2_Good_run1,S) :- poss(assignSub2_run1,S).
poss(assignSub2_Bad_run1,S) :- 	poss(assignSub2_run1,S).

/*======================================================*/


orderSup1InTime_fl_run1(do(A,S)) :-orderSup1InTime_fl_run1(S); A=orderSup1_InTime_run1.
orderSup2InTime_fl_run1(do(A,S)) :-orderSup2InTime_fl_run1(S); A=orderSup2_InTime_run1.

orderSup1Late_fl_run1(do(A,S)) :-orderSup1Late_fl_run1(S); A=orderSup1_Late_run1.
orderSup2Late_fl_run1(do(A,S)) :-orderSup2Late_fl_run1(S); A=orderSup2_Late_run1.

orderSup1Never_fl_run1(do(A,S)) :-orderSup1Never_fl_run1(S); A=orderSup1_Never_run1.
orderSup2Never_fl_run1(do(A,S)) :-orderSup2Never_fl_run1(S); A=orderSup2_Never_run1.

assignSub1Good_fl_run1(do(A,S)) :- assignSub1Good_fl_run1(S); A=assignSub1_Good_run1.
assignSub1Bad_fl_run1(do(A,S)) :- assignSub1Bad_fl_run1(S); A=assignSub1_Bad_run1.

assignSub2Good_fl_run1(do(A,S)) :- assignSub2Good_fl_run1(S); A=assignSub2_Good_run1.
assignSub2Bad_fl_run1(do(A,S)) :- assignSub2Bad_fl_run1(S); A=assignSub2_Bad_run1.


reputation_fl_run1(0,s0).
reputation_fl_run1(0,[]).
reputation_fl_run1(M,do(A,S)) :- 
				% None of the affecting actions happen - fluent stays as is.
				(
				 A \= orderSup1_InTime_run1,A \= orderSup2_InTime_run1,
				 A \= orderSup1_Late_run1,A \= orderSup1_Never_run1, 
				 A \= orderSup2_Late_run1,A \= orderSup2_Never_run1, 
				 A \= assignSub1_Bad_run1,A \= assignSub2_Bad_run1, 
				 A \= assignSub1_Good_run1,A \= assignSub2_Good_run1, 
				reputation_fl_run1(M,S));
		
				% Affecting actions change fluent.				
				(A=orderSup1_InTime_run1, reputation_fl_run1(N,S), M is N + 1.0);
				(A=orderSup2_InTime_run1, reputation_fl_run1(N,S), M is N + 1.0);
				(A=orderSup1_Late_run1, reputation_fl_run1(N,S), M is N - 1.0);
				(A=orderSup1_Never_run1, reputation_fl_run1(N,S), M is N - 5.0);
				(A=orderSup2_Late_run1, reputation_fl_run1(N,S), M is N - 1.0);
				(A=orderSup2_Never_run1, reputation_fl_run1(N,S), M is N - 5.0);
				(A=assignSub1_Bad_run1, reputation_fl_run1(N,S), M is N - 1.0);
				(A=assignSub2_Bad_run1, reputation_fl_run1(N,S), M is N - 1.0);				
				(A=assignSub1_Good_run1, reputation_fl_run1(N,S), M is N + 1.0);
				(A=assignSub2_Good_run1, reputation_fl_run1(N,S), M is N + 1.0).


reputation_Inst_fl_run1(0,s0). 
reputation_Inst_fl_run1(M,do(A,S)) :- 
				% None of the affecting actions happen - fluent stays as is.
				(
				 A \= orderSup1_InTime_run1,A \= orderSup2_InTime_run1,
				 A \= orderSup1_Late_run1,A \= orderSup1_Never_run1, 
				 A \= orderSup2_Late_run1,A \= orderSup2_Never_run1, 
				 A \= assignSub1_Bad_run1,A \= assignSub2_Bad_run1, 
				 A \= assignSub1_Good_run1,A \= assignSub2_Good_run1, 
				M is 0);
		
				% Affecting actions change fluent.				
				(A=orderSup1_InTime_run1, reputation_fl_run1(N,S), M is 1.0);
				(A=orderSup2_InTime_run1, reputation_fl_run1(N,S), M is 1.0);
				(A=orderSup1_Late_run1, reputation_fl_run1(N,S), M is -1.0);
				(A=orderSup1_Never_run1, reputation_fl_run1(N,S), M is -5.0);
				(A=orderSup2_Late_run1, reputation_fl_run1(N,S), M is -1.0);
				(A=orderSup2_Never_run1, reputation_fl_run1(N,S), M is -5.0);
				(A=assignSub1_Bad_run1, reputation_fl_run1(N,S), M is -1.0);
				(A=assignSub2_Bad_run1, reputation_fl_run1(N,S), M is -1.0);				
				(A=assignSub1_Good_run1, reputation_fl_run1(N,S), M is 1.0);
				(A=assignSub2_Good_run1, reputation_fl_run1(N,S), M is 1.0).


gain_fl_run1(0,s0).
gain_fl_run1(0,[]).
gain_fl_run1(M,do(A,S)) :- 
				% None of the affecting actions happen - fluent stays as is.
				(A \= orderSup2_InTime_run1, A \= orderSup2_Late_run1,A \= orderSup2_Never_run1, 
				 A \= assignSub2_Good_run1,A \= assignSub2_Bad_run1, 
				gain_fl_run1(M,S));
				% Affecting actions change fluent.				
				(A=orderSup2_InTime_run1, gain_fl_run1(N,S), M is N + 1.0);
				(A=orderSup2_Late_run1, gain_fl_run1(N,S), M is N + 1.0);
				(A=orderSup2_Never_run1, gain_fl_run1(N,S), M is N + 1.0);
				(A=assignSub2_Good_run1, gain_fl_run1(N,S), M is N + 0.5);
				(A=assignSub2_Bad_run1, gain_fl_run1(N,S), M is N + 0.5).

gain_Inst_fl_run1(0,s0).
gain_Inst_fl_run1(M,do(A,S)) :- 
				% None of the affecting actions happen - fluent stays as is.
				(A \= orderSup2_InTime_run1, A \= orderSup2_Late_run1,A \= orderSup2_Never_run1, 
				 A \= assignSub2_Good_run1,A \= assignSub2_Bad_run1, 
				M is 0);
				% Affecting actions change fluent.				
				(A=orderSup2_InTime_run1, gain_fl_run1(N,S), M is 1.0);
				(A=orderSup2_Late_run1, gain_fl_run1(N,S), M is 1.0);
				(A=orderSup2_Never_run1, gain_fl_run1(N,S), M is 1.0);
				(A=assignSub2_Good_run1, gain_fl_run1(N,S), M is 0.5);
				(A=assignSub2_Bad_run1, gain_fl_run1(N,S), M is 0.5).



/*======================================================*/


/*====================*****==================================*/

/* Rewards & Softgoals */


%
% Instant reward after the action
%
rewardInst_run1(R,S) :- reputation_Inst_fl_run1(RRep,S),
                gain_Inst_fl_run1(RGain,S),
               R is 0.7*RRep + 0.3*RGain.


%
% Cummulative Reward for episode
%
rewardCum_run1(R,S) :- reputation_fl_run1(RRep,S),
                gain_fl_run1(RGain,S),
               R is 0.7*RRep + 0.3*RGain. 










/*  The predicate  senseCondition(Outcome,Psi) describes what logical
    formula Psi (in our case, fluent) should be evaluated to determine 
    Outcome uniquely
*/

senseCondition(orderSup1_InTime_run1,orderSup1InTime_fl_run1).
senseCondition(orderSup2_InTime_run1,orderSup2InTime_fl_run1).

senseCondition(orderSup1_Late_run1,orderSup1Late_fl_run1).
senseCondition(orderSup2_Late_run1,orderSup2Late_fl_run1).

senseCondition(orderSup1_Never_run1,orderSup1Never_fl_run1).
senseCondition(orderSup2_Never_run1,orderSup2Never_fl_run1).

senseCondition(assignSub1_Good_run1,assignSub1Good_fl_run1).
senseCondition(assignSub1_Bad_run1,assignSub1Bad_fl_run1).

senseCondition(assignSub2_Good_run1,assignSub2Good_fl_run1).
senseCondition(assignSub2_Bad_run1,assignSub2Bad_fl_run1).


/*======================================================*/

/* Syntactic Suger: Restore suppressed situation arguments. */

restoreSitArg(orderSup1InTime_fl_run1,S,orderSup1InTime_fl_run1(S)).
restoreSitArg(orderSup2InTime_fl_run1,S,orderSup2InTime_fl_run1(S)).

restoreSitArg(orderSup1Late_fl_run1,S,orderSup1Late_fl_run1(S)).
restoreSitArg(orderSup2Late_fl_run1,S,orderSup2Late_fl_run1(S)).

restoreSitArg(orderSup1Never_fl_run1,S,orderSup1Never_fl_run1(S)).
restoreSitArg(orderSup2Never_fl_run1,S,orderSup2Never_fl_run1(S)).

restoreSitArg(assignSub1Good_fl_run1,S,assignSub1Good_fl_run1(S)).
restoreSitArg(assignSub1Bad_fl_run1,S,assignSub1Bad_fl_run1(S)).

restoreSitArg(assignSub2Good_fl_run1,S,assignSub2Good_fl_run1(S)).
restoreSitArg(assignSub2Bad_fl_run1,S,assignSub2Bad_fl_run1(S)).

restoreSitArg(reputation_fl_run1(N),S,reputation_fl_run1(N,S)).
restoreSitArg(gain_fl_run1(N),S,gain_fl_run1(N,S)).
