:-style_check(-discontiguous).
:-style_check(-singleton).
:- multifile proc/2.
:- multifile agentAction/1.
:- multifile nondetActions/3.
:- multifile prob/3.
:- multifile prob/2.
:- multifile poss/2.
:- multifile senseCondition/2.
:- multifile restoreSitArg/3.

:- multifile initTemp/1.
:- multifile outsideTemp/1.
:- multifile maxTemp/1.
:- multifile idealTemp/1.
:- multifile c1/1.
:- multifile c2/1.
:- multifile c3/1.
:- multifile c4/1.

                   %  THE SIMPLE CONTRACTOR

/* :- discontiguous(whatever / 1). */
/* A = agent action, S = successful, F = failed, R = relation, AF = att. formula */
/*======================================================*/



/* Agent actions, one for each task */
agentAction(keepOn_run2).
agentAction(keepOff_run2).

/*======================================================

Definitions of Complex Control Actions/Procedures (AND/OR Decomp.) */


proc(control_run2, keepOn_run2 # keepOff_run2).


/*======================================================*/

/* Stochastic actions, two for each task/agent action; suffix S = succeeds, F = fails */
nondetActions(keepOn_run2,_,[keepOnS_run2, keepOnF_run2]).
nondetActions(keepOff_run2,_,[keepOffS_run2, keepOffF_run2]).



/*======================================================*/

/* Att. Formulae */
controlled_run2(S) :- 	keepOnS_fl_run2(S);keepOnF_fl_run2(S);
						keepOffS_fl_run2(S);keepOffF_fl_run2(S).
runDone_run2(S) :- controlled_run2(S).

/*======================================================*/

/* Using predicate  prob(Outcome,Probability,Situation)
   we specify numerical values of probabilities for each outcome 
*/


%prob(keepOnS_run2,0.95,_).
%prob(keepOnF_run2,0.05,_).
%prob(keepOffS_run2,0.95,_).
%prob(keepOffF_run2,0.05,_).


prob(keepOnS_run2,P,_) :- prob(keepOnS,P).
prob(keepOnF_run2,P,_) :- prob(keepOnF,P).
prob(keepOffS_run2,P,_):- prob(keepOffS,P).
prob(keepOffF_run2,P,_):- prob(keepOffF,P).



/* We formulate precondition axioms using the predicate poss(Outcome, Situation).
The right-hand side of precondition axioms provides conditions under which Outcome 
is possible in Situation. These are given for stochastic actions only.
*/


% The other supplier has not been chosen
poss(keepOn_run2, S):- 	\+ controlled_run2(S),controlled_run1(S).
poss(keepOff_run2, S):- \+ controlled_run2(S),controlled_run1(S).
			
% Same as above but for stochastic fluents
poss(keepOnS_run2,S) :- poss(keepOn_run2,S).
poss(keepOnF_run2,S) :- poss(keepOn_run2,S).

poss(keepOffS_run2,S) :- poss(keepOff_run2,S).
poss(keepOffF_run2,S) :- poss(keepOff_run2,S).


/*======================================================*/


keepOnS_fl_run2(do(A,S)) :- keepOnS_fl_run2(S); A=keepOnS_run2.
keepOnF_fl_run2(do(A,S)) :- keepOnF_fl_run2(S); A=keepOnF_run2.

keepOffS_fl_run2(do(A,S)) :- keepOffS_fl_run2(S); A=keepOffS_run2.
keepOffF_fl_run2(do(A,S)) :- keepOffF_fl_run2(S); A=keepOffF_run2.


/*  The predicate  senseCondition(Outcome,Psi) describes what logical
    formula Psi (in our case, fluent) should be evaluated to determine 
    Outcome uniquely
*/

senseCondition(keepOnS_run2,keepOnS_fl_run2).
senseCondition(keepOnF_run2,keepOnF_fl_run2).

senseCondition(keepOffS_run2,keepOffS_fl_run2).
senseCondition(keepOffF_run2,keepOffF_fl_run2).

/*======================================================*/

/* Syntactic Suger: Restore suppressed situation arguments. */

restoreSitArg(keepOnS_fl_run2,S,keepOnS_fl_run2(S)).
restoreSitArg(keepOnF_fl_run2,S,keepOnF_fl_run2(S)).

restoreSitArg(keepOffS_fl_run2,S,keepOffS_fl_run2(S)).
restoreSitArg(keepOffF_fl_run2,S,keepOffF_fl_run2(S)).
