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
agentAction(keepOn_run3).
agentAction(keepOff_run3).

/*======================================================

Definitions of Complex Control Actions/Procedures (AND/OR Decomp.) */


proc(control_run3, keepOn_run3 # keepOff_run3).


/*======================================================*/

/* Stochastic actions, two for each task/agent action; suffix S = succeeds, F = fails */
nondetActions(keepOn_run3,_,[keepOnS_run3, keepOnF_run3]).
nondetActions(keepOff_run3,_,[keepOffS_run3, keepOffF_run3]).



/*======================================================*/

/* Att. Formulae */
controlled_run3(S) :- 	keepOnS_fl_run3(S);keepOnF_fl_run3(S);
						keepOffS_fl_run3(S);keepOffF_fl_run3(S).
runDone_run3(S) :- controlled_run3(S).

/*======================================================*/

/* Using predicate  prob(Outcome,Probability,Situation)
   we specify numerical values of probabilities for each outcome 
*/


%prob(keepOnS_run3,0.95,_).
%prob(keepOnF_run3,0.05,_).
%prob(keepOffS_run3,0.95,_).
%prob(keepOffF_run3,0.05,_).

prob(keepOnS_run3,P,_) :- prob(keepOnS,P).
prob(keepOnF_run3,P,_) :- prob(keepOnF,P).
prob(keepOffS_run3,P,_):- prob(keepOffS,P).
prob(keepOffF_run3,P,_):- prob(keepOffF,P).


/* We formulate precondition axioms using the predicate poss(Outcome, Situation).
The right-hand side of precondition axioms provides conditions under which Outcome 
is possible in Situation. These are given for stochastic actions only.
*/


% The other supplier has not been chosen
poss(keepOn_run3, S):- 	\+ controlled_run3(S),controlled_run2(S).
poss(keepOff_run3, S):- \+ controlled_run3(S),controlled_run2(S).
			
% Same as above but for stochastic fluents
poss(keepOnS_run3,S) :- poss(keepOn_run3,S).
poss(keepOnF_run3,S) :- poss(keepOn_run3,S).

poss(keepOffS_run3,S) :- poss(keepOff_run3,S).
poss(keepOffF_run3,S) :- poss(keepOff_run3,S).


/*======================================================*/


keepOnS_fl_run3(do(A,S)) :- keepOnS_fl_run3(S); A=keepOnS_run3.
keepOnF_fl_run3(do(A,S)) :- keepOnF_fl_run3(S); A=keepOnF_run3.

keepOffS_fl_run3(do(A,S)) :- keepOffS_fl_run3(S); A=keepOffS_run3.
keepOffF_fl_run3(do(A,S)) :- keepOffF_fl_run3(S); A=keepOffF_run3.


/*  The predicate  senseCondition(Outcome,Psi) describes what logical
    formula Psi (in our case, fluent) should be evaluated to determine 
    Outcome uniquely
*/

senseCondition(keepOnS_run3,keepOnS_fl_run3).
senseCondition(keepOnF_run3,keepOnF_fl_run3).

senseCondition(keepOffS_run3,keepOffS_fl_run3).
senseCondition(keepOffF_run3,keepOffF_fl_run3).

/*======================================================*/

/* Syntactic Suger: Restore suppressed situation arguments. */

restoreSitArg(keepOnS_fl_run3,S,keepOnS_fl_run3(S)).
restoreSitArg(keepOnF_fl_run3,S,keepOnF_fl_run3(S)).

restoreSitArg(keepOffS_fl_run3,S,keepOffS_fl_run3(S)).
restoreSitArg(keepOffF_fl_run3,S,keepOffF_fl_run3(S)).
