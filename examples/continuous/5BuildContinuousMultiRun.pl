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

getObsType(continuous).
getNumRuns(2).
getInfeasiblePenalty(-100).



%
% TRANSCEDENTAL STATE
%

% Transcendental State Structure: fluents that retain their values across runs.
transStateStructure([reputation_fl(_), gain_fl(_)]).

%
% Initialize Transcendental Fluents
%

% The initial state to be re-asserted by the calling envioronment from one run 
% to the next
init([reputation_fl(0), gain_fl(0)]).


%
% LISTS: Agent Actions, Stochastic Actions, Fluents
%
agentActionList([orderSup1,orderSup2,
				assignSub1,assignSub2]).

agentAction(orderSup1).
agentAction(orderSup2).
agentAction(assignSub1).
agentAction(assignSub2).

stochasticActionList([orderSup1_InTime, orderSup1_Late, orderSup1_Never,
					  orderSup2_InTime, orderSup2_Late, orderSup2_Never,
					  assignSub1_Good, assignSub1_Bad,
					  assignSub2_Good, assignSub2_Bad]).

nondetActions(orderSup1,_,[orderSup1_InTime, orderSup1_Late, orderSup1_Never]).
nondetActions(orderSup2,_,[orderSup2_InTime, orderSup2_Late, orderSup2_Never]).
nondetActions(assignSub1,_,[assignSub1_Good, assignSub1_Bad]).
nondetActions(assignSub2,_,[assignSub2_Good, assignSub2_Bad]).

fluentList([orderSup1InTime_fl,orderSup1Late_fl,orderSup1Never_fl,
		orderSup2InTime_fl,orderSup2Late_fl,orderSup2Never_fl,
		assignSub1Good_fl,assignSub1Bad_fl,
		assignSub2Good_fl,assignSub2Bad_fl]).

%
% PROCEDURES and ATTAINMENT FORMULAE
%
proc(buildRoof, orderMaterial : assignWork).
proc(orderMaterial, orderSup1 # orderSup2).
proc(assignWork, assignSub1 # assignSub2).

materialOrdered(S) :- orderSup1InTime_fl(S);orderSup1Late_fl(S);
						orderSup2InTime_fl(S);orderSup2Late_fl(S).
						
materialOrderedFailed(S) :- orderSup1Never_fl(S); orderSup2Never_fl(S).

workAssigned(S) :- assignSub1Good_fl(S); assignSub1Bad_fl(S);
						assignSub2Good_fl(S); assignSub2Bad_fl(S).
						
roofBuilt(S) :- materialOrdered(S),workAssigned(S).

goalAchieved(S) :- roofBuilt(S).


%
% PROBABILITIES
%
prob(orderSup1_InTime,0.75,_). prob(orderSup1_Late,0.2,_). prob(orderSup1_Never,0.05,_).
prob(orderSup2_InTime,0.5,_). prob(orderSup2_Late,0.35,_). prob(orderSup2_Never,0.15,_).

prob(assignSub1_Good,0.7,_). prob(assignSub1_Bad,0.3,_).
prob(assignSub2_Good,0.5,_). prob(assignSub2_Bad,0.5,_).


%
% ACTION PRECONDITION AXIOMS
%
poss(orderSup1, S):- \+ (orderSup2InTime_fl(S);orderSup2Late_fl(S);orderSup2Never_fl(S)),
						\+ (orderSup1InTime_fl(S);orderSup1Late_fl(S);orderSup1Never_fl(S)).
						
poss(orderSup2, S):- \+ (orderSup2InTime_fl(S);orderSup2Late_fl(S);orderSup2Never_fl(S)),
						\+ (orderSup1InTime_fl(S);orderSup1Late_fl(S);orderSup1Never_fl(S)).

poss(assignSub1,S):- materialOrdered(S),
						\+(assignSub2Good_fl(S);assignSub2Bad_fl(S)),
						\+(assignSub1Good_fl(S);assignSub1Bad_fl(S)),
						% NPR
						\+ (orderSup2InTime_fl(S);orderSup2Late_fl(S);orderSup2Never_fl(S)).
					
poss(assignSub2,S):- materialOrdered(S),
						\+(assignSub2Good_fl(S);assignSub2Bad_fl(S)),
						\+(assignSub1Good_fl(S);assignSub1Bad_fl(S)).


poss(orderSup1_InTime,S):- poss(orderSup1,S).
poss(orderSup1_Late,S)  :- poss(orderSup1,S).
poss(orderSup1_Never,S) :- poss(orderSup1,S).

poss(orderSup2_InTime,S):- poss(orderSup2,S).
poss(orderSup2_Late,S)  :- poss(orderSup2,S).
poss(orderSup2_Never,S) :- poss(orderSup2,S).

poss(assignSub1_Good,S):- poss(assignSub1,S).
poss(assignSub1_Bad,S) :- poss(assignSub1,S).
						
poss(assignSub2_Good,S):- poss(assignSub2,S).
poss(assignSub2_Bad,S) :- poss(assignSub2,S).


%
% SUCCESSOR STATE AXIOMS
%
orderSup1InTime_fl(do(A,S)) :-orderSup1InTime_fl(S); A=orderSup1_InTime.
orderSup2InTime_fl(do(A,S)) :-orderSup2InTime_fl(S); A=orderSup2_InTime.

orderSup1Late_fl(do(A,S)) :-orderSup1Late_fl(S); A=orderSup1_Late.
orderSup2Late_fl(do(A,S)) :-orderSup2Late_fl(S); A=orderSup2_Late.

orderSup1Never_fl(do(A,S)) :-orderSup1Never_fl(S); A=orderSup1_Never.
orderSup2Never_fl(do(A,S)) :-orderSup2Never_fl(S); A=orderSup2_Never.

assignSub1Good_fl(do(A,S)) :- assignSub1Good_fl(S); A=assignSub1_Good.
assignSub1Bad_fl(do(A,S)) :- assignSub1Bad_fl(S); A=assignSub1_Bad.

assignSub2Good_fl(do(A,S)) :- assignSub2Good_fl(S); A=assignSub2_Good.
assignSub2Bad_fl(do(A,S)) :- assignSub2Bad_fl(S); A=assignSub2_Bad.


%
% SENSE CONDITIONS
%
senseCondition(orderSup1_InTime,orderSup1InTime_fl).
senseCondition(orderSup2_InTime,orderSup2InTime_fl).

senseCondition(orderSup1_Late,orderSup1Late_fl).
senseCondition(orderSup2_Late,orderSup2Late_fl).

senseCondition(orderSup1_Never,orderSup1Never_fl).
senseCondition(orderSup2_Never,orderSup2Never_fl).

senseCondition(assignSub1_Good,assignSub1Good_fl).
senseCondition(assignSub1_Bad,assignSub1Bad_fl).

senseCondition(assignSub2_Good,assignSub2Good_fl).
senseCondition(assignSub2_Bad,assignSub2Bad_fl).

%
% Argument Restoration - Helpers
%
restoreSitArg(orderSup1InTime_fl,S,orderSup1InTime_fl(S)).
restoreSitArg(orderSup2InTime_fl,S,orderSup2InTime_fl(S)).

restoreSitArg(orderSup1Late_fl,S,orderSup1Late_fl(S)).
restoreSitArg(orderSup2Late_fl,S,orderSup2Late_fl(S)).

restoreSitArg(orderSup1Never_fl,S,orderSup1Never_fl(S)).
restoreSitArg(orderSup2Never_fl,S,orderSup2Never_fl(S)).

restoreSitArg(assignSub1Good_fl,S,assignSub1Good_fl(S)).
restoreSitArg(assignSub1Bad_fl,S,assignSub1Bad_fl(S)).

restoreSitArg(assignSub2Good_fl,S,assignSub2Good_fl(S)).
restoreSitArg(assignSub2Bad_fl,S,assignSub2Bad_fl(S)).

restoreSitArg(reputation_fl(N),S,reputation_fl(N,S)).
restoreSitArg(gain_fl(N),S,gain_fl(N,S)).



%
%
% Continuous Fluents
%
%

% The list of all continuous fluents
ccFluentList([reputation_fl(_),gain(_)]).
% ccStateFluentList([reputation_fl(_)]).


%
% Continuous State Information
%
ccStateShapeInfo([[reputation_fl(_), -7, 5], [gain_fl(_), 0, 5]]).
%ccStateShapeInfo([[reputation_fl(_), -20, 10]]).




%
%
% REWARD STRUCTURE
%
%

reputation_fl(M,s0):- init(A),findVal(M,A,reputation_fl).
reputation_fl(M,do(A,S)) :- 
				% None of the affecting actions happen - fluent stays as is.
				(
				 A \= orderSup1_InTime,A \= orderSup2_InTime,
				 A \= orderSup1_Late,A \= orderSup1_Never, 
				 A \= orderSup2_Late,A \= orderSup2_Never, 
				 A \= assignSub1_Bad,A \= assignSub2_Bad, 
				 A \= assignSub1_Good,A \= assignSub2_Good, 
				reputation_fl(M,S));
		
				% Affecting actions change fluent.				
				(A=orderSup1_InTime, reputation_fl(N,S), M is N + 1.0);
				(A=orderSup2_InTime, reputation_fl(N,S), M is N + 1.0);
				(A=orderSup1_Late, reputation_fl(N,S), M is N - 1.0);
				(A=orderSup1_Never, reputation_fl(N,S), M is N - 5.0);
				(A=orderSup2_Late, reputation_fl(N,S), M is N - 1.0);
				(A=orderSup2_Never, reputation_fl(N,S), M is N - 5.0);
				(A=assignSub1_Bad, reputation_fl(N,S), M is N - 1.0);
				(A=assignSub2_Bad, reputation_fl(N,S), M is N - 1.0);				
				(A=assignSub1_Good, reputation_fl(N,S), M is N + 1.0);
				(A=assignSub2_Good, reputation_fl(N,S), M is N + 1.0).


reputation_Inst_fl(0,s0). 
reputation_Inst_fl(M,do(A,S)) :- 
				% None of the affecting actions happen - fluent is zero.
				(
				 A \= orderSup1_InTime,A \= orderSup2_InTime,
				 A \= orderSup1_Late,A \= orderSup1_Never, 
				 A \= orderSup2_Late,A \= orderSup2_Never, 
				 A \= assignSub1_Bad,A \= assignSub2_Bad, 
				 A \= assignSub1_Good,A \= assignSub2_Good, 
				M is 0);
		
				% Affecting actions change fluent.				
				(A=orderSup1_InTime, reputation_fl(N,S), M is 1.0);
				(A=orderSup2_InTime, reputation_fl(N,S), M is 1.0);
				(A=orderSup1_Late, reputation_fl(N,S), M is -1.0);
				(A=orderSup1_Never, reputation_fl(N,S), M is -5.0);
				(A=orderSup2_Late, reputation_fl(N,S), M is -1.0);
				(A=orderSup2_Never, reputation_fl(N,S), M is -5.0);
				(A=assignSub1_Bad, reputation_fl(N,S), M is -1.0);
				(A=assignSub2_Bad, reputation_fl(N,S), M is -1.0);				
				(A=assignSub1_Good, reputation_fl(N,S), M is 1.0);
				(A=assignSub2_Good, reputation_fl(N,S), M is 1.0).


gain_fl(M,s0) :- init(A),findVal(M,A,gain_fl).
gain_fl(M,do(A,S)) :- 
				% None of the affecting actions happen - fluent stays as is.
				(A \= orderSup2_InTime, A \= orderSup2_Late,A \= orderSup2_Never, 
				 A \= assignSub2_Good,A \= assignSub2_Bad, 
				gain_fl(M,S));
				% Affecting actions change fluent.				
				(A=orderSup2_InTime, gain_fl(N,S), M is N + 1.0);
				(A=orderSup2_Late, gain_fl(N,S), M is N + 1.0);
				(A=orderSup2_Never, gain_fl(N,S), M is N + 1.0);
				(A=assignSub2_Good, gain_fl(N,S), M is N + 0.5);
				(A=assignSub2_Bad, gain_fl(N,S), M is N + 0.5).

gain_Inst_fl(0,s0).
gain_Inst_fl(M,do(A,S)) :- 
				% None of the affecting actions happen - fluent stays as is.
				(A \= orderSup2_InTime, A \= orderSup2_Late,A \= orderSup2_Never, 
				 A \= assignSub2_Good,A \= assignSub2_Bad, 
				M is 0);
				% Affecting actions change fluent.				
				(A=orderSup2_InTime, gain_fl(N,S), M is 1.0);
				(A=orderSup2_Late, gain_fl(N,S), M is 1.0);
				(A=orderSup2_Never, gain_fl(N,S), M is 1.0);
				(A=assignSub2_Good, gain_fl(N,S), M is 0.5);
				(A=assignSub2_Bad, gain_fl(N,S), M is 0.5).


%
% Instant reward after the action
%
rewardInst(R,S) :- reputation_Inst_fl(RRep,S),
                gain_Inst_fl(RGain,S),
               R is 0.7*RRep + 0.3*RGain.


%
% Cummulative Reward for episode (not used)
%
rewardCum(R,S) :- reputation_fl(RRep,S),
                gain_fl(RGain,S),
               R is 0.7*RRep + 0.3*RGain. 





