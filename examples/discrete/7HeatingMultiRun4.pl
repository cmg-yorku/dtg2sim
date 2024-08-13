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

%getRewardMode(instant).
%getRewardModeDTG(instant).
getObsType(discrete).
getNumRuns(4).
getInfeasiblePenalty(-100).



%
% TRANSCEDENTAL STATE
%

% Transcendental State Structure: fluents that retain their values across runs.
transStateStructure([roomTemp_Inst_fl(_), hvac_on_fl]).

%
% Initialize Transcendental Fluents
%

% The initial state to be re-asserted by the calling envioronment from one run 
% to the next
init([roomTemp_Inst_fl(25)]).
hvac_on_fl(s0) :- init(A),findVal(M,A,hvac_on_fl).

%
% Custom Constants
%
idealTemp(23).
maxTemp(45).
outsideTemp(5).
c1(0.05).
c2(0.1).
c3(0.06).
c4(-1).




%
% LISTS: Agent Actions, Stochastic Actions, Fluents
%
agentActionList([keepOn,keepOff]). 
agentAction(keepOn). 
agentAction(keepOff).

stochasticActionList([keepOnS, keepOnF,keepOffS, keepOffF]).
nondetActions(keepOn,_,[keepOnS, keepOnF]).
nondetActions(keepOff,_,[keepOffS, keepOffF]).

fluentList([keepOnS_fl, keepOnF_fl, keepOffS_fl, keepOffF_fl]).


%
% PROCEDURES and ATTAINMENT FORMULAE`
%
proc(control, keepOn # keepOff).

/* Att. Formulae */
controlled(S) :- keepOnS_fl(S);keepOnF_fl(S);
						keepOffS_fl(S); keepOffF_fl(S).
goalAchieved(S) :- controlled(S).




%
% PROBABILITIES
%
prob(keepOnS,0.95,_).
prob(keepOnF,0.05,_).
prob(keepOffS,0.95,_).
prob(keepOffF,0.05,_).


%
% ACTION PRECONDITION AXIOMS
%

poss(keepOn, S):- \+ controlled(S).
poss(keepOff, S):- \+ controlled(S).
			
% Same as above but for stochastic fluents
poss(keepOnS,S) :- poss(keepOn,S).
poss(keepOnF,S) :- poss(keepOn,S).

poss(keepOffS,S) :- poss(keepOff,S).
poss(keepOffF,S) :- poss(keepOff,S).



%
% SUCCESSOR STATE AXIOMS
%


keepOnS_fl(do(A,S)) :- keepOnS_fl(S); A=keepOnS.
keepOnF_fl(do(A,S)) :- keepOnF_fl(S); A=keepOnF.

keepOffS_fl(do(A,S)) :- keepOffS_fl(S); A=keepOffS.
keepOffF_fl(do(A,S)) :- keepOffF_fl(S); A=keepOffF.

hvac_on_fl(do(A,S)) :- (hvac_on_fl(S),\+(A=keepOffS),!);A=keepOnS.


%
% SENSE CONDITIONS
%
senseCondition(keepOnS,keepOnS_fl_run1).
senseCondition(keepOnF,keepOnF_fl_run1).

senseCondition(keepOffS,keepOffS_fl).
senseCondition(keepOffF,keepOffF_fl).


%
% Argument Restoration - Helpers
%
restoreSitArg(keepOnS_fl,S,keepOnS_fl(S)).
restoreSitArg(keepOnF_fl,S,keepOnF_fl(S)).

restoreSitArg(keepOffS_fl,S,keepOffS_fl(S)).
restoreSitArg(keepOffF_fl,S,keepOffF_fl(S)).

restoreSitArg(hvac_on_fl,S,hvac_on_fl(S)).


%
%
% CONTINUOUS FLUENTS
%
%


%
% Continuous State Shape Information
%
%ccStateShapeInfo([[roomTemp_Inst_fl(_), 17, 29],[runningTime_Inst_fl(_),0,41]]).



%
%
% Fluents
%
%
runningTime_Inst_fl(0,s0). 
runningTime_Inst_fl(M,do(A,S)) :- 
                (((hvac_on_fl(S),\+(A=keepOffS),!);A=keepOnS),M is 10,!);
                (((\+ hvac_on_fl(S),\+(A=keepOnS),!);A=keepOffS), M is 0).
restoreSitArg(runningTime_Inst_fl(N),S,runningTime_Inst_fl(N,S)).


cost_Inst_fl(M,S) :-  runningTime_Inst_fl(N,S), c3(C3), M is N*C3.


roomTemp_Inst_fl(M,s0):- init(A),findVal(M,A,roomTemp_Inst_fl).
roomTemp_Inst_fl(M,do(A,S)) :- 
				% Affecting actions change fluent.				
				roomTemp_Inst_fl(Curr,S),
				maxTemp(MaxTemp),outsideTemp(OutsideTemp),c1(C1),c2(C2),
				(
				  (((hvac_on_fl(S),\+(A=keepOffS),!);A=keepOnS),M is Curr + C1*(MaxTemp - Curr),!);
				  (((\+ hvac_on_fl(S),\+(A=keepOnS),!);A=keepOffS),M is Curr - C2*(Curr-OutsideTemp))
				).
restoreSitArg(roomTemp_Inst_fl(N),S,roomTemp_Inst_fl(N,S)).


comfort_Inst_fl(M,S) :-  roomTemp_Inst_fl(T,S), c4(C4),
								idealTemp(IdealTemp),
								M is C4*abs(T - IdealTemp).


%
%
% REWARD
%
%
rewardInst(R,S) :- cost_Inst_fl(RCost,S),
                comfort_Inst_fl(RComf,S),
               R is 0.7*RComf + 0.3*RCost.



