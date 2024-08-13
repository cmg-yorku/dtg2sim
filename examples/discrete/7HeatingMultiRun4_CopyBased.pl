:-consult("../../scripts/QE/DT-Golog.pl").
:-style_check(-discontiguous).
:-style_check(-singleton).
:-dynamic initiTemp/1.
:- multifile proc/2.
:- multifile agentAction/1.
:- multifile nondetActions/3.
:- multifile prob/3.
:- multifile prob/2.
:- multifile poss/2.
:- multifile senseCondition/2.
:- multifile restoreSitArg/3.
:- multifile getRewardMode/2.
:- multifile getRewardModeDTG/2.

:- multifile initTemp/1.
:- multifile outsideTemp/1.
:- multifile maxTemp/1.
:- multifile idealTemp/1.
:- multifile c1/1.
:- multifile c2/1.
:- multifile c3/1.
:- multifile c4/1.


%bp(control,10,Pol,Util,Prob,out).

%
% OPTIONS 
%

getObsType(continuous).
%getObsType(discrete).
%getRewardMode(instant).
%getRewardModeDTG(instant).
getInfeasiblePenalty(-100).
penalizeDeadlock(0).
deadlockPenalty(0).

initTemp(25).
idealTemp(23).
maxTemp(45).
outsideTemp(5).
c1(0.05).
c2(0.1).
c3(0.06).
c4(-1).

/* :- discontiguous(whatever / 1). */
/* A = agent action, S = successful, F = failed, R = relation, AF = att. formula */
/*======================================================*/



/* Agent actions, one for each task */

realAgentActionList([keepOn,keepOff]).


agentActionList([	keepOn_run1,keepOff_run1,
					keepOn_run2,keepOff_run2,
					keepOn_run3,keepOff_run3,
					keepOn_run4,keepOff_run4
					]). 


stochasticActionList([	keepOnS_run1, keepOnF_run1,keepOffS_run1, keepOffF_run1,
						keepOnS_run2, keepOnF_run2,keepOffS_run2, keepOffF_run2,
						keepOnS_run3, keepOnF_run3,keepOffS_run3, keepOffF_run3,
						keepOnS_run4, keepOnF_run4,keepOffS_run4, keepOffF_run4
						]).

prob(keepOnS,0.95).
prob(keepOnF,0.05).
prob(keepOffS,0.95).
prob(keepOffF,0.05).



fluentList([			keepOnS_fl_run1, keepOnF_fl_run1, keepOffS_fl_run1, keepOffF_fl_run1,
						keepOnS_fl_run2, keepOnF_fl_run2, keepOffS_fl_run2, keepOffF_fl_run2,
						keepOnS_fl_run3, keepOnF_fl_run3, keepOffS_fl_run3, keepOffF_fl_run3,
						keepOnS_fl_run4, keepOnF_fl_run4, keepOffS_fl_run4, keepOffF_fl_run4
						]).


/*======================================================

Definitions of Complex Control Actions/Procedures (AND/OR Decomp.) */

proc(control, control_run1 : control_run2 : control_run3 : control_run4).

goalAchieved(S) :- runDone_run1(S),runDone_run2(S),runDone_run3(S),runDone_run4(S).


runCounter(C,S) :- \+ runDone_run1(S),C = 0.
runCounter(C,S) :- runDone_run1(S),\+ runDone_run2(S),C = 1. 
runCounter(C,S) :- runDone_run2(S),\+ runDone_run3(S),C = 2. 
runCounter(C,S) :- runDone_run3(S),\+ runDone_run4(S),C = 3. 

% State Information
%ccStateShapeInfo([[reputation_fl(_), -20, 10], [gain_fl(_), 0, 5]]).
ccStateShapeInfo([	[runCounter(_), 0, 4],
					[temp_fl(_), O, M]]):-outsideTemp(O),maxTemp(M).

%
% State fluents require their own successor state axiom as the span accross runs.
% In this case we simply add the cummulative reputation measures of the two runs
% However, in reality the axiom can be arbitrarily complex.
temp_fl(R,S) :- roomTemp_Inst_fl(R,S).

restoreSitArg(runCounter(N),S,runCounter(N,S)).
restoreSitArg(temp_fl(N),S,temp_fl(N,S)).


runningTime_Inst_fl(0,[]).
runningTime_Inst_fl(0,s0). 
runningTime_Inst_fl(M,do(A,S)) :- 
				% None of the affecting actions happen - time stays zero.
				(
                    ( /* cases in which it heats up */
                    ((hvac_on_fl(S),
    				  \+ (A = keepOffS_run1),
    				  \+ (A = keepOffS_run2),
    				  \+ (A = keepOffS_run3),
    				  \+ (A = keepOffS_run4)
    				 );
    				  (\+ hvac_on_fl(S),
      				   ((A = keepOnS_run1);
      				    (A = keepOnS_run2);
      				    (A = keepOnS_run3);
      				    (A = keepOnS_run4))
      				  )
    				), M is 10,!)
    				;
    				/* all other cases */
                    (M is 0)
                ).

cost_Inst_fl(M,S) :-  runningTime_Inst_fl(N,S), c3(C3), M is N*C3.


roomTemp_Inst_fl(M,[]):- initTemp(M).
roomTemp_Inst_fl(M,s0):- initTemp(M).
roomTemp_Inst_fl(M,do(A,S)) :- 
				% Affecting actions change fluent.				
				roomTemp_Inst_fl(Curr,S),
				maxTemp(MaxTemp),outsideTemp(OutsideTemp),c1(C1),c2(C2),
				(
                    ( /* cases in which it heats up */
                    ((hvac_on_fl(S),
    				  \+ (A = keepOffS_run1),
    				  \+ (A = keepOffS_run2),
    				  \+ (A = keepOffS_run3),
    				  \+ (A = keepOffS_run4)
    				 );
    				  (\+ hvac_on_fl(S),
      				   ((A = keepOnS_run1);
      				    (A = keepOnS_run2);
      				    (A = keepOnS_run3);
      				    (A = keepOnS_run4))
      				  )
    				), M is Curr + C1*(MaxTemp - Curr),!)
    				;
				/* all other cases */
                (M is Curr - C2*(Curr-OutsideTemp))).

                

comfort_Inst_fl(M,S) :-  roomTemp_Inst_fl(T,S), c4(C4),
								idealTemp(IdealTemp),
								M is C4*abs(T - IdealTemp).


hvac_on_fl(do(A,S)) :- 
                    (
                        hvac_on_fl(S),
                        \+(A=keepOffS_run1),
                        \+(A=keepOffS_run2),
                        \+(A=keepOffS_run3),
                        \+(A=keepOffS_run4),
                        !    
                    )
                    ;
                    (
                        A=keepOnS_run1;
                        A=keepOnS_run2;
                        A=keepOnS_run3;
                        A=keepOnS_run4
                    ).


restoreSitArg(hvac_on_fl,S,hvac_on_fl(S)).

%
% Instant reward after the action
%
rewardInst(0,s0).
rewardInst(R,S) :- cost_Inst_fl(RCost,S),
                comfort_Inst_fl(RComf,S),
               R is 0.7*RComf + 0.3*RCost.

reward(R,S) :- rewardInst(R,S).

%rewardInst(R,S) :- roomTemp_Inst_fl(R,S). 
%rewardInst(R,S) :- cost_Inst_fl(R,S). 

%
% Load Runs
%
:-consult("7Heating-Runs/7Heating-Run1__.pl").
:-consult("7Heating-Runs/7Heating-Run2__.pl").
:-consult("7Heating-Runs/7Heating-Run3__.pl").
:-consult("7Heating-Runs/7Heating-Run4__.pl").