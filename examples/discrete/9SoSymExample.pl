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

getObsType(discrete).
getNumRuns(1).


%
% TRANSCEDENTAL STATE
%

% Transcendental State Structure: fluents that retain their values across runs.
% transStateStructure([roomTemp_Inst_fl(_), hvac_on_fl]).

%
% Initialize Transcendental Fluents
%

% The initial state to be re-asserted by the calling envioronment from one run 
% to the next
init([]).

%
% DOMAIN 
%
agentActionList([refA,nonrefA,
                paperA,onlineA,
                cmtA,headA
					]).
agentAction(refA).
agentAction(nonrefA).
agentAction(paperA).  
agentAction(onlineA).
agentAction(cmtA).
agentAction(headA).

stochasticActionList([refS,refF,
						nonrefS,nonrefF,
						paperS,paperP,paperF,
						onlineS,onlineP,onlineF,
						cmtS,cmtF,
						headS,headF
						]).
nondetActions(refA,S,[refS,refF]).
nondetActions(nonrefA,S,[nonrefS,nonrefF]).
nondetActions(paperA,S,[paperS,paperP,paperF]).
nondetActions(onlineA,S,[onlineS,onlineP,onlineF]).
nondetActions(cmtA,S,[cmtS,cmtF]).
nondetActions(headA,S,[headS,headF]).

fluentList([refRS,refRF,
			nonrefRS,nonrefRF,
			paperRS,paperRP,paperRF,
			onlineRS,onlineRP,onlineRF,
			cmtRS,cmtRF,
			headRS,headRF
			]).


%
% PROCEDURES and ATTAINMENT FORMULAE
%
proc(tkBookedP, refA # nonrefA).
proc(authSignedP, cmtA # headA).
proc(appPreppedP, paperA # onlineA).
proc(authObtainedP, appPreppedP : authSignedP).

proc(travelOrganizedP, tkBookedP : authObtainedP).

paperAF(S) :- paperRS(S);paperRP(S).
paperAF4R(S) :- paperRS(S);paperRP(S);paperRF(S). /* For rewards */
onlineAF(S) :- onlineRS(S);onlineRP(S).
onlineAF4R(S) :- onlineRS(S);onlineRP(S);onlineRF(S). /* For rewards */

granted(S):-cmtRS(S);headRS(S).
granted_inst(do(A,S)) :- A=cmtS;A=headS.

denied(S):-cmtRF(S);headRF(S).
tixBooked(S):- refRS(S);nonrefRS(S).


tixBookingAtt(S) :- refRS(S);nonrefRS(S);refRF(S);nonrefRF(S).
appPreparedAtt(S) :- paperAF4R(S);onlineAF4R(S).
authSignedAtt(S) :- granted(S);denied(S).

goalAchieved(S) :- tixBooked(S),(paperAF(S);onlineAF(S)),granted(S).



%
% PROBABILITIES
%
prob(refS,0.95,S). prob(refF,0.05,S). 
prob(nonrefS,0.95,S). prob(nonrefF,0.05,S).

/* SA 1-3 */ 
prob(paperS,0.7,S). prob(paperP,0.2,S). prob(paperF,0.1,S).

/* SA 4-6 */ 
prob(onlineS,0.89,S). prob(onlineP,0.1,S). prob(onlineF,0.01,S).

/* SA 7 */ 
prob(cmtS, 0.9, S) :- paperAF(S),(not onlineAF(S)).
prob(cmtF, 0.1, S) :- paperAF(S),(not onlineAF(S)).

/* SA 8 */ 
prob(cmtS, 0.5, S) :- onlineAF(S),(not paperAF(S)).
prob(cmtF, 0.5, S) :- onlineAF(S),(not paperAF(S)).

/* SA 9 */
prob(headS,0.7,S). prob(headF,0.3,S).

%
% ACTION PRECONDITION AXIOMS
%
poss(refA,S):- \+ (refRS(S);refRF(S)), \+ (nonrefRS(S);nonrefRF(S)).

poss(nonrefA,S):- \+ (nonrefRS(S);nonrefRF(S)), \+ (refRS(S);refRF(S)).

poss(onlineA,S) :- tixBooked(S),\+ (paperRS(S);paperRP(S);paperRF(S)),
								\+ (onlineRS(S);onlineRP(S);onlineRF(S)).

poss(paperA,S) :- tixBooked(S),\+ (onlineRS(S);onlineRP(S);onlineRF(S)),
								\+ (paperRS(S);paperRP(S);paperRF(S)).

poss(cmtA,S):- (paperAF(S);onlineAF(S)),\+(headRS(S);headRF(S)),\+(cmtRS(S);cmtRF(S)).
poss(headA,S) :- (paperAF(S);onlineAF(S)),\+(cmtRS(S);cmtRF(S)),\+(headRS(S);headRF(S)).


poss(refS,S):- poss(refA,S).
poss(refF,S):- poss(refA,S).
poss(nonrefS,S):- poss(paperA,S).
poss(nonrefF,S):- poss(paperA,S).

poss(onlineS,S) :- poss(onlineA,S).
poss(onlineP,S) :- poss(onlineA,S).
poss(onlineF,S) :- poss(onlineA,S).

poss(paperS,S) :- poss(paperA,S).
poss(paperP,S) :- poss(paperA,S).
poss(paperF,S) :- poss(paperA,S).


poss(cmtS,S):- poss(cmtA,S).
poss(cmtF,S):- poss(cmtA,S).
poss(headS,S) :- poss(headA,S).
poss(headF,S) :- poss(headA,S).


%
% SUCCESSOR STATE AXIOMS
%
refRS(do(A,S)) :- refRS(S); A=refS.
refRF(do(A,S)) :- refRF(S); A=refF.

nonrefRS(do(A,S)) :- nonrefRS(S); A=nonrefS.
nonrefRF(do(A,S)) :- nonrefRF(S); A=nonrefF.

paperRS(do(A,S)) :- paperRS(S); A=paperS.
paperRP(do(A,S)) :- paperRP(S); A=paperP.
paperRF(do(A,S)) :- paperRF(S); A=paperF.

onlineRS(do(A,S)) :- onlineRS(S); A=onlineS.
onlineRP(do(A,S)) :- onlineRP(S); A=onlineP.
onlineRF(do(A,S)) :- onlineRF(S); A=onlineF.

cmtRS(do(A,S)) :- cmtRS(S); A=cmtS.
cmtRF(do(A,S)) :- cmtRF(S); A=cmtF.

headRS(do(A,S)) :- headRS(S); A=headS.
headRF(do(A,S)) :- headRF(S); A=headF.


%
% SENSE CONDITIONS
%
senseCondition(refS,refRS).        senseCondition(refF,(-refRS)).
senseCondition(nonrefS,nonrefRS).  senseCondition(nonrefF,(-nonrefRS)).
senseCondition(cmtS,cmtRS).        senseCondition(cmtF,(-cmtRS)).
senseCondition(headS,headRS).      senseCondition(headF,(-headRS)).

senseCondition(onlineS,onlineRS). senseCondition(onlineF,onlineRF).
senseCondition(onlineP,onlineRP). 
senseCondition(paperS,paperRS).   senseCondition(paperF,paperRF).
senseCondition(paperP,paperRP).

%
% Argument Restoration - Helpers
%
restoreSitArg(refRS,S,refRS(S)).
restoreSitArg(refRF,S,refRF(S)).
restoreSitArg(nonrefRS,S,nonrefRS(S)).
restoreSitArg(nonrefRF,S,nonrefRF(S)).

restoreSitArg(paperRS,S,paperRS(S)).
restoreSitArg(paperRP,S,paperRP(S)).
restoreSitArg(paperRF,S,paperRF(S)).

restoreSitArg(onlineRS,S,onlineRS(S)).
restoreSitArg(onlineRP,S,onlineRP(S)).
restoreSitArg(onlineRF,S,onlineRF(S)).

restoreSitArg(cmtRS,S,cmtRS(S)).
restoreSitArg(cmtRF,S,cmtRF(S)).

restoreSitArg(headRS,S,headRS(S)).
restoreSitArg(headRF,S,headRF(S)).


%
%
% Continuous Fluents
%
%

% The list of all continuous fluents
% ccFluentList([reputation_fl(_),gain(_)]).
% ccStateFluentList([reputation_fl(_)]).


%
% Continuous State Information
%
%ccStateShapeInfo([[reputation_fl(_), -20, 10], [gain_fl(_), 0, 5]]).
%ccStateShapeInfo([[reputation_fl(_), -20, 10]]).

%
%
% REWARD STRUCTURE
%
%

rewardEffAuth(R,S) :- (goalAchieved(S),cmtRS(S),R is 0.5); /* SAF 1 */ 
                      (goalAchieved(S),headRS(S),R is 1.0); /* SAF 2 */ 
                      (goalAchieved(S),cmtRF(S),R is 0.5);
                      (goalAchieved(S),headRF(S),R is 1.0);
                       R is 0.

rewardEffAuth_Inst(0,s0).
rewardEffAuth_Inst(R,do(A,S)) :- 
					  (A \= cmtS, A \= cmtF, A \= headS, A\= headF, R is 0);
					  (A = cmtS, R is 0.5);
					  (A = cmtF, R is 1.0);
					  (A = headS, R is 0.5);
					  (A = headF, R is 1.0).


rewardEffAppPrep(R,S) :- (goalAchieved(S), paperRS(S),  R is 0.7); /* SAF 3 */ 
                         (goalAchieved(S), paperRP(S),  R is 0.4); /* SAF 4 */
                         (goalAchieved(S), onlineRS(S), R is 1.0); /* SAF 5 */
                         (goalAchieved(S), onlineRP(S), R is 0.2); /* SAF 6 */
	                 R is 0. 

rewardEffAppPrep_Inst(0,s0).
rewardEffAppPrep_Inst(R,do(A,S)) :-
						(A \= paperS, A \= paperP, A \= onlineS, A\= onlineP, R is 0);
						(A = paperS, R is 0.7);
						(A = paperP, R is 0.4);
						(A = onlineS, R is 1.0);
						(A = onlineP, R is 0.2).	


rewardPrivacy(R,S) :- (goalAchieved(S), cmtRS(S),R is 0.2);  /* SAF 7 */
                      (goalAchieved(S), headRS(S),R is 0.8); /* SAF 8 */
                      (goalAchieved(S), cmtRF(S),R is 0.2); 
                      (goalAchieved(S), headRF(S),R is 0.8);
                       R is 0.

rewardPrivacy_Inst(0,s0).
rewardPrivacy_Inst(R,do(A,S)) :- 
						(A \= cmtS, A \= cmtF, A \= headS, A\= headF, R is 0);
						(A = cmtS, R is 0.2);
						(A = cmtF, R is 0.2);
						(A = headS, R is 0.8);
						(A = headF, R is 0.8).	

rewardReduceCost(R,S) :- (goalAchieved(S), granted(S), refRS(S),     R is 0.7); /* SAF 9 */
                         (goalAchieved(S), granted(S), nonrefRS(S),  R is 1.0); /* SAF 10 */
                         (goalAchieved(S), denied(S),  refRS(S),     R is 0.7); /* SAF 11 */
                         (goalAchieved(S), denied(S), nonrefRS(S),   R is 0.0); /* SAF 12 */
                         R is 0.0.

rewardReduceCost_Inst(0,s0).
rewardReduceCost_Inst(R,do(A,S)) :-
						(A \= cmtS, A \= cmtF, A \= headS, A\= headF, R is 0);
						(\+ refRS(S), \+ nonrefRS(S), R is 0);
						(A = cmtS, refRS(S), R is 0.7);
						(A = headS, refRS(S), R is 0.7);
						(A = cmtS, nonrefRS(S), R is 1.0);
						(A = headS, nonrefRS(S), R is 1.0);
						(A = cmtF, refRS(S), R is 0.7);
						(A = headF, refRS(S), R is 0.7);
						(A = cmtF, nonrefRS(S), R is 0.0);
						(A = headF, nonrefRS(S), R is 0.0).

/* SAF 13 */
rewardAvoidLoss(R,S) :- (goalAchieved(S), refRS(S),     R is 1.0); 
                        (goalAchieved(S), refRF(S),     R is 1.0);
                         R is 0.

rewardAvoidLoss_Inst(0,s0).
rewardAvoidLoss_Inst(R,do(A,S)) :- 
						(A \= refS, A \= refF, R is 0.0);
						(A  = refS, R is 1.0);
						(A  = refF, R is 1.0).


rewardInst(R,S) :- 	
				rewardReduceCost_Inst(RCost,S),
				rewardEffAuth_Inst(REAuth,S),
				rewardEffAppPrep_Inst(REAppPrep,S),
				rewardPrivacy_Inst(RPriv,S),
				rewardAvoidLoss_Inst(RAvLoss,S),
/* Scenario 1 */
				R is RCost*0.3 + 
				REAuth*0.02 +
				REAppPrep*0.18 + 
				RPriv*0.3 + 
				RAvLoss*0.2.



rewardCum(R,S) :- 
				rewardReduceCost(RCost,S),
				rewardEffAuth(REAuth,S),
				rewardEffAppPrep(REAppPrep,S),
				rewardPrivacy(RPriv,S),
				rewardAvoidLoss(RAvLoss,S),
% Scenario 1
				R is RCost*0.3 + 
				REAuth*0.02 +
				REAppPrep*0.18 + 
				RPriv*0.3 + 
				RAvLoss*0.2. 
	
/* Scenario 2 */
/*             R is RCost*1 + 
               REAuth*0.00 +
               REAppPrep*0.00 + 
               RPriv*0.00 + 
               RAvLoss*0.00. */
/* Scenario 3 */
/*               R is RCost*0.7 + 
               REAuth*0.00 +
               REAppPrep*0.00 + 
               RPriv*0.00 + 
               RAvLoss*0.3. */
/* Scenario 4 */
/*               R is RCost*0.0 + 
               (REAuth*0.1 + REAppPrep*0.9)*0.4 + 
               RPriv*0.6 + 
               RAvLoss*0.0. */

