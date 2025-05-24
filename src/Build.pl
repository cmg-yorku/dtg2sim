% DT-Golog Specification for Model: Spec 
% Date Translated: 2025-05-23 12:51:14 
% From source: Spec 
% Using DTTRanslate 
:-style_check(-discontiguous).
:-style_check(-singleton).
:- multifile getRewardMode/1.
:- multifile getRewardModeDTG/1.
:- multifile penalizeDeadlock/1.
:- multifile deadlockPenalty/1.
:- multifile getInfeasiblePenalty/1.
:- multifile val/2.
:-dynamic(init/1).



%
% OPTIONS 
% 

getNumRuns(1).
getObsType(discrete).



%
% CROSS-RUN ELEMENTS 
% 

%
% C R O S S   S T A T E 
%
transStateStructure([cost(_),overallQuality(_),reputation(_)]).




%
% EXPORTED STATE ELEMENTS 
% 

%
% D I S C R E T E   E X P O R T S
%
discreteExportedSet([deliveredInTimeA_fl,deliveredInTimeB_fl,deliveredLateA_fl,deliveredLateB_fl,neverDeliveredA_fl,neverDeliveredB_fl]).


%
% C O N T I N U O U S   E X P O R T S
%
continuousExportedSet([]).




%
% ACTION LISTS 
% 

agentActionList([orderFromSupplierA,orderFromSupplierB,assignToSubcontractorA,assignToSubcontractorB]).

agentAction(orderFromSupplierA).
agentAction(orderFromSupplierB).
agentAction(assignToSubcontractorA).
agentAction(assignToSubcontractorB).

stochasticActionList([deliveredInTimeA_Eff,neverDeliveredA_Eff,deliveredLateA_Eff,deliveredInTimeB_Eff,neverDeliveredB_Eff,deliveredLateB_Eff,goodQualityA_Eff,badQualityA_Eff,goodQualityB_Eff,badQualityB_Eff]).

stochasticAction(deliveredInTimeA_Eff).
stochasticAction(neverDeliveredA_Eff).
stochasticAction(deliveredLateA_Eff).
stochasticAction(deliveredInTimeB_Eff).
stochasticAction(neverDeliveredB_Eff).
stochasticAction(deliveredLateB_Eff).
stochasticAction(goodQualityA_Eff).
stochasticAction(badQualityA_Eff).
stochasticAction(goodQualityB_Eff).
stochasticAction(badQualityB_Eff).

nondetActions(orderFromSupplierA,_,[deliveredInTimeA_Eff,neverDeliveredA_Eff,deliveredLateA_Eff]).
nondetActions(orderFromSupplierB,_,[deliveredInTimeB_Eff,neverDeliveredB_Eff,deliveredLateB_Eff]).
nondetActions(assignToSubcontractorA,_,[goodQualityA_Eff,badQualityA_Eff]).
nondetActions(assignToSubcontractorB,_,[goodQualityB_Eff,badQualityB_Eff]).

prob(deliveredInTimeA_Eff,0.75,_).
prob(neverDeliveredA_Eff,0.05,_).
prob(deliveredLateA_Eff,0.2,_).
prob(deliveredInTimeB_Eff,0.5,_).
prob(neverDeliveredB_Eff,0.15,_).
prob(deliveredLateB_Eff,0.35,_).
prob(goodQualityA_Eff,0.7,_).
prob(badQualityA_Eff,0.3,_).
prob(goodQualityB_Eff,0.5,_).
prob(badQualityB_Eff,0.5,_).



%
% PROCEDURES 
% 

proc(orderMaterial, orderFromSupplierA # orderFromSupplierB).
proc(assignWork, assignToSubcontractorA # assignToSubcontractorB).
proc(buildRoof, orderMaterial : assignWork).



%
% FLUENT LISTS 
% 

fluentList([deliveredInTimeA_fl,neverDeliveredA_fl,deliveredLateA_fl,deliveredInTimeB_fl,neverDeliveredB_fl,deliveredLateB_fl,goodQualityA_fl,badQualityA_fl,goodQualityB_fl,badQualityB_fl]).

%
% SUCCESSOR STATE AXIOMS 
% 

deliveredInTimeA_fl(do(A,S)) :- deliveredInTimeA_fl(S); A=deliveredInTimeA_Eff.
neverDeliveredA_fl(do(A,S)) :- neverDeliveredA_fl(S); A=neverDeliveredA_Eff.
deliveredLateA_fl(do(A,S)) :- deliveredLateA_fl(S); A=deliveredLateA_Eff.
deliveredInTimeB_fl(do(A,S)) :- deliveredInTimeB_fl(S); A=deliveredInTimeB_Eff.
neverDeliveredB_fl(do(A,S)) :- neverDeliveredB_fl(S); A=neverDeliveredB_Eff.
deliveredLateB_fl(do(A,S)) :- deliveredLateB_fl(S); A=deliveredLateB_Eff.
goodQualityA_fl(do(A,S)) :- goodQualityA_fl(S); A=goodQualityA_Eff.
badQualityA_fl(do(A,S)) :- badQualityA_fl(S); A=badQualityA_Eff.
goodQualityB_fl(do(A,S)) :- goodQualityB_fl(S); A=goodQualityB_Eff.
badQualityB_fl(do(A,S)) :- badQualityB_fl(S); A=badQualityB_Eff.

%
% PRECONDITION AXIOMS 
% 

poss(deliveredInTimeA_Eff,S) :- \+ orderFromSupplierA_Att(S),\+ orderFromSupplierB_Att(S).
poss(neverDeliveredA_Eff,S) :- \+ orderFromSupplierA_Att(S),\+ orderFromSupplierB_Att(S).
poss(deliveredLateA_Eff,S) :- \+ orderFromSupplierA_Att(S),\+ orderFromSupplierB_Att(S).
poss(deliveredInTimeB_Eff,S) :- \+ orderFromSupplierB_Att(S),\+ orderFromSupplierA_Att(S).
poss(neverDeliveredB_Eff,S) :- \+ orderFromSupplierB_Att(S),\+ orderFromSupplierA_Att(S).
poss(deliveredLateB_Eff,S) :- \+ orderFromSupplierB_Att(S),\+ orderFromSupplierA_Att(S).
poss(goodQualityA_Eff,S) :- \+ assignToSubcontractorA_Att(S),\+ assignToSubcontractorB_Att(S).
poss(badQualityA_Eff,S) :- \+ assignToSubcontractorA_Att(S),\+ assignToSubcontractorB_Att(S).
poss(goodQualityB_Eff,S) :- \+ assignToSubcontractorB_Att(S),\+ assignToSubcontractorA_Att(S).
poss(badQualityB_Eff,S) :- \+ assignToSubcontractorB_Att(S),\+ assignToSubcontractorA_Att(S).


poss(orderFromSupplierA,S) :- (poss(deliveredInTimeA_Eff,S);poss(neverDeliveredA_Eff,S);poss(deliveredLateA_Eff,S)).
poss(orderFromSupplierB,S) :- (poss(deliveredInTimeB_Eff,S);poss(neverDeliveredB_Eff,S);poss(deliveredLateB_Eff,S)).
poss(assignToSubcontractorA,S) :- (poss(goodQualityA_Eff,S);poss(badQualityA_Eff,S)).
poss(assignToSubcontractorB,S) :- (poss(goodQualityB_Eff,S);poss(badQualityB_Eff,S)).

%
% SATISFACTION FORMULAE 
% 

orderFromSupplierA_Sat(S) :- deliveredInTimeA_fl(S);deliveredLateA_fl(S).
orderFromSupplierB_Sat(S) :- deliveredInTimeB_fl(S);deliveredLateB_fl(S).
assignToSubcontractorA_Sat(S) :- goodQualityA_fl(S);badQualityA_fl(S).
assignToSubcontractorB_Sat(S) :- goodQualityB_fl(S);badQualityB_fl(S).
orderMaterial_Sat(S) :- orderFromSupplierA_Sat(S);orderFromSupplierB_Sat(S).
assignWork_Sat(S) :- assignToSubcontractorA_Sat(S);assignToSubcontractorB_Sat(S).
buildRoof_Sat(S) :- orderMaterial_Sat(S),assignWork_Sat(S).

%
% ATTEMPT FORMULAE 
% 

orderFromSupplierA_Att(S) :- deliveredInTimeA_fl(S);neverDeliveredA_fl(S);deliveredLateA_fl(S).
orderFromSupplierB_Att(S) :- deliveredInTimeB_fl(S);neverDeliveredB_fl(S);deliveredLateB_fl(S).
assignToSubcontractorA_Att(S) :- goodQualityA_fl(S);badQualityA_fl(S).
assignToSubcontractorB_Att(S) :- goodQualityB_fl(S);badQualityB_fl(S).
orderMaterial_Att(S) :- orderFromSupplierA_Att(S);orderFromSupplierB_Att(S).
assignWork_Att(S) :- assignToSubcontractorA_Att(S);assignToSubcontractorB_Att(S).
buildRoof_Att(S) :- orderMaterial_Att(S);assignWork_Att(S).

%
% ROOT SATISFACTION 
% 

goalAchieved(S) :- buildRoof_Sat(S).

%
% REWARD FORMULAE 
% 

cost(V,S) :- R .

overallQuality(V,S) :- R .

reputation(V,S) :- R .


rewardInst(R,S) :- overallQuality(R,S).


%
% SENSE CONDITIONS 
% 

senseCondition(deliveredInTimeA_Eff,deliveredInTimeA_Eff_fl).
senseCondition(neverDeliveredA_Eff,neverDeliveredA_Eff_fl).
senseCondition(deliveredLateA_Eff,deliveredLateA_Eff_fl).
senseCondition(deliveredInTimeB_Eff,deliveredInTimeB_Eff_fl).
senseCondition(neverDeliveredB_Eff,neverDeliveredB_Eff_fl).
senseCondition(deliveredLateB_Eff,deliveredLateB_Eff_fl).
senseCondition(goodQualityA_Eff,goodQualityA_Eff_fl).
senseCondition(badQualityA_Eff,badQualityA_Eff_fl).
senseCondition(goodQualityB_Eff,goodQualityB_Eff_fl).
senseCondition(badQualityB_Eff,badQualityB_Eff_fl).

%
% RESTORE SITUATION ARGUMENT 
% 

restoreSitArg(deliveredInTimeA_fl,S,deliveredInTimeA_fl(S)).
restoreSitArg(neverDeliveredA_fl,S,neverDeliveredA_fl(S)).
restoreSitArg(deliveredLateA_fl,S,deliveredLateA_fl(S)).
restoreSitArg(orderFromSupplierA_Sat,S,orderFromSupplierA_Sat(S)).
restoreSitArg(orderFromSupplierA_Att,S,orderFromSupplierA_Att(S)).
restoreSitArg(deliveredInTimeB_fl,S,deliveredInTimeB_fl(S)).
restoreSitArg(neverDeliveredB_fl,S,neverDeliveredB_fl(S)).
restoreSitArg(deliveredLateB_fl,S,deliveredLateB_fl(S)).
restoreSitArg(orderFromSupplierB_Sat,S,orderFromSupplierB_Sat(S)).
restoreSitArg(orderFromSupplierB_Att,S,orderFromSupplierB_Att(S)).
restoreSitArg(goodQualityA_fl,S,goodQualityA_fl(S)).
restoreSitArg(badQualityA_fl,S,badQualityA_fl(S)).
restoreSitArg(assignToSubcontractorA_Sat,S,assignToSubcontractorA_Sat(S)).
restoreSitArg(assignToSubcontractorA_Att,S,assignToSubcontractorA_Att(S)).
restoreSitArg(goodQualityB_fl,S,goodQualityB_fl(S)).
restoreSitArg(badQualityB_fl,S,badQualityB_fl(S)).
restoreSitArg(assignToSubcontractorB_Sat,S,assignToSubcontractorB_Sat(S)).
restoreSitArg(assignToSubcontractorB_Att,S,assignToSubcontractorB_Att(S)).
restoreSitArg(orderMaterial_Sat,S,orderMaterial_Sat(S)).
restoreSitArg(orderMaterial_Att,S,orderMaterial_Att(S)).
restoreSitArg(assignWork_Sat,S,assignWork_Sat(S)).
restoreSitArg(assignWork_Att,S,assignWork_Att(S)).
restoreSitArg(buildRoof_Sat,S,buildRoof_Sat(S)).
restoreSitArg(buildRoof_Att,S,buildRoof_Att(S)).
restoreSitArg(cost(X),S,cost(X,S)).
restoreSitArg(overallQuality(X),S,overallQuality(X,S)).
restoreSitArg(reputation(X),S,reputation(X,S)).

