:-style_check(-discontiguous).
:-style_check(-singleton).


/******  State Shape Information **** 

State shape information is offered in a predicate in the form of a list:

ccStateShapeInfo([[state_fluent1(), minVal2, maxVal2],
				  [state_fluent2(), minVal2, maxVal2],
				  ...
				  ]).

The following routines extract elements from this list.

*/

/* 
getStateShapeInfo(-Terms,-Mins,-Maxs)
Reads the list of state fluents and retruns separate lists with the term literal (Terms) and the Minimum and Maximum values specified
*/
unwrapStateShapeInfo([],[],[],[]).
unwrapStateShapeInfo([Top|Rest],[TopTerm|RestTerm],[TopMin|RestMin],[TopMax|RestMax]) :-
							unwrapStateShapeInfo(Rest,RestTerm,RestMin,RestMax),
							nth0(0,Top,TopTerm),
							nth0(1,Top,TopMin),
							nth0(2,Top,TopMax).


/* C O N T I N U O U S   S T A T E */


trueCCFluents([],_,[]).
trueCCFluents([TopPool|Pool],S,[TopPool|Result]) :- 
			holds(TopPool,S),trueCCFluents(Pool,S,Result),!.
trueCCFluents([TopPool|Pool],S,Result) :- 
			trueCCFluents(Pool,S,Result).


extractValues([],[]).
extractValues([Top|Fs],[TopRes|Res]) :- 
		extractValues(Fs,Res),
		arg(1,Top,TopRes).


/* D I S C R E T E   S T A T E */


whatIsTrue([],_,[]).
whatIsTrue([TopPool|Pool],S,[TopResult|Result]) :- 
			((holds(TopPool,S),TopResult is 1);
			(\+ holds(TopPool,S),TopResult is 0)),
			whatIsTrue(Pool,S,Result).

/*
getStateG(+S,-Res)
From a situation S returns a binary list marking the fluents that are true.
*/
/* getStateG(S,Res) :- fluentList(Fs),
					whatIsTrue(Fs,S,Res),write(Res).
*/
getStateG(S,Res) :- discreteExportedSet(Fs),
					whatIsTrue(Fs,S,Res).


/* 
constructSituation/2
From a list of action index number 0..(numAction-1) construct a situation 
*/
constructSituation(NumActionList,S) :-  reverse(NumActionList,A),constructSit(A,S).
%constructSit([],[]).
constructSit([],s0).
constructSit([TopList|List],do(Res,S)):-  
					stochasticActionList(Actions),
					nth0(TopList,Actions,Res),
					constructSit(List,S).


getProbs([],_,[]).
getProbs([TopAction|StochActionList],S,[TopProb|Probs]) :- 
		prob(TopAction,TopProb,S),
		getProbs(StochActionList,S,Probs).


/*
getRewardRL(+SNum,-R).
+SNum: a list of indexes of stochastic actions, represneting the current situation.
*/
getRewardRL_(SNum,R) :- 
		getRewardMode(episodic),!,
		constructSituation(SNum,S),
		rewardEpis(R,S).
		
getRewardRL_(SNum,R) :- !,
		getRewardMode(instant),!,
		constructSituation(SNum,S),
		rewardInst(R,S).
		
getRewardRL_(SNum,R) :- 
		\+ (getRewardMode(instant);getRewardMode(cummulative);getRewardMode(episodic)),
		write("ERROR: No reward mode declared.").



%
% Episodic reward: cummulative at the end of the episode
%
rewardEpis(R,S) :- \+ goalAchieved(S),\+ deadlock(S),!,
				R is 0. 
rewardEpis(R,S) :- rewardInst(R,S). 

%
% DTGolog Reward
%

%reward(R,S) :- penalizeDeadlock(1), deadlock(S), deadlockPenalty(R).

dt_reward(R,S) :- getRewardModeDTG(instant),!,
				rewardInst(R,S).
dt_reward(R,S) :- getRewardModeDTG(episodic),!,
				rewardEpis(R,S).				


reward(R,S) :- deadlock(S),current_predicate(deadlockPenalty/1),!,
				deadlockPenalty(Pen),
				dt_reward(R1,S),
				R is R1+Pen.
reward(R,S) :- dt_reward(R,S).

/*
reward(R,S) :- \+ (penalizeDeadlock(1), deadlock(S), deadlockPenalty(R)),
				getRewardModeDTG(instant),
				rewardInst(R,S).
reward(R,S) :- \+ (penalizeDeadlock(1), deadlock(S), deadlockPenalty(R)),
				getRewardModeDTG(episodic),
				rewardEpis(R,S).				
*/


/* 
 * CONTROL PREDICATES
 */
deadlock(S) :- noActionPossible(S),\+ goalAchieved(S).



/* 
HELPERS!
*/


/* 
From items to index [and reverse]
*/
fromItemsToIndex([],_,[]).
fromItemsToIndex([TopItem|ItemList],Pool,[TopIndex|IndexList]):-
	nth0(TopIndex,Pool,TopItem),
	fromItemsToIndex(ItemList,Pool,IndexList).


% possibleAgentActionsNum(+SNum,ActionList)
% + SNum: a list of indexes of Stochastic Actions from the first to the last
% - Res: A list of indexes of agent Actions.
possibleAgentActionsNum(SNum,Res) :- constructSituation(SNum,S),
								possibleAgentActions(S,Res).


 
% possibleAgentActions(+Situation,ActionList)
% + Situation: a Golog situation
% - ActionList: A list of indexes of Agent Actions.
possA(X,S):-agentAction(X),poss(X,S).
possibleAgentActions(S,Res) :- 
								setof(X, possA(X,S), Bag),
								agentActionList(Pool),
								fromItemsToIndex(Bag,Pool,Res).


% noActionPossibleAgentActions(+Situation)
% + Situation: a Golog situation
noActionPossible(S) :- \+ (setof(X, possA(X,S), Bag),length(Bag,X),X > 0).
/* noActionPossible(S) :- \+ (setof(X, poss(X,S), Bag),length(Bag,X),X > 0). */


/*
DEPRECATED????
findVal(-X,+A,+T)
Given an predicate term T, find its value (X) with a list A that contains it unified with that value.
-X: The value
+A: The list of instantiated predicates
+T: The predicate name we are interested in.
*/
/* findVal(X,[Top|Rest],T) :- functor(Top,T,1),arg(1,Top,X),!.
findVal(1,[Top|Rest],T) :- functor(Top,T,0),!.
findVal(X,[Top|Rest],T) :- findVal(X,Rest,T).*/


/*
getInitVal(+Predicate,-Value)
Given a predicate/functor +Predicate, find its value Value as it appears in the init list.
Return 0 if init does not exist or predicate is not in the list.
+Predicate: The Predicate (its functor)
-Value: The value
*/
getInitValue(Predicate,Value):- \+ current_predicate(init/1), Value is 0,!. /* never happening really */
getInitValue(Predicate,Value):- current_predicate(init/1), init([]), Value is 0,!. 
getInitValue(Predicate,Value):- current_predicate(init/1), \+ init(_), Value is 0,!. 
getInitValue(Predicate,Value):- current_predicate(init/1), init(X), getArgVal(Predicate,X,Value).


getArgVal(Predicate,[Top|Rest],Value) :- functor(Top,Predicate,1),arg(1,Top,Value),!.
getArgVal(Predicate,[Top|Rest],Value) :- getArgVal(Predicate,Rest,Value),!.
getArgVal(Predicate,[],0):-!.


initiallyTrue(Predicate):- current_predicate(init/1), init(X), memberchk(Predicate,X). 


% binary_list_to_int(+BinaryList, -Int)
binary_list_to_int(Bits, Int) :-
    binary_list_to_int(Bits, 0, Int).

% Helper: binary_list_to_int(+Bits, +Acc, -Result)
binary_list_to_int([], Acc, Acc).
binary_list_to_int([Bit|Rest], Acc, Result) :-
    Acc1 is Acc * 2 + Bit,
    binary_list_to_int(Rest, Acc1, Result).
	
	
% filter_by_mask(+InputList, +MaskList, -ResultList)
filter_by_mask([], [], []) :- !.
filter_by_mask([X|Xs], [1|Ms], [X|Ys]) :-
    !,
    filter_by_mask(Xs, Ms, Ys).
filter_by_mask([_|Xs], [0|Ms], Ys) :-
    !,
    filter_by_mask(Xs, Ms, Ys).
	
	
% items_at_indexes(+List, +Indexes, -Sublist)
% Sublist is a subset of List that match the indexes in Indexes 
items_at_indexes(_, [], []) :- !.
items_at_indexes(List, [I|Is], [X|Xs]) :-
    nth0(I, List, X),
    items_at_indexes(List, Is, Xs).
	
	
/*
val(-Value,+Pred)
Value is 1 if Pred is satisfied, 0 otherwise
+Pred: an predicate (a fluent in our case).
*/
val(1, Pred) :- call(Pred), !.
val(0, _).