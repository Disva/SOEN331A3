% Mathieu Beauchemin ID: 6760953, Dmitry Svoiski 6893570
% SOEN 331
% Assignment # 3
% Due Date: March 30 2015

% state(S) - implies S is a state
% superstate(S1,S2) - implies S1 is a superstate of S2
% initialstate(S) - implies S is the initial state

% transition(Source, Destination, Event, Guard, Action) - implies there is a transition
% between states Source and Destination which occurs given an Event and Guard, and which
% also performs an action. If noevent, noguard, and noaction are convention for leaving
% them out.

% composite_state(S) - returns set of all superstates in the system.
composite_state(S) :-
	findall(Composite,superstate(Composite,_),L),
	list_to_set(L,S).

% nested_state(S) - succeeds if there exists a superstate of S
nested_state(S) :- superstate(_,S).

% total_degree(Result) - returns 2 times the number of transitions in the system.
% A transition has an event or a guard or both.
total_degree(Result) :-
	findall(Event,transition(_,_,Event,noguard,_),L1), % find all guardless events
	length(L1,N1),
	findall(Event,transition(_,_,noevent,Guard,_),L2), % find all eventless guards
	length(L2,N2),
	findall(Event,transition(_,_,not(noevent),not(noguard),_),L3), % find all eventful guards (haha)
	length(L3,N3),
	N4 is N1+N2+N3,
	Result is N4*2.

% indegree(State,ID) - returns indegree of State as ID.
% Again, a transition has an event or a guard or both.
indegree(State,ID) :-
	findall(Event, transition(_,State,Event,noguard,_),L1), % find all guardless events
	length(L1,N1),
	findall(Event, transition(_,State,noevent,Guard,_),L2), % find all guardless events
	length(L1,N2),
	findall(Event, transition(_,State,not(noevent),not(noguard),_),L3), % find all eventful guards (hehe)
	length(L1,N3),
	ID is N1+N2+N3.

% outdegree(State,OD) - returns indegree of State as OD.
% Again, a transition has an event or a guard or both.
indegree(State,ID) :-
	findall(Event, transition(State,_,Event,noguard,_),L1), % find all guardless events
	length(L1,N1),
	findall(Event, transition(State,_,noevent,Guard,_),L2), % find all guardless events
	length(L1,N2),
	findall(Event, transition(State,_,not(noevent),not(noguard),_),L3), % find all eventful guards (hehe)
	length(L1,N3),
	OD is N1+N2+N3.

% events_per_state(State, EventSet) - returns set of events within a composite State as EventSet
events_per_state(State, EventSet) :-
	findall(Event,
		(superstate(State,Substate),
		transition(Substate,_,Event,_,_)),
		EventList),
	list_to_set(EventList,EventSet).

% multigraph :- succeeds if the system contains parallel edges
multigraph :-
	transition(S,D,E1,G1,_),
	transition(S,D,E2,G2,_),
	((E1 \== E2) ; (G1 \== G2)).

% included_states(S,L) - returns list of states within S as L
included_states(S,L) :-
	findall(State,superstate(S,State),L).

% events(EventSet) - returns set of all events in system as EventSet
events(EventSet) :-
	findall(Event,transition(_,_,Event,_,_),L),
	list_to_set(L,EventSet).

% reachable(Target,Source) - succeeds if Target state is reachable by Source state via transitions
reachable(Target,Source) :-
	transition(Source,Target,_,_,_).
reachable(Target,Source) :-
	transition(Source,Node,_,_,_),
	reachable(Target,Node).
