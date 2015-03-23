% Mathieu Beauchemin ID: 6760953, Dmitry Svoiski 6893570
% SOEN 331
% Assignment # 3
% Due Date: March 30 2015

% state(S) - implies S is a state
% below are the main "safe room" states
state(dormant).
state(init).
state(safe_shutdown).
state(error_diagnosis).
state(idle).
state(monitoring).
% below are the init state states
state(boot_hw).
state(senchk).
state(tchk).
state(psichk).
state(ready).
% below are the error_diagnosis state states
state(error_rcv).
state(applicable_rescue).
state(reset_module_data).
% below are the monitoring state states
state(monidle).
state(regulate_environment).
state(lockdown).
% below are the lockdown state states
state(prep_vpurge).
state(alt_temp).
state(alt_psi).
state(risk_assess).
state(safe_status).

% superstate(S1,S2) - implies S1 is a superstate of S2
superstate(null,dormant).
superstate(null,init).
superstate(null,safe_shutdown).
superstate(null,error_diagnosis).
superstate(null,idle).
% below are the init state superstates
superstate(init, boot_hw).
superstate(init, senchk).
superstate(init, tchk).
superstate(init, psichk).
superstate(init, ready).
% below are the error_diagnosis state superstates
superstate(error_diagnosis, error_rcv).
superstate(error_diagnosis, applicable_rescue).
superstate(error_diagnosis, reset_module_data).
% below are the monitoring state superstates
superstate(monitoring, monidle).
superstate(monitoring, regulate_environment).
superstate(monitoring, lockdown).
% below are the lockdown state superstates
superstate(lockdown, prep_vpurge).
superstate(lockdown, alt_temp).
superstate(lockdown, alt_psi).
superstate(lockdown, risk_assess).
superstate(lockdown, safe_status).

% initialstate(S) - implies S is the initial state
% below is the main "safe room" initial state
initialstate(dormant).
% below is the init state initial state
initialstate(boot_hw).
% below is the error_diagnosis initial state
initialstate(error_rcv).
% below is the monitoring initial state
initialstate(monidle).
% below is the lockdown initial state
initialstate(prep_vpurge).

% transition(Source, Destination, Event, Guard, Action) - implies there is a transition
% between states Source and Destination which occurs given an Event and Guard, and which
% also performs an action. If there is no event/ guard / action, then
% noevent, noguard, and noaction are convention for leaving them out.
% below are the main "safe room" transitions
transition(dormant, init, start, noguard, noaction).
transition(init, idle, init_ok, noguard, noaction).
transition(init, error_diagnosis, init_crash, noguard, 'broadcast init_err_msg').
transition(error_diagnosis, init, retry_init, 'num_tries < 3', 'num_tries++').
transition(idle, monitoring, begin_monitoring, noguard, noaction).
transition(idle, error_diagnosis, idle_crash, noguard, 'broadcast idle_err_msg').
transition(error_diagnosis, idle, idle_rescue, noguard, noaction).
transition(monitoring, error_diagnosis, monitor_crash, 'inlockdown = false','broadcast moni_err_msg').
transition(error_diagnosis, monitoring, moni_rescue, noguard, noaction).
transition(error_diagnosis, safe_shutdown, shutdown, 'num_tries = 3', noaction).
transition(safe_shutdown, dormant, sleep, noguard, noaction).
% below are the init state transitions
transition(boot_hw, senchk, hw_ok, noguard, noaction).
transition(senchk, tchk, sen_ok, noguard, noaction).
transition(tchk, psichk, t_ok, noguard, noaction).
transition(psichk, ready, psi_ok, noguard, noaction).
% below are the error_diagnosis state transitions
transition(error_rcv, applicable_rescue, noevent, 'err_protocol_def = true', apply_protocol_rescues).
transition(error_rcv, reset_module_data, noevent, 'err_protocol_def = false', reset_to_stable).
% below are the monitoring state transitions
transition(monidle, regulate_environment, no_contagion, noguard, noaction).
transition(monidle, lockdown, contagion_alert, noguard, ['broadcast FACILITY_CRIT_MESG'|'inlockdown = true']).
transition(regulate_environment, monidle, after_100ms, noguard, noaction).
transition(lockdown, monidle, purge_succ, noguard, 'inlockdown = false').
% below are the lockdown state transitions
transition(prep_vpurge, alt_temp, initiate_purge, noguard, lock_doors).
transition(prep_vpurge, alt_psi, initiate_purge, noguard, lock_doors).
transition(alt_temp, risk_assess, tcyc_comp, noguard, noaction).
transition(alt_psi, risk_assess, psicyc_comp, noguard, noaction).
transition(risk_assess, safe_status, noevent, 'risk < 1%', unlock_doors).
transition(risk_assess, prep_vpurge, noevent, 'risk > 1%', noaction).

% composite_state(S) - returns set of all superstates in the system as S.
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
	findall(Guard,transition(_,_,noevent,Guard,_),L2), % find all eventless guards
	length(L2,N2),
	findall([Event|Guard],transition(_,_,(Event,not(noevent)),(Guard,not(noguard)),_),L3), % find all eventful guards (haha)
	length(L3,N3),
	N4 is N1+N2+N3,
	Result is N4*2.

% indegree(State,ID) - returns indegree of State as ID.
% Again, a transition has an event or a guard or both.
indegree(State,ID) :-
	findall(Event, transition(_,State,Event,noguard,_),L1), % find all guardless events
	length(L1,N1),
	findall(Guard, transition(_,State,noevent,Guard,_),L2), % find all guardless events
	length(L1,N2),
	findall([Event|Guard], transition(_,State,(Event,not(noevent)),(Guard,not(noguard)),_),L3), % find all eventful guards (hehe)
	length(L1,N3),
	ID is N1+N2+N3.

% outdegree(State,OD) - returns indegree of State as OD.
% Again, a transition has an event or a guard or both.
outdegree(State,OD) :-
	findall(Event, transition(State,_,Event,noguard,_),L1), % find all guardless events
	length(L1,N1),
	findall(Guard, transition(State,_,noevent,Guard,_),L2), % find all guardless events
	length(L1,N2),
	findall([Event|Guard], transition(State,_,(Event,not(noevent)),(Guard,not(noguard)),_),L3), % find all eventful guards (hehe)
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

% is_loop(Event, Guard) - succeeds by Finding a loop edge. We assume that an edge
% can be represented by a non-null event-guard pair.
% INCOMPLETE

% all_loops(Set) - returns set of all loop edges as Set.
% INCOMPLETE

% is_edge(Event, Guard) succeeds if there exists an edge with specific Event and Guard.
is_edge(Event, Guard) :-
	transition(_,_,Event,Guard,_).

% size(Length) - returns number of edges in EFSM as Length.
size(Length) :-
	findall(Event,transition(_,_,Event,noguard,_),L1), % find all guardless events
	length(L1,N1),
	findall(Guard,transition(_,_,noevent,Guard,_),L2), % find all eventless guards
	length(L2,N2),
	findall([Event|Guard],transition(_,_,(Event,not(noevent)),(Guard,not(noguard)),_),L3), % find all eventful guards (haha)
	length(L3,N3),
	Length is N1+N2+N3.
	
% is_link(Event, Guard) - succeeds if Event Guard pair is a link edge
is_link(Event,Guard) :-
	
% all_superstates(Set) - returns all superstates in the system as Set.
all_superstates(Set) :-
	findall(Superstate,superstate(Superstate,_),L),
	list_to_set(L,Set).

% ancestor(Ancestor, Descendant) - succeeds if state Descendant has ancestor state Ancestor
% INCOMPLETE
ancestor(Ancestor, Descendant) :-
	superstate(Ancestor,Descendant).
ancestor(Ancestor, Descendant) :-
	superstate(Node,Descendant),
	ancestor(Ancestor, Node).
	
% inherits_transitions(State, List) - returns all transitions inherited by State as List
% INCOMPLETE
inherits_transitions(State, List) :-
	findall(Ancestor,transition(ancestor(Ancestor,State),_,_,_,_),List).

% all_states(L) - returns list of all states as L.
all_states(L) :-
	findall(S,state(S),L).
% all_init_states(L) - returns list of all starting states as L.
all_init_states(L) :-
	findall(S,initialstate(S),L).

% get_starting_state(State) - succeeds by returning the top-level starting state
get_starting_state(State) :-
	initialstate(State),
	superstate(null,State).
	
% state_is_reflexive(State) - succeeds if State has a transition to itself
state_is_reflexive(State) :-
	transition(State,State,_,_,_).
	
graph_is_reflexive - succeeds if the entire EFSM is reflexive
graph_is_reflexive :-
	get_starting_state(S),
	reachable(S,S).
	
% get_guards(Ret) - returns set of all guards as Ret
get_guards(Ret) :-
	findall(Guard,transition(_,_,_,(Guard,not(noguard)),_),L),
	list_to_set(L,Ret).
	
% get_events(Ret) - returns set of all events as Ret
get_events(Ret) :-
	findall(Event,transition(_,_,(Event,not(noevent)),_,_),L),
	list_to_set(L,Ret).
	
% get_actions(Ret) - returns set of all actions as Ret
get_actions(Ret) :-
	findall(Action,transition(_,_,_,_,(Action,not(noaction))),L),
	list_to_set(L,Ret).

% get_only_guarded(Ret) - returns set of state pairs that are associated by guards as Ret
get_only_guarded(Ret) :-
	findall([S1|S2],transition(S1,S2,_,not(noguard),_),L),
	list_to_set(L,Ret).

% legal_events_of(State, L) - returns list of legal event-guard pairs to State as L.
% INCOMPLETE
legal_events_of(State, L) :-
	findall([Event|Guard],transition(State,_,Event,Guard,_),L).
