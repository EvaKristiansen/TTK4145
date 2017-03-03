
- module (fsm).
- export(init/0, get_state/1, set_state/1,get_direction/1,set_direction/2,get_last_known_floor/1,set_last_known_floor/2).
- define(FSM_PID, fsm). %Maybe we can send to this process on other computers when it is registered like this?

init()->
	Memberlist = get_member_list(),
	io:fwrite("~w ~n ", [Memberlist]),
	States = init_storage(dict:new(), Memberlist),
	register(?FSM_PID, spawn(?MODULE, fsm_storage_loop, [States])).

get_member_list() ->
	[node()] ++ nodes().	
	

init_storage(States,MemberList)->
	case MemberList of 
		[Member | Rest] ->	
			New_states = dict:append(Out_name, init , Temp_dict),  %Assuming we will not initialize state machine unless in state init
			init_storage(New_states,Rest);
		[] ->
			States
	end.


fsm_storage_loop(States) ->
	receive
		{get, {Pid, Key} ->
			{_ok,[State | _Meh]} = dict:find(Key, States),
			io:fwrite("~w ~n ", [State]),
			Pid ! {ok,State},
			fsm_storage_loop(States);

		{set, {Key,State}} -> 
			Updated_states = dict:append(Key, State, dict:erase(Key, States)),
			fsm_storage_loop(Updated_states);		   					       
	end.

get_state(ElevatorId) ->
	?FSM_PID ! {get,{self(),ElevatorId}}.
	receive
		{ok,State} ->
			State
	end.

set_state(ElevatorId,State) ->
	?FSM_PID ! {set,{ElevatorId,State}}.

