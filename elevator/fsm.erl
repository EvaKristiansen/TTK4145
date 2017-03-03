
- module (state_storage). %Was thinking file is not a state machine, but a state storage?
- export(init/0).
- define(STATE_STORAGE_PID, ss). %Maybe we can send to this process on other computers when it is registered like this, at least, the simple add functions used to just send are not needed!

init()->
	Memberlist = get_member_list(),
	io:fwrite("~w ~n ", [Memberlist]),
	States = init_state_storage(dict:new(), Memberlist),
	Last_known_floors = init__floor_storage(dict:new(), Memberlist), %Reconsider function name and structure
	Directions = init_direction_storage(dict:new(), Memberlist),
	register(?FSM_PID, spawn(?MODULE, fsm_storage_loop, [States,Last_known_floors,Directions])).

get_member_list() ->
	[node()] ++ nodes().	
	

init_state_storage(States,MemberList)->
	case MemberList of 
		[Member | Rest] ->	
			New_states = dict:append(Member, init , States),  %Assuming we will not initialize state machine unless in state init
			init_storage(New_states,Rest);
		[] ->
			States
	end.

init__floor_storage(Last_known_floors, Memberlist) ->
	case MemberList of 
		[Member | Rest] ->	
			New_last_known_floors = dict:append(Member, -1 , Last_known_floors),  %Assuming the actual last known floor will be set in main, using -1 as dummy variable
			init_storage(New_last_known_floors,Rest);
		[] ->
			Last_known_floors
	end.

init__direction_storage(Directions, Memberlist) ->
	case MemberList of 
		[Member | Rest] ->	
			New_directions= dict:append(Member, 0 , Directions),  %Assuming the actual last known floor will be set in main, using 0 as dummy variable(is also rather probable)
			init_storage(New_last_known_floors,Rest);
		[] ->
			Directions
	end.

fsm_storage_loop(States,Last_known_floors,Directions) ->
	receive
		{get_state, {Pid, Key}} ->
			{_ok,[State | _Meh]} = dict:find(Key, States),
			io:fwrite("~w ~n ", [State]), %Debug
			Pid ! {ok,State},
			fsm_storage_loop(States,Last_known_floors,Directions);

		{get_last_known_floor, {Pid, Key}} ->
			{_ok,[Last_known_floor| _Meh]} = dict:find(Key, Last_known_floors),
			io:fwrite("~w ~n ", [Last_known_floor]), %Debug
			Pid ! {ok,Last_known_floor},
			fsm_storage_loop(States,Last_known_floors,Directions);

		{get_direction,{Pid,Key}} ->
			{_ok,[Direction| _Meh]} = dict:find(Key, Directions),
			io:fwrite("~w ~n ", [Direction]), %Debug
			Pid ! {ok,Direction},
			fsm_storage_loop(States,Last_known_floors,Directions);

		{set_state, {Key,State}} -> 
			Updated_states = dict:append(Key, State, dict:erase(Key, States)),
			fsm_storage_loop(Updated_states);

	end.
