
- module (state_storage). %Was thinking file is not a state machine, but a state storage?
- compile(export_all). % Turns out, loops have to be exported to be registered like we do... this may not be very neat
- define(STATE_STORAGE_PID, ss). %Maybe we can send to this process on other computers when it is registered like this, at least, the simple add functions used to just send are not needed!

init(Floor)-> % Floor? DEBUG
	Memberlist = get_member_list(),
	io:fwrite("Memberlist: ~w ~n ", [Memberlist]),
	States = init_storage(dict:new(), Memberlist, init),
	Last_known_floors = init_storage(dict:new(), Memberlist, Floor),  % -1 for Floor? 
	Directions = init_storage(dict:new(), Memberlist, stop),

	register(?STATE_STORAGE_PID, spawn(?MODULE, storage_loop, [States,Last_known_floors,Directions])).

get_member_list() ->
	[node()] ++ nodes().	
	

init_storage(StorageList,MemberList,Initial_value)-> %Consider merging these three functions!
%	case MemberList of 
%		[Member | Rest] ->	
%			New_storage = dict:append(Member, Initial_value , StorageList),  %Assuming we will not initialize state machine unless in state init
%			init_storage(New_storage,Rest,Initial_value);
%		[] ->
%			StorageList
%	end.
	case MemberList of 
		[Member | Rest] ->	
			case (Member == node()) of 
				true ->
					New_storage = dict:append(Member, Initial_value , StorageList),  %Assuming we will not initialize state machine unless in state init
					io:fwrite("Initializing state_floor as: ~w for elevator: ~w ~n",[Initial_value, Member]), %DEBUG
					init_storage(New_storage,Rest,Initial_value);
				false ->
					{?STATE_STORAGE_PID, Member} ! {get_state, {self(), Member}}, % Spør den gitte noden om dens egen state
					receive
						{ok, State} ->
						State
					end,
					New_storage = dict:append(Member, State , StorageList),  %Assuming we will not initialize state machine unless in state init
					init_storage(New_storage,Rest,State)
			end;
		[] ->
			StorageList
	end.

storage_loop(States,Last_known_floors,Directions) ->
	receive
		{get_state, {Pid, Key}} ->
			{_ok,[State | _Meh]} = dict:find(Key, States),
			%io:fwrite("~w ~n ", [State]), %Debug
			Pid ! {ok,State},
			storage_loop(States,Last_known_floors,Directions);

		{get_last_known_floor, {Pid, Key}} ->
			{_ok,[Last_known_floor| _Meh]} = dict:find(Key, Last_known_floors),
			io:fwrite("Floor: ~w ~n", [Last_known_floor]), %Debug
			Pid ! {ok,Last_known_floor},
			storage_loop(States,Last_known_floors,Directions);

		{get_direction,{Pid,Key}} ->
			{_ok,[Direction| _Meh]} = dict:find(Key, Directions),
			io:fwrite("Direction: ~w ~n", [Direction]), %Debug
			Pid ! {ok,Direction},
			storage_loop(States,Last_known_floors,Directions);

		{set_state, {Key,State}} -> 
			Updated_states = dict:append(Key, State, dict:erase(Key, States)),
			storage_loop(Updated_states,Last_known_floors,Directions);

		{set_last_known_floor, {Key,Last_known_floor}} -> 
			Updated_last_known_floors = dict:append(Key, Last_known_floor, dict:erase(Key, Last_known_floors)),
			storage_loop(States,Updated_last_known_floors,Directions);

		{set_direction, {Key,Direction}} -> 
			Updated_directions = dict:append(Key, Direction, dict:erase(Key, Directions)),
			storage_loop(States,Last_known_floors,Updated_directions);

		{update, New_member} ->
			case dict:find(New_member,States) of
			{ok,_} ->
				Updated_states = dict:append(New_member, unknown, dict:erase(New_member, States)), %TODO tenk på unknown
				storage_loop(Updated_states,Last_known_floors,Directions);
			error -> 
				Updated_states = dict:append(New_member, unknown, States), %TODO tenk på unknown
				Updated_last_known_floors = dict:append(New_member, -1 ,Last_known_floors),
				Updated_directions = dict:append(New_member, 0, Directions),
				storage_loop(Updated_states,Updated_last_known_floors,Updated_directions)
			end

	end.

get_state(ElevatorID) ->
	?STATE_STORAGE_PID ! {get_state, {self(),ElevatorID}},
	receive
		{ok, State} ->
			State
	end.

get_last_floor(ElevatorID) ->
	?STATE_STORAGE_PID ! {get_last_known_floor, {self(),ElevatorID}},
	receive
		{ok, Floor} ->
			Floor
	end.

get_direction(ElevatorID) ->
?STATE_STORAGE_PID ! {get_direction, {self(),ElevatorID}},
	receive
		{ok, Direction} ->
			Direction
	end.

update_storage(ElevatorID) ->
	?STATE_STORAGE_PID ! {update, Member}.

update_state(ElevatorID, New_state) ->
	?STATE_STORAGE_PID ! {set_state, {ElevatorID, New_state}}.

update_floor(ElevatorID, New_floor) ->
	?STATE_STORAGE_PID ! {set_last_known_floor, {ElevatorID, New_floor}}.

updated_direction(ElevatorID, New_direction) ->
	?STATE_STORAGE_PID ! {set_direction, {ElevatorID, New_direction}}.
