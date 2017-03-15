
- module (state_storage).
- export([init/2, storage_loop/3]).
- export([get_information/2, set_information/2, update_storage/1]).

- define(STATE_STORAGE_PID, ss).

init(Init_listener, Floor)->
	Memberlist = get_member_list(),
	States = init_storage(dict:new(), Memberlist, init, get_state),
	Last_known_floors = init_storage(dict:new(), Memberlist, Floor, get_last_known_floor),
	Directions = init_storage(dict:new(), Memberlist, stop, get_direction),
	register(?STATE_STORAGE_PID, spawn(?MODULE, storage_loop, [States,Last_known_floors,Directions])),
	Init_listener ! state_init_complete.

init_storage(Storagelist, [], _Inital_value, _Type) -> Storagelist;
init_storage(Storagelist, MemberList, Initial_value, Type) ->
	[Member | Rest] = MemberList,
	add_storage(Member == node(), Storagelist, Member, Rest, Initial_value, Type).

add_storage(true, Storagelist, Member, Rest, Initial_value, Type) ->
	New_storage = dict:append(Member, Initial_value , Storagelist), 
	init_storage(New_storage,Rest,Initial_value, Type);
add_storage(false, Storagelist, Member, Rest, _Initial_value, Type) ->
	{?STATE_STORAGE_PID, Member} ! {Type, {self(), Member}},
	receive
		{ok, State} ->
		State
	end,
	New_storage = dict:append(Member, State , Storagelist),
	init_storage(New_storage,Rest,State, Type).

storage_loop(States,Last_known_floors,Directions) ->
	receive
		{get_state, {Pid, Key}} ->
			{_ok,[State | _Meh]} = dict:find(Key, States),
			Pid ! {ok,State},
			storage_loop(States,Last_known_floors,Directions);

		{get_last_known_floor, {Pid, Key}} ->
			{_ok,[Last_known_floor| _Meh]} = dict:find(Key, Last_known_floors),
			Pid ! {ok,Last_known_floor},
			storage_loop(States,Last_known_floors,Directions);

		{get_direction,{Pid,Key}} ->
			{_ok,[Direction| _Meh]} = dict:find(Key, Directions),
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
			Known_information = dict:find(New_member, States),
			{Updated_states, Updated_floors, Updated_directions} = add_member_if_unkown(Known_information, New_member, {States, Last_known_floors, Directions}), % TODO REPLACEMET
			
			storage_loop(Updated_states, Updated_floors, Updated_directions)
	end.

add_member_if_unkown({ok, _}, New_member, {States, Last_known_floors, Directions}) ->
	Updated_states = dict:append(New_member, unknown, dict:erase(New_member, States)),
	{Updated_states, Last_known_floors, Directions};

add_member_if_unkown(error, New_member, {States, Last_known_floors, Directions}) ->
	Updated_states = dict:append(New_member, unknown, States),
	Updated_last_known_floors = dict:append(New_member, -1 ,Last_known_floors),
	Updated_directions = dict:append(New_member, stop, Directions),
	{Updated_states, Updated_last_known_floors, Updated_directions}.


get_information(Command, ElevatorID) ->
	?STATE_STORAGE_PID ! {Command, {self(), ElevatorID}},
	receive
		{ok, Value} ->
			Value
	end.

update_storage(ElevatorID) ->
	?STATE_STORAGE_PID ! {update, ElevatorID}.

set_information(Command, {ElevatorID, Value}) ->
	?STATE_STORAGE_PID ! {Command, {ElevatorID, Value}}.

get_member_list() ->
	[node()] ++ nodes().