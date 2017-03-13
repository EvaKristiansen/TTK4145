- module (queue_module).
- compile(export_all).
- record(order,{floor,type}).

- define(QUEUE_PID, queue).
- define(PROCESS_GROUP_NAME, nodes). %reconsider, bad name because of nodes()?
- define(INNER, "_inner").
- define(OUTER, "_outer").

%Kan vi legge til -define(ORDER_TYPES,[up,down,inner]) eller noe i den duren?


init()->
	Memberlist = get_member_list(),
	io:fwrite("~w ~n ", [Memberlist]),
	Queues = init_storage(dict:new(), Memberlist),
	register(?QUEUE_PID, spawn(?MODULE, queue_storage_loop, [Queues, none])).

get_member_list() ->
	[node()] ++ nodes().	
	

init_storage(Queues, []) -> Queues;
init_storage(Queues, Memberlist) ->
	[Member | Rest] = Memberlist,
	init_storage(Queues, Member, Rest).

init_storage(Queues, Member, Rest) ->
	In_name = atom_to_list(Member) ++ "_inner",
	Out_name = atom_to_list(Member) ++ "_outer",
	Temp_dict = dict:append(In_name, ordsets:new(), Queues),
	New_Queues = dict:append(Out_name, ordsets:new(), Temp_dict),
	init_storage(New_Queues,Rest).

update_queue(New_member) ->
	?QUEUE_PID ! {update, New_member}.


queue_storage_loop(Queues, My_next) -> %TODO: WHAT IS MEH!?
	receive

		{add, {Pid, Key, Order}} ->
			{_ok,[SubjectSet | _Meh]} = dict:find(Key, Queues),
			New_set = ordsets:add_element(Order, SubjectSet),
			Updated_queues = dict:append(Key, New_set, dict:erase(Key, Queues)),
			Pid ! {ok,Updated_queues},
			queue_storage_loop(Updated_queues, My_next);

		{remove, {Key, Order}} -> 
			{_ok,[SubjectSet | _Meh]} = dict:find(Key, Queues),
			New_set = ordsets:del_element(Order,SubjectSet),
			Updated_queues = dict:append(Key, New_set, dict:erase(Key, Queues)),
			queue_storage_loop(Updated_queues, My_next);

		{is_in_queue, {Pid, Key, Order}} ->
			{_ok,[SubjectSet | _Meh]} = dict:find(Key, Queues),
			Pid ! {ok, ordsets:is_element(Order, SubjectSet)},
			queue_storage_loop(Queues, My_next);

		{update, New_member} ->
			Known_information = dict:find(create_key(New_member, inner), Queues),
			Updated_queues = add_member_if_unkown(Known_information, New_member, Queues), 
			queue_storage_loop(Updated_queues, My_next); 

		{get_queue, {Pid, Key}} ->
			{_ok,[SubjectSet|_Meh]} = dict:find(Key, Queues),
			Pid ! {ok,SubjectSet},
			queue_storage_loop(Queues, My_next);

		{get_my_next, Pid} ->
			Pid ! My_next,
			queue_storage_loop(Queues, My_next);

		{set_my_next, Next_value} -> 
			queue_storage_loop(Queues, Next_value)

	end.

set_my_next(Next_value) -> 
	?QUEUE_PID ! {set_my_next, Next_value}.

get_my_next() ->
	?QUEUE_PID ! {get_my_next, self()},
	receive 
		My_next ->
			My_next
	end.

add_member_if_unkown({ok, _}, _New_member, Queues) -> Queues;
add_member_if_unkown(error, New_member, Queues) ->
	In_name = create_key(New_member, inner),
	Out_name = create_key(New_member, outer),
	Temp_dict = dict:append(In_name, ordsets:new(), Queues),
	dict:append(Out_name, ordsets:new(), Temp_dict).


add_to_queue(ElevatorID, Order) -> 
	Key = create_key(ElevatorID, Order#order.type), 

	?QUEUE_PID ! {add, {self(), Key, Order}},

	receive
		{ok,_Queue} ->
			ok;
		{_,_} ->
			error

	end.

create_key(ElevatorID, inner) -> atom_to_list(ElevatorID) ++ "_inner";
create_key(ElevatorID, outer) -> atom_to_list(ElevatorID) ++ "_outer";
create_key(ElevatorID, up) -> atom_to_list(ElevatorID) ++ "_outer";
create_key(ElevatorID, down) -> atom_to_list(ElevatorID) ++ "_outer".

is_order(Order) ->
	is_order(Order,get_member_list()).
	


is_order(_Order, []) -> false;
is_order(Order, MemberList) ->
	[Member | Rest] = MemberList,
	is_order(Order, Member, Rest).

is_order(Order, Member, Rest) ->
	Key = create_key(Member, Order#order.type),
	?QUEUE_PID ! {is_in_queue, {self(), Key, Order}},
	receive 
		{ok, false} ->
			is_order(Order, Rest);
		{ok, true} ->
			true
	end.


is_floor_in_queue(ElevatorID, Floor) ->
	InKey = create_key(ElevatorID, inner),
	OutKey = create_key(ElevatorID, outer),
	Order1  = #order{floor = Floor, type = down},
	Order2  = #order{floor = Floor, type = inner},
	Order3  = #order{floor = Floor, type = up},

	?QUEUE_PID ! {is_in_queue, {self(), OutKey, Order1}},
	receive
		{ok, Value1} ->
			true
	end,
	?QUEUE_PID ! {is_in_queue, {self(), InKey, Order2}},
	receive
		{ok, Value2} ->
			true
	end,
	?QUEUE_PID ! {is_in_queue, {self(), OutKey, Order3}},
	receive
			{ok, Value3} ->
				true
		end,
	Value1 or Value2 or Value3.

remove_from_queue(true, ElevatorID, Floor) ->
	InKey = create_key(ElevatorID, inner),
	OutKey = create_key(ElevatorID, outer),
	Order1  = #order{floor = Floor, type = down},
	Order2  = #order{floor = Floor, type = inner},
	Order3  = #order{floor = Floor, type = up},
	?QUEUE_PID ! {remove, {OutKey, Order1}},
	?QUEUE_PID ! {remove, {InKey, Order2}},
	?QUEUE_PID ! {remove, {OutKey, Order3}},
	ok;
remove_from_queue(false, ElevatorID, Floor) ->
	OutKey = create_key(ElevatorID, outer),
	Order1  = #order{floor = Floor, type = down},
	Order3  = #order{floor = Floor, type = up},
	?QUEUE_PID ! {remove, {OutKey, Order1}},
	?QUEUE_PID ! {remove, {OutKey, Order3}},
	ok.




 get_queue_set(ElevatorID, Preposition) ->
	?QUEUE_PID ! {get_queue, {self(),create_key(ElevatorID, Preposition)}},
	receive 
		{ok, Set} ->
			ok
	end,
	Set.
