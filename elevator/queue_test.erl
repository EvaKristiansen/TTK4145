
- module (queue).
- record(order,{floor,direction}).

- define(IN_KEY, 13).
- define(OUT_KEY, 42).
- define(QUEUE_PID, queue).
- define(PROCESS_GROUP_NAME, nodes). %reconsider
- define(INNER, "_inner").
- define(OUTER, "_outer").




init()->
	join_process_group(),
	Memberlist = get_member_list(),
	Queues = init_storage(dict:new(), Memberlist),
	register(?QUEUE_PID, spawn(?MODULE, queue_storage_loop, [Queues])).


join_process_group() ->
	pg2:create(?PROCESS_GROUP_NAME),
	pg2:join(?PROCESS_GROUP_NAME, self()).

get_member_list() ->
	pg2:get_members(?PROCESS_GROUP_NAME).

init_storage(Queues,MemberList)->
	case MemberList of 
		[Member | Rest] ->	
			In_name = atom_to_list(member) + "_inner",
			Out_name = atom_to_list(member) + "_outer",

			Temp_dict = dict:append(In_name, ordsets:new(), Queues),
			New_Queues = dict:append(Out_name, ordsets:new(), Temp_dict),
			init_storage(New_Queues,Rest);
		[] ->
			Queues
	end.

queue_storage_loop(Queues) ->

	receive
   	
		{add, {Pid, Key, Order}} ->
			SubjectSet = dict:find(Key, Queues),
			New_set = ordsets:add_element(Order, SubjectSet),
			Updated_queues = dict:append(New_set, dict:erase(Key, Queues)),
			queue_storage_loop(Updated_queues);

		{remove, {Key, Order}} -> 
			SubjectSet = dict:find(Key, Queues),
			New_set = ordsets:subtract(SubjectSet, Order#order.floor, Order#order.direction),
			Updated_queues = dict:append(New_set, dict:erase(Key, Queues)),
			queue_storage_loop(Updated_queues)
	end.


add_to_queue(ElevatorID, Order) -> % Er en dum funksjon, som bare setter inn basert på input, order_distributer bestemmer hvor
	
	case Order#order.direction == 0 of 
		true ->
			Key = atom_to_list(ElevatorID) + "_inner";
		false ->
			Key = atom_to_list(ElevatorID) + "_outer"
	end,

	?QUEUE_PID ! {add, {self(), Key, Order}},

	receive
		{ok,Queue} ->
			ok;
		{_,_} ->
			io:fwrite("Order was not added ~n ", []),
			error
	end.


removeFromQueue(ElevatorID, Floor) ->
	% Kjør en loop som itererer over retningene (-1, 0, 1)
	InKey = atom_to_list(ElevatorID) + "_inner",
	OutKey = atom_to_list(ElevatorID) + "_outer",
	Order1  = #order{floor = Floor, direction = -1},
	Order2  = #order{floor = Floor, direction = 0},
	Order3  = #order{floor = Floor, direction = 1},
	?QUEUE_PID ! {remove, {OutKey, Order1}},
	?QUEUE_PID ! {remove, {InKey, Order2}},
	?QUEUE_PID ! {remove, {OutKey, Order3}}.