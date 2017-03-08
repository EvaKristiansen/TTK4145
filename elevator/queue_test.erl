
- module (queue_test).
- compile(export_all).
- record(order,{floor,direction}).

- define(IN_KEY, 13).
- define(OUT_KEY, 42).
- define(QUEUE_PID, queue).
- define(PROCESS_GROUP_NAME, nodes). %reconsider
- define(INNER, "_inner").
- define(OUTER, "_outer").

%Kan vi legge til -define(ORDER_TYPES,[up,down,inner]) eller noe i den duren?


init()->
	Memberlist = get_member_list(),
	io:fwrite("~w ~n ", [Memberlist]),
	Queues = init_storage(dict:new(), Memberlist),
	register(?QUEUE_PID, spawn(?MODULE, queue_storage_loop, [Queues])).

get_member_list() ->
	[node()] ++ nodes().	
	

init_storage(Queues,MemberList)->
	case MemberList of 
		[Member | Rest] ->	
			In_name = atom_to_list(Member) ++ "_inner",
			Out_name = atom_to_list(Member) ++ "_outer",

			Temp_dict = dict:append(In_name, ordsets:new(), Queues),
			New_Queues = dict:append(Out_name, ordsets:new(), Temp_dict),
			init_storage(New_Queues,Rest);
		[] ->
			Queues
	end.

%	end.
queue_storage_loop(Queues) ->

	receive
   	
		{add, {Pid, Key, Order}} ->
			{_ok,[SubjectSet | _Meh]} = dict:find(Key, Queues),
			New_set = ordsets:add_element(Order, SubjectSet),
			io:fwrite("~w ~n ", [New_set]),
			Updated_queues = dict:append(Key, New_set, dict:erase(Key, Queues)),
			Pid ! {ok,Updated_queues},
			queue_storage_loop(Updated_queues);

		{remove, {Key, Order}} -> 
			{_ok,[SubjectSet | _Meh]} = dict:find(Key, Queues),
			New_set = ordsets:del_element(Order,SubjectSet),
			io:fwrite("~w ~n ", [New_set]),
			Updated_queues = dict:append(Key, New_set, dict:erase(Key, Queues)),
			queue_storage_loop(Updated_queues);

		{is_in_queue, {Pid, Key, Order}} ->
			{_ok,[SubjectSet | _Meh]} = dict:find(Key, Queues),
			Pid ! {ok, ordsets:is_element(Order, SubjectSet)},
			queue_storage_loop(Queues)
				   					       
	end.


add_to_queue(ElevatorID, Floor, Direction) -> % Er en dum funksjon, som bare setter inn basert på input, order_distributer bestemmer hvor
	Order = #order{floor = Floor, direction = Direction},
	case Order#order.direction == 0 of 
		true ->
			Key = atom_to_list(ElevatorID) ++ "_inner";
		false ->
			Key = atom_to_list(ElevatorID) ++ "_outer"
	end,

	?QUEUE_PID ! {add, {self(), Key, Order}},

	receive
		{ok,_Queue} ->
			ok;
		{_,_} ->
			io:fwrite("Order was not added ~n ", []),
			error

	end.

is_order(Floor,Direction) ->
	is_order(Floor,Direction,get_member_list()).
	
is_order(Floor, Direction, MemberList) ->
	Order = #order{floor = Floor, direction = Direction},
	case MemberList of
		[Member | Rest] ->
			case Order#order.direction == 0 of 
				true ->
					Key = atom_to_list(Member) ++ "_inner";
				false ->
					Key = atom_to_list(Member) ++ "_outer"
			end,

			?QUEUE_PID ! {is_in_queue, {self(), Key, Order}},
			receive 
				{ok, false} ->
					is_order(Floor,Direction, Rest);
				{ok, true} ->
					true
				after 50 ->
					io:fwrite("isOrder recieved nothing ~n ", []),
					error
			end;
		[] ->
			false
	end.


remove_from_queue(ElevatorID, Floor) ->
	% Kjør en loop som itererer over retningene (-1, 0, 1)
	InKey = atom_to_list(ElevatorID) ++ "_inner",
	OutKey = atom_to_list(ElevatorID) ++ "_outer",
	Order1  = #order{floor = Floor, direction = -1},
	Order2  = #order{floor = Floor, direction = 0},
	Order3  = #order{floor = Floor, direction = 1},
	?QUEUE_PID ! {remove, {OutKey, Order1}},
	?QUEUE_PID ! {remove, {InKey, Order2}},
	?QUEUE_PID ! {remove, {OutKey, Order3}},
	ok.
