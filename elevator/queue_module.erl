- module (queue_module).
- compile(export_all).
- record(order,{floor,type}).

- define(IN_KEY, 13). %TODO: REMOVE!
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

update_queue(New_member) ->
	?QUEUE_PID ! {update, New_member}.


queue_storage_loop(Queues) -> %TODO: WHAT IS MEH!?
	receive
   	
		{add, {Pid, Key, Order}} ->
			{_ok,[SubjectSet | _Meh]} = dict:find(Key, Queues),
			New_set = ordsets:add_element(Order, SubjectSet),
			io:fwrite("Adding: ~w ~n ", [New_set]),
			Updated_queues = dict:append(Key, New_set, dict:erase(Key, Queues)),
			Pid ! {ok,Updated_queues},
			queue_storage_loop(Updated_queues);

		{remove, {Key, Order}} -> 
			{_ok,[SubjectSet | _Meh]} = dict:find(Key, Queues),
			New_set = ordsets:del_element(Order,SubjectSet),
			io:fwrite("Removing: ~w ~n ", [New_set]),
			Updated_queues = dict:append(Key, New_set, dict:erase(Key, Queues)),
			queue_storage_loop(Updated_queues);

		{is_in_queue, {Pid, Key, Order}} ->
			{_ok,[SubjectSet | _Meh]} = dict:find(Key, Queues),
			Pid ! {ok, ordsets:is_element(Order, SubjectSet)},
			queue_storage_loop(Queues);

		{update, New_member} ->
			case dict:find(atom_to_list(New_member)++"_inner", Queues) of
				{ok, _} ->
					ok;
				error ->
					In_name = atom_to_list(New_member) ++ "_inner",
					Out_name = atom_to_list(New_member) ++ "_outer",

					Temp_dict = dict:append(In_name, ordsets:new(), Queues),
					UpdatedQueues = dict:append(Out_name, ordsets:new(), Temp_dict),
					queue_storage_loop(UpdatedQueues)
			end;

		{get_queue, {Pid, Key}} ->
			{_ok,[SubjectSet|_Meh]} = dict:find(Key, Queues),
			Pid ! {ok,SubjectSet},
			queue_storage_loop(Queues)
	end.


add_to_queue(ElevatorID, Order) -> % Er en dum funksjon, som bare setter inn basert på input, order_distributer bestemmer hvor
	case Order#order.type == inner of 
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

is_order(Order) ->
	is_order(Order,get_member_list()).
	
is_order(Order, MemberList) ->
	case MemberList of
		[Member | Rest] ->
			case Order#order.type == inner of 
				true ->
					Key = atom_to_list(Member) ++ "_inner";
				false ->
					Key = atom_to_list(Member) ++ "_outer"
			end,

			?QUEUE_PID ! {is_in_queue, {self(), Key, Order}},
			receive 
				{ok, false} ->
					is_order(Order, Rest);
				{ok, true} ->
					true
				after 50 ->
					io:fwrite("isOrder recieved nothing ~n ", []),
					error
			end;

		[] ->
			false
	end.

is_floor_in_queue(ElevatorID, Floor) ->
	InKey = atom_to_list(ElevatorID) ++ "_inner",
	OutKey = atom_to_list(ElevatorID) ++ "_outer",
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

remove_from_queue(ElevatorID, Floor) ->
	InKey = atom_to_list(ElevatorID) ++ "_inner",
	OutKey = atom_to_list(ElevatorID) ++ "_outer",
	Order1  = #order{floor = Floor, type = down},
	Order2  = #order{floor = Floor, type = inner},
	Order3  = #order{floor = Floor, type = up},
	?QUEUE_PID ! {remove, {OutKey, Order1}},
	?QUEUE_PID ! {remove, {InKey, Order2}},
	?QUEUE_PID ! {remove, {OutKey, Order3}},
	ok.

get_first_in_queue(ElevatorID, inner) -> %TODO: MAKE LESS UGLY!
	?QUEUE_PID ! {get_queue, {self(),atom_to_list(ElevatorID) ++ "_inner"}},
	receive
		{ok,[]} ->
			First = empty;
		{ok,[First | _Rest]} ->
			ok
	end,
	First; 

get_first_in_queue(ElevatorID, outer) ->
	?QUEUE_PID ! {get_queue, {self(),atom_to_list(ElevatorID) ++ "_outer"}},
	receive
		{ok,[]} ->
			First = empty;
		{ok,[First | _Rest]} ->
			ok
	end,
	First.

 get_queue_set(ElevatorID, outer) ->
	?QUEUE_PID ! {get_queue, {self(), atom_to_list(ElevatorID) ++ "_outer"}},
	receive 
		{ok, Outer_set} ->
			ok
	end,
	Outer_set.


 get_outer_queue(ElevatorID) ->
	?QUEUE_PID ! {get_queue, {self(), atom_to_list(ElevatorID) ++ "_outer"}},
	receive 
		{ok, GivenSet} ->
			ok
	end,
	GivenSet.