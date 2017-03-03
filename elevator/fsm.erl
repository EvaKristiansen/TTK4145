
- module (fsm).
- export(init/0,get_state/1,).
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
			New_states = dict:append(Out_name, init , Temp_dict),  %Assuming we will not initialize state machine unless 
			init_storage(New_states,Rest);
		[] ->
			States
	end.


queue_storage_loop(States) ->

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

