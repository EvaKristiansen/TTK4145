

-module (queue).
-export( [init/0, add_to_queue/3, addToInnerQueue/2, mergeFromQueue/2, getInnerQueue/2, getNextOrder/1, deleteFromQueue/2, isOrder/2]).

% Order in a tuple of the form {floor, direction}
- record(order,{floor,direction}).

- define(IN_KEY = 13).
- define(OUT_KEY = 42).
- define(QUEUE_PID = queue).
- define(PROCESS_GROUP_NAME = nodes).
- define(INNER = "_inner").
- dofine(OUTER = "_outer").



init() ->
	Memberlist = get_members(),
	Queues = init_storage(Queues, Memberlist),
	register(?QUEUE_PID, spawn(?MODULE, queue_storage_loop, Queues).


init_from_stop -> %?
	% Modulen skal spørre andre tilkoplede heiser om deres ytre og indre kø.
	% Bruker "insert" egenskapen i queue_storage som skal overskrive eventuelle data som er på "dict -> key"


%% Process Group Implementation, taken from Kjetil Kjeka.
join_process_group() ->
	pg2:create(?PROCESS_GROUP_NAME),
	pg2:join(?PROCESS_GROUP_NAME, self()).
get_member_list() ->
	pg2:get_members(?PROCESS_GROUP_NAME).




queue_storage_loop(Queues) ->
	receive
		{get_set, {Pid, {Key, _Floor, _Direction}}} ->
			Pid ! dict:find(Key),
			queue_storage_loop(Queues);

		{insert, {Pid, Key, Order}}
			SubjectSet = dict:find(Key, Queues),
			New_set = ordsets:add_element(Order, SubjectSet),
			Updated_queues = dict:append(New_set, dict:erase(Key, Queues)),
			queue_storage(Updated_queues);

		{add, }
		{is_in_queue, {Pid, Key, Order}} ->
			SubjectSet = dict:find(Key, Queues),
			Pid ! ordsets:is_element(SubjectSet, Order#order.floor, Order#order.direction);

		{remove, {Key, Order}} -> 
			SubjectSet = dict:find(Key, Queues),
			New_set = ordsets:subtract(SubjectSet, Order#order.floor, Order#order.direction),
			Updated_queues = dict:append(New_set, dict:erase(Key, Queues)),
			queue_storage_loop(Updated_queues)
	end 



add_to_queue(ElevatorID, Order) -> % Er en dum funksjon, som bare setter inn basert på input, order_distributer bestemmer hvor
	case Order#order.direction == 0 of 
		true ->
			Key = atom_to_list(ElevatorID) + INNER,
		false ->
			Key = atom_to_list(ElevatorID) + OUTER,
	end.
	?QUEUE_PID ! {insert, {self(), Key, Order}}
	receive
		{ok,Queue} ->
			ok;
		{_,_} ->
			io:fwrite("Order was not added ~n ", []),
			error;
	end

mergeFromQueue(ElevatorID) -> % er dum, merger fra elevatorID inn i andre køer, sletter køen til elevatorID?

getInnerQueue(ElevatorID) ->
	MyInKey = atom_to_list(ElevatorID) + "_inner",
	?QUEUE_PID ! {get_set, {self(), {MyInKey, 0, 0}}} % MY_PID er tenkt å brukes så queue_storage kan sende et svar
	receive
		{ok,Queue} ->
			Queue;
		{_,_} ->
			io:fwrite("No queue for ~w ~n ", [ElevatorID]),
			error;
	end

getNextOrder(ElevatorID) -> %Må denne være glup? I så fall, flytt til order_distributer

deleteFromQueue(ElevatorID, Floor) ->
	% Kjør en loop som itererer over retningene (-1, 0, 1)
	MyInKey = atom_to_list(ElevatorID) + "_inner",
	MyOutKey = atom_to_list(ElevatorID) + "_outer",
	Order1  = #order{floor = Floor, direction = -1},
	Order2  = #order{floor = Floor, direction = 0},
	Order3  = #order{floor = Floor, direction = 1},
	?QUEUE_PID ! {remove, {MyOutKey, Order1}},
	?QUEUE_PID ! {remove, {MyInKey, Order2}},
	?QUEUE_PID ! {remove, {MyOutKey, Order3}};

isOrder(Order) ->
	MemberList = get_member_list(),
	isOrder(Order, MemberList).

isOrder(Order, MemberList) ->
	case MemberList of
		[Member | Rest] ->
			case Order#order.direction == 0 of 
				true ->
					Key = atom_to_list(Member) + INNER,
				false ->
					Key = atom_to_list(Member) + OUTER,
			end.
			?QUEUE_PID ! {is_in_queue, {self(), Key, Order}};
			receive 
				{ok, false} ->
					isOrder(Order, Rest),
				{ok, true} ->
					true;
		[] ->
			false
	end.

init_storage(Queues,MemberList)->
	case MemberList of 
		[Member | Rest] ->	
			In_name = atom_to_list(member) + "_inner",
			Out_name = atom_to_list(member) + "_outer",

			Temp_dict = dict:append(In_name, ordsets:new(), Queues),
			New_Queues = dict:append(Out_name, ordsets:new(), Temp_dict),
			init_storage(New_Queues,Rest);
		[] ->
			Queues;
	end.
