

-module (queue).
-export( [init/0, add_to_queue/3, addToInnerQueue/2, mergeFromQueue/2, getInnerQueue/2, getNextOrder/1, deleteFromQueue/2, isOrder/2]).

% Order in a tuple of the form {floor, direction}
- record(order,{floor,direction}).

- define(IN_KEY = 13).
- define(OUT_KEY = 42).
- define(QUEUE_PID = queue).
- define(ELEVATORS = elevators)

init() ->
	Memberlist = get_members(),
	Queues = init_storage(Queues, Memberlist),
	register(?QUEUE_PID, spawn(?MODULE, queue_storage_loop, Queues).


init_from_stop -> %?
	% Modulen skal spørre andre tilkoplede heiser om deres ytre og indre kø.
	% Bruker "insert" egenskapen i queue_storage som skal overskrive eventuelle data som er på "dict -> key"
	

queue_storage_loop(Queues) ->
	receive
		{get_set, {Pid, {Id, Key, _Floor, _Direction}}} ->
			Pid ! dict:find(Key, Id),
			queue_storage(MyQueue, Queue1, Queue2);

		{insert, {Pid, {Id, Key, _Floor, _Direction}}}
			%Add to dictionary? How?
			Pid ! dict:find(Key,Id) %Hvordan fungerer denne?
			queue_storage(MyQueue, Queue1, Queue2);

		{add, }

		{remove, {Id, Key, Floor, Direction}} -> % UFERDIG
			SubjectSet = dict:find(Key, Id),
			New_set = ordsets:subtract(SubjectSet, {Floor, Direction}),
			New_dict = dict:append(dict:erase(Key, Id)
			queue_storage(MyQueue, ,

	end 



add_to_queue(ElevatorID, Key,OrderFloor,OrderDir) -> % Er en dum funksjon, som bare setter inn basert på input, order_distributer bestemmer hvor
	?QUEUE_PID ! {insert,{self(),ElevatorID,Key,OrderFloor,OrderDir}}
	receive
		{ok,Queue} ->
			ok;
		{_,_} ->
			io:fwrite("Order was not added ~n ", []),
			error;
	end

mergeFromQueue(ElevatorID) -> % er dum, merger fra elevatorID inn i andre køer, sletter køen til elevatorID?

getInnerQueue(ElevatorID) ->
	?QUEUE_PID ! {get_set, {self(), {ElevatorID, IN_KEY, 0, 0}}} % MY_PID er tenkt å brukes så queue_storage kan sende et svar
	receive
		{ok,Queue} ->
			Queue;
		{_,_} ->
			io:fwrite("No queue for ~w ~n ", [ElevatorID]),
			error;
	end

getNextOrder(ElevatorID) -> %Må denne være glup? I så fall, flytt til order_distributer

deleteFromQueue(ElevatorID, CurrentFloor) ->
	% Kjør en loop som itererer over retningene (-1, 0, 1)
	?QUEUE_PID ! {remove, {ID, IN_KEY, CurrentFloor, Direction}}
	?QUEUE_PID ! {remove, {ID, OUT_KEY, CurrentFloor, Direction}}

isOrder(Floor, ButtonType) ->

init_storage(Queues,Memberlist)->
	case Memberlist of 
		[Member | Rest] ->	
			In_name = atom_to_list(member) + "_inner",
			Out_name = atom_to_list(member) + "_outer",

			Temp_dict = dict:append(In_name, ordsets:new(), Queues),
			New_Queues = dict:append(Out_name, ordsets:new(), Temp_dict),
			init_storage(New_Queues,Rest);
		[] ->
			Queues;
	end.
