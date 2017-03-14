- module(elevator).
- export([start/0]).
- define(ELEVATOR_MONITOR_PID, empid).
- define(REMOTE_LISTENER_PID, rlpid).
- define(STATE_STORAGE_PID, ss).
- define(DRIVER_MANAGER_PID, dmpid).
- define(NODE_WATCHER_PID, nwpid).
- record(order,{floor,type}).
 -record(button,{floor,type,state = 0}).
- compile(export_all).

start() ->
	register(?ELEVATOR_MONITOR_PID, spawn(fun() -> elevator_monitor_init() end)), %Should be init-versions of functions
	register(?REMOTE_LISTENER_PID, spawn(fun() -> remote_listener_init() end)),
	register(?DRIVER_MANAGER_PID, spawn(fun() -> driver_manager_init() end)),

	connection:init(self()),
	receive 
		connection_init_complete ->
			connection_init_ok
	end,

	driver:start(self(), ?ELEVATOR_MONITOR_PID),
	receive
		{driver_init_complete, Floor} ->
			driver_init_ok
	end,

	io:fwrite("Starting ... in floor: ~w ~n",[Floor]), %DEBUG

	queue_module:init(self()),
	receive
		queue_init_complete ->
			queue_init_ok
	end,

	state_storage:init(self(), Floor),
	receive
		state_init_complete ->
			state_init_ok
	end,


	spawn(fun() -> button_light_manager(driver:create_buttons([],0)) end),
	register(?NODE_WATCHER_PID ,spawn(fun() -> node_watcher({0,0,0}) end)), 

	?ELEVATOR_MONITOR_PID ! init_complete,
	?REMOTE_LISTENER_PID ! init_complete,
	?DRIVER_MANAGER_PID ! init_complete,

	lists:foreach(fun(Node) -> {?NODE_WATCHER_PID, Node} ! init_complete end, nodes()).

	state_storage:update_state(node(), idle),
	send_to_connected_nodes(update_state, {node(), idle}),
	spawn(fun()-> order_poller() end).

%The function that handles button presses should do as little as possible.....
elevator_monitor_init() ->
	receive
		init_complete ->
			elevator_monitor()
	end.

elevator_monitor() ->
	receive
		{new_floor_reached,Floor} ->
			driver:set_floor_indicator(Floor),
			?STATE_STORAGE_PID ! {set_last_known_floor, {node(), Floor}},
			send_to_connected_nodes(update_floor, {node(), Floor}),
			respond_to_new_floor(Floor == queue_module:get_my_next(), Floor),% argument (Stop_for_order, Floor)
			elevator_monitor();

		{button_pressed, Floor, ButtonType} ->
			Order = #order{floor = Floor, type = ButtonType},
			Winner = order_distributer:distribute_order(Order),
			send_to_connected_nodes(add_order, {Winner, Order}),
			elevator_monitor();

			

		{door_closed} ->
			?STATE_STORAGE_PID ! {set_state, {node(), idle}},
			send_to_connected_nodes(update_state, {node(), idle}),
			spawn(fun()-> order_poller() end),
			elevator_monitor();

		{new_destination, stop} ->
			Floor = state_storage:get_last_floor(node()),
			?DRIVER_MANAGER_PID  ! {stop_at_floor,Floor},
			lists:foreach(fun(Node) -> queue_module:remove_from_queue(Node == node(), Node, Floor) end, [node()]++nodes() ), 

			send_to_connected_nodes(remove_from_queue, {node(), Floor}),
			?STATE_STORAGE_PID ! {set_state, {node(), door_open}},
			send_to_connected_nodes(update_state, {node(), door_open}),
			elevator_monitor();

		{new_destination, Direction} ->
			?DRIVER_MANAGER_PID  ! {go_to_order, Direction},
			?STATE_STORAGE_PID ! {set_state, {node(), moving}},
			send_to_connected_nodes(update_state, {node(), moving}),
			elevator_monitor()
	end.


send_to_connected_nodes(Command, Message) ->
	lists:foreach(fun(Node) -> {?REMOTE_LISTENER_PID, Node} ! {Command, Message} end, nodes()).


remote_listener_init() ->
	receive 
		init_complete ->

			remote_listener()
	end.


remote_listener() -> % TODO
	io:fwrite("Starting remote remote_listener ~n ", []),
	receive
		{add_order, {Elevator, Order}} ->
			queue_module:add_to_queue(Elevator, Order),
			remote_listener();

		{update_state, {Elevator, State}} ->
			state_storage:update_state(Elevator, State),
			remote_listener();

		{update_floor, Elevator, Floor} ->
			state_storage:update_floor(Elevator, Floor),
			remote_listener();

		{update_direction, {Elevator, Direction}} ->
			state_storage:update_direction(Elevator, Direction),
			remote_listener();

		{remove_from_queue, {Elevator, Floor}} ->
			lists:foreach(fun(Node) -> queue_module:remove_from_queue(Node == Elevator, Node, Floor) end, [node()]++nodes() ),
			remote_listener();

		{merge_to_inner_queue, Remote_queue} ->	%Remote_queue is ordset
			io:fwrite("Got merge to inner queue request ~n ", []),
			Original_queue = queue_module:get_queue_set(node(),inner),
			io:fwrite("My queue before merge message was: ~w ~n ", [Original_queue]),
			io:fwrite("The queue I received was: ~w ~n ", [Remote_queue]),
			New_queue = ordsets:union(Original_queue,Remote_queue),
			io:fwrite("My new queue is : ~w ~n ", [New_queue]),
			queue_module:replace_queue(node(),New_queue),
			remote_listener()

			%UFERDIG: TIL CONSISTENCY CHECKS%
		%{outer_queue_request, Sender} ->
			%Outer_queue_list = 
	end.

driver_manager_init() ->
	receive
		init_complete ->
			driver_manager()
	end.

driver_manager() ->
	receive
		{stop_at_floor,Floor} ->
			driver:set_motor_direction(stop),
			?STATE_STORAGE_PID ! {set_direction, {node(), stop}},
			send_to_connected_nodes(update_direction, {node(), stop}),

			driver:reset_button_lights(Floor),
			driver:set_door_open_lamp(on),
			timer:sleep(3000),

			driver:set_door_open_lamp(off),
			?ELEVATOR_MONITOR_PID ! {door_closed},
			driver_manager();

		{at_end_floor} ->
			driver:set_motor_direction(stop),
			?STATE_STORAGE_PID ! {set_direction, {node(), stop}},
			send_to_connected_nodes(update_direction, {node(), stop}),
			driver_manager();

		{go_to_order, Direction} ->
			driver:set_motor_direction(Direction),
			?STATE_STORAGE_PID ! {set_direction, {node(), Direction}},
			send_to_connected_nodes(update_direction, {node(), Direction}),
			driver_manager()
	end.


order_poller() ->
	order_distributer:update_my_next(),
	order_poller(queue_module:get_my_next()).

order_poller(none) -> % No orders to take, wait and check again.
	timer:sleep(300), %TODO check poll period 
	order_poller();

order_poller(Next_floor) -> 				%Checks if my elevator has place it should be, when in state idle
	Elevator_floor = state_storage:get_last_floor(node()), %Assume it has been updated
	Relative_position = Next_floor - Elevator_floor,
	%io:fwrite("Relative position is: ~w ~n",[Relative_position]), %DEBUG
	?ELEVATOR_MONITOR_PID ! {new_destination, direction(Relative_position)}.

respond_to_new_floor(true, Floor) ->% argument (Stop_for_order, Floor)
	%STOP, Remove from order, Send stopped to fsm, open_door
	?DRIVER_MANAGER_PID ! {stop_at_floor,Floor},

	lists:foreach(fun(Node) -> queue_module:remove_from_queue(Node == node(), Node, Floor) end, [node()]++nodes() ), 
	send_to_connected_nodes(remove_from_queue, {node(), Floor}),

	?STATE_STORAGE_PID ! {set_state, {node(), door_open}},
	send_to_connected_nodes(update_state, {node(), door_open});

respond_to_new_floor(false, 0) -> 
	respond_to_new_floor(false, 3);

respond_to_new_floor(false, 3) -> 
	?DRIVER_MANAGER_PID  ! {at_end_floor},
	?STATE_STORAGE_PID ! {set_state, {node(), idle}},
	send_to_connected_nodes(update_state, {node(), idle}), 
	spawn(fun()-> order_poller() end);
respond_to_new_floor(false, _) ->
	ok.


button_light_manager(Buttons) ->
	lists:foreach(fun(Button) -> update_button_light(Button) end,Buttons),
	timer:sleep(50),
	button_light_manager(Buttons).

update_button_light(Button)->
	Toset = queue_module:is_order(button_to_order(Button)),
	set_button (Toset, Button). 

set_button(true, Button) -> driver:set_button_lamp(Button#button.type,Button#button.floor,on);
set_button(false, Button) -> driver:set_button_lamp(Button#button.type,Button#button.floor,off).

node_watcher({0,0,0}) ->
	global_group:monitor_nodes(true),
	node_watcher({0,0,1});

node_watcher(Timestamp) ->
	receive 
		{nodedown, Node} ->
			io:fwrite("NODE DOWN : ~w ~n", [Node]),
			order_distributer:merge_from_elevator(Node),
			node_watcher(Timestamp);
		{nodeup, Node} ->
			io:fwrite("NODE UP : ~w ~n", [Node]),
			queue_module:update_queue(Node),
			state_storage:update_storage(Node),
			Node_queue = queue_module:get_queue_set(Node,inner),
			io:fwrite("My representation of ~w queue at crash: ~w ~n ", [Node,Node_queue]),
			receive
				init_complete ->
					ok
			end,
			{?REMOTE_LISTENER_PID, Node} ! {merge_to_inner_queue, Node_queue}

	after 30000 ->
		%EVA: DO SOME CONSISTENSY CHECKS BETWEEN STORAGES HERE!
		ok
	end,
	node_watcher({0,0,1}).

%%%%%%%%%%%%%%% HELPER FUNCTIONS; MAY BE REMOVABLE %%%%%%%%%%%%%%%%%%%%%%%%%%%
direction(0) -> stop;
direction(Relative_position) when Relative_position > 0 -> up;
direction(Relative_position) when Relative_position < 0 -> down.

button_to_order(Button) ->
	#order{floor= Button#button.floor, type = Button#button.type}.