- module(elevator).
- export([start/0]).
- define(ELEVATOR_MONITOR_PID, empid).
- define(REMOTE_LISTENER_PID, rlpid).
- define(STATE_STORAGE_PID, ss).
- define(DRIVER_MANAGER_PID, dmpid).
- record(order,{floor,type}).
 -record(button,{floor,type,state = 0}).
- compile(export_all).

start() ->
	connection:init(),
	
	timer:sleep(60),

	queue_module:init(),
	register(?ELEVATOR_MONITOR_PID, spawn(fun() -> elevator_monitor() end)), %Should be init-versions of functions
	
	Floor = driver:start(?ELEVATOR_MONITOR_PID),
	io:fwrite("Starting ... in floor: ~w ~n",[Floor]), %DEBUG
	state_storage:init(Floor),

	

	register(?REMOTE_LISTENER_PID, spawn(fun() -> remote_listener() end)),
	register(?DRIVER_MANAGER_PID, spawn(fun() -> driver_manager() end)),
	spawn(fun() -> button_light_manager(driver:create_buttons([],0)) end),
	spawn(fun() -> node_watcher({0,0,0}) end), 
	?STATE_STORAGE_PID ! {set_state, {node(), idle}},
	send_remote_state_update(idle), 
	spawn(fun()-> order_poller() end).

%The function that handles button presses should do as little as possible.....
elevator_monitor() ->
	receive
		{new_floor_reached,Floor} ->

			driver:set_floor_indicator(Floor),
			?STATE_STORAGE_PID ! {set_last_known_floor, {node(), Floor}},
			send_remote_floor_update(Floor), 
			respond_to_new_floor(Floor == queue_module:get_my_next(), Floor),% argument (Stop_for_order, Floor)
			elevator_monitor();

		{button_pressed, Floor, ButtonType} ->
			Order = #order{floor = Floor, type = ButtonType},
			Winner = order_distributer:distribute_order(Order),
			add_to_queue_on_nodes(Winner,Order),
			elevator_monitor();

			

		{door_closed} ->
			?STATE_STORAGE_PID ! {set_state, {node(), idle}},
			send_remote_state_update(idle),
			spawn(fun()-> order_poller() end),
			elevator_monitor();

		{new_destination, stop} ->
			Floor = state_storage:get_last_floor(node()),
			?DRIVER_MANAGER_PID  ! {stop_at_floor,Floor},
			lists:foreach(fun(Node) -> queue_module:remove_from_queue(Node == node(), Node, Floor) end, [node()]++nodes() ), 

			send_remote_queue_removal(Floor),
			?STATE_STORAGE_PID ! {set_state, {node(), door_open}},
			send_remote_state_update(door_open), 
			elevator_monitor();

		{new_destination, Direction} ->
			?DRIVER_MANAGER_PID  ! {go_to_order, Direction},
			?STATE_STORAGE_PID ! {set_state, {node(), moving}},
			send_remote_state_update(moving), 
			elevator_monitor()
	end.

add_to_queue_on_nodes(Elevator, Order) ->
	lists:foreach(fun(Node) -> {?REMOTE_LISTENER_PID, Node} ! {add_order, Elevator, Order} end, nodes()).

send_remote_state_update(State) ->
	lists:foreach(fun(Node) -> {?REMOTE_LISTENER_PID, Node} ! {update_state, node(), State} end, nodes()).

send_remote_direction_update(Direction) ->
	lists:foreach(fun(Node) -> {?REMOTE_LISTENER_PID, Node} ! {update_direction, node(), Direction} end, nodes()).

send_remote_floor_update(Floor) ->
	lists:foreach(fun(Node) -> {?REMOTE_LISTENER_PID, Node} ! {update_floor, node(), Floor} end, nodes()).

send_remote_queue_removal(Floor) ->
	lists:foreach(fun(Node) -> {?REMOTE_LISTENER_PID, Node} ! {remove_from_queue, {node(), Floor}} end, nodes()).


remote_listener() -> % TODO
	receive
		{add_order, Elevator, Order} ->
			queue_module:add_to_queue(Elevator, Order),
			remote_listener();

		{update_state, Elevator, State} ->
			state_storage:update_state(Elevator, State),
			remote_listener();

		{update_floor, Elevator, Floor} ->
			state_storage:update_floor(Elevator, Floor),
			remote_listener();

		{update_direction, Elevator, Direction} ->
			state_storage:update_direction(Elevator, Direction),
			remote_listener();

		{remove_from_queue, {Elevator, Floor}} ->
			lists:foreach(fun(Node) -> queue_module:remove_from_queue(Node == Elevator, Node, Floor) end, [node()]++nodes() ),
			remote_listener();

		{merge_to_inner_queue, Remote_queue} ->	%Remote_queue is ordset
			Original_queue = queue_module:get_queue_set(node(),inner),
			New_queue = ordsets:union(Original_queue,Remote_queue),
			queue_module:replace_queue(node(),New_queue)
	end.

driver_manager() ->
	receive
		{stop_at_floor,Floor} ->
			driver:set_motor_direction(stop),
			?STATE_STORAGE_PID ! {set_direction, {node(), stop}},
			send_remote_direction_update(stop), 

			driver:reset_button_lights(Floor),
			driver:set_door_open_lamp(on),
			timer:sleep(3000),

			driver:set_door_open_lamp(off),
			?ELEVATOR_MONITOR_PID ! {door_closed},
			driver_manager();

		{at_end_floor} ->
			driver:set_motor_direction(stop),
			?STATE_STORAGE_PID ! {set_direction, {node(), stop}},
			send_remote_direction_update(stop),
			driver_manager();

		{go_to_order, Direction} ->
			driver:set_motor_direction(Direction),
			?STATE_STORAGE_PID ! {set_direction, {node(), Direction}},
			send_remote_direction_update(Direction),
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

	%%%%%%%%%%%%%%%%%%%%%%%%% CURRENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	lists:foreach(fun(Node) -> queue_module:remove_from_queue(Node == node(), Node, Floor) end, [node()]++nodes() ), 
	send_remote_queue_removal(Floor),

	?STATE_STORAGE_PID ! {set_state, {node(), door_open}},
	send_remote_state_update(door_open);

respond_to_new_floor(false, 0) -> 
	respond_to_new_floor(false, 3);

respond_to_new_floor(false, 3) -> 
	?DRIVER_MANAGER_PID  ! {at_end_floor},
	?STATE_STORAGE_PID ! {set_state, {node(), idle}},
	send_remote_state_update(idle), 
	spawn(fun()-> order_poller() end);
respond_to_new_floor(false, _) ->
	ok.


button_light_manager(Buttons) ->
	lists:foreach(fun(Button) -> update_button_light(Button) end,Buttons),
	timer:sleep(50),
	button_light_manager(Buttons).

update_button_light(Button)-> %Not happy with name, but tired, renamed from set_button
	Toset = queue_module:is_order(button_to_order(Button)),
	set_button (Toset, Button). % Todo

set_button(true, Button) -> driver:set_button_lamp(Button#button.type,Button#button.floor,on);
set_button(false, Button) -> driver:set_button_lamp(Button#button.type,Button#button.floor,off).

node_watcher({0,0,0}) ->
	global_group:monitor_nodes(true),
	node_watcher({0,0,1});

node_watcher(Timestamp) ->
	receive 
		{nodedown, Node} ->
			order_distributer:merge_from_elevator(Node),
			node_watcher(Timestamp);
		{nodeup, Node} ->
			io:fwrite("NODE UP : ~w ~n", [Node]),
			queue_module:update_queue(Node),
			state_storage:update_storage(Node),
			Node_queue = queue_module:get_queue_set(Node,inner),
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