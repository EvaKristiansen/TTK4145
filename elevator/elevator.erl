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
	spawn(fun() -> storage_maintainer({0,0,0}) end), %EVA
	?STATE_STORAGE_PID ! {set_state, {node(), idle}},
	send_remote_state_update(idle), 
	spawn(fun()-> order_poller() end). %TODO shouldn't it be spawned? Is now spawned, not tested
	% TODO, was order_poller() before the new function


%The function that handles button presses should do as little as possible.....
elevator_monitor() ->
	receive
		{new_floor_reached,Floor} ->

			driver:set_floor_indicator(Floor),
			?STATE_STORAGE_PID ! {set_last_known_floor, {node(), Floor}},
			send_remote_floor_update(Floor), 
			Stop_for_order = queue_module:is_floor_in_queue(node(),Floor),
			respond_to_new_floor(Stop_for_order, Floor),% argument (Stop_for_order, Floor)
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
			queue_module:remove_from_queue(node(), Floor), %TODO Bør ikke denne funksjonen fjerne for alle heiser, ikkebare node?
			?STATE_STORAGE_PID ! {set_state, {node(), door_open}},
			send_remote_state_update(door_open), 
			elevator_monitor();

		{new_destination, Direction} ->
			?DRIVER_MANAGER_PID  ! {go_to_order, Direction},
			?STATE_STORAGE_PID ! {set_state, {node(), moving}},
			send_remote_state_update(moving), 
			elevator_monitor();

		{elevator_dropout, ElevatorID} ->
			lists:foreach(fun(Order) -> 
				order_distributer:distribute_order(Order) % Funksjonen returnerer Winner, men vi distribuerer den ikke
			end,
			queue_module:get_outer_queue(ElevatorID))
	end.

add_to_queue_on_nodes(Elevator, Order) ->
	lists:foreach(fun(Node) -> {?REMOTE_LISTENER_PID, Node} ! {add_order, Elevator, Order} end, nodes()).
send_remote_state_update(State) ->
	lists:foreach(fun(Node) -> {?REMOTE_LISTENER_PID, Node} ! {update_state, node(), State} end, nodes()).
send_remote_direction_update(Direction) ->
	lists:foreach(fun(Node) -> {?REMOTE_LISTENER_PID, Node} ! {update_direction, node(), Direction} end, nodes()).
send_remote_floor_update(Floor) ->
	lists:foreach(fun(Node) -> {?REMOTE_LISTENER_PID, Node} ! {update_floor, node(), Floor} end, nodes()).

remote_listener() -> % TODO
	receive
		{add_order, Elevator, Order} ->
			queue_module:add_to_queue(Elevator, Order);


		{update_state, Elevator, State} ->
			state_storage:update_state(Elevator, State);

		{update_floor, Elevator, Floor} ->
			state_storage:update_floor(Elevator, Floor);

		{update_direction, Elevator, Direction} ->
			state_storage:update_direction(Elevator, Direction)
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
	queue_module:remove_from_queue(node(), Floor), %TODO Bør ikke denne funksjonen fjerne for alle heiser, ikkebare node?
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



storage_maintainer({0,0,0}) ->
	global_group:monitor_nodes(true),
	storage_maintainer({0,0,1});

storage_maintainer(Timestamp) ->
	receive 
		{nodedown, _Node} ->
			storage_maintainer(Timestamp);
		{nodeup, Node} ->
			queue_module:update_queue(Node),
			state_storage:update_storage(Node)

	after 30000 ->
		%EVA: DO SOME CONSISTENSY CHECKS BETWEEN STORAGES HERE!
		ok
	end,

	?MODULE:storage_maintainer({0,0,1}).


%%%%%%%%%%%%%%% HELPER FUNCTIONS; MAY BE REMOVABLE %%%%%%%%%%%%%%%%%%%%%%%%%%%
direction(0) -> stop;
direction(Relative_position) when Relative_position > 0 -> up;
direction(Relative_position) when Relative_position < 0 -> down.

button_to_order(Button) ->
	#order{floor= Button#button.floor, type = Button#button.type}.

