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
	?STATE_STORAGE_PID ! {set_state, {node(), idle}},
	send_remote_state_update(idle), 
	order_poller().


%The function that handles button presses should do as little as possible.....
elevator_monitor() ->
	receive
		{new_floor_reached,Floor} ->

			driver:set_floor_indicator(Floor),
			?STATE_STORAGE_PID ! {set_last_known_floor, {node(), Floor}},
			send_remote_floor_update(Floor), 
			Stop_for_order = queue_module:is_floor_in_queue(node(),Floor),

%			case Stop_for_order of
%				true ->
%					%STOP, Remove from order, Send stopped to fsm, open_door
%					?DRIVER_MANAGER_PID ! {stop_at_floor,Floor},
%					queue_module:remove_from_queue(node(), Floor), %TODO Bør ikke denne funksjonen fjerne for alle heiser, ikkebare node?
%					?STATE_STORAGE_PID ! {set_state, {node(), door_open}},
%					send_remote_state_update(door_open); 
%				false ->
%					%Keep goinggit 
%					ok
%			end,
%
%			%Just to stop in each end:
%			case ((Floor == 0) or (Floor == 3)) and (not Stop_for_order) of %TODO: REMOVE UGLY CASE ADD PATTERN MATCHING
%				true ->
%					?DRIVER_MANAGER_PID  ! {at_end_floor},
%					?STATE_STORAGE_PID ! {set_state, {node(), idle}},
%					send_remote_state_update(idle), 
%					spawn(fun()-> order_poller() end);
%				false ->
%				%Keep going
%					ok
%			end,
			respone_to_new_floor(Stop_for_order, Floor),% argument (Stop_for_order, Floor)
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
			io:fwrite("Got new destination at direction: ~w ~n",[stop]), %DEBUG
			Floor = state_storage:get_last_floor(node()),
			?DRIVER_MANAGER_PID  ! {stop_at_floor,Floor},
			queue_module:remove_from_queue(node(), Floor), %TODO Bør ikke denne funksjonen fjerne for alle heiser, ikkebare node?
			?STATE_STORAGE_PID ! {set_state, {node(), door_open}},
			send_remote_state_update(door_open), 
			elevator_monitor();

		{new_destination, Direction} ->
			io:fwrite("Got new destination at direction: ~w ~n",[Direction]), %DEBUG
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

order_poller() -> %Checks if my elevator has place it should be, when in state idle
	Next = order_distributer:get_next_order(node()),
	case Next of
		false ->
			timer:sleep(3000), %TODO check poll period 
			order_poller();

		Order ->
			Elevator_floor = state_storage:get_last_floor(node()), %Assume it has been updated
			Relative_position = Order#order.floor - Elevator_floor,
			io:fwrite("Relative position is: ~w ~n",[Relative_position]), %DEBUG
			?ELEVATOR_MONITOR_PID ! {new_destination, direction(Relative_position)}
	end.

respone_to_new_floor(true, Floor) ->% argument (Stop_for_order, Floor)
	%STOP, Remove from order, Send stopped to fsm, open_door
	?DRIVER_MANAGER_PID ! {stop_at_floor,Floor},
	queue_module:remove_from_queue(node(), Floor), %TODO Bør ikke denne funksjonen fjerne for alle heiser, ikkebare node?
	?STATE_STORAGE_PID ! {set_state, {node(), door_open}},
	send_remote_state_update(door_open);

respone_to_new_floor(false, 0) -> 
	respone_to_new_floor(false, 3);

respone_to_new_floor(false, 3) -> 
	?DRIVER_MANAGER_PID  ! {at_end_floor},
	?STATE_STORAGE_PID ! {set_state, {node(), idle}},
	send_remote_state_update(idle), 
	spawn(fun()-> order_poller() end).


button_light_manager(Buttons) ->
	lists:foreach(fun(Button) -> set_button(Button) end,Buttons),
	timer:sleep(50),
	button_light_manager(Buttons).

set_button(Button)-> %Not happy with name, but tired
	Toset = queue_module:is_order(button_to_order(Button)),
	case Toset of
		true ->
			driver:set_button_lamp(Button#button.type,Button#button.floor,on);
		false ->
			driver:set_button_lamp(Button#button.type,Button#button.floor,off)
	end.

%%%%%%%%%%%%%%% HELPER FUNCTIONS; MAY BE REMOVABLE %%%%%%%%%%%%%%%%%%%%%%%%%%%
direction(0) -> stop;
direction(Relative_position) when Relative_position > 0 -> up;
direction(Relative_position) when Relative_position < 0 -> down.

button_to_order(Button) ->
	#order{floor= Button#button.floor, type = Button#button.type}.