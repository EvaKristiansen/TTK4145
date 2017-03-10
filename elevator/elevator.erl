- module(elevator).
- export([start/0]).
- define(ELEVATOR_MONITOR_PID, empid).
- define(REMOTE_LISTENER_PID, rlpid).
- define(STATE_STORAGE_PID, ss).
- define(DRIVER_MANAGER_PID, dmpid).
- record(order,{floor,type}).
- compile(export_all).

start() ->
	connection:init(),
	
	timer:sleep(60),

	queue_module:init(),
	state_storage:init(),

	register(?ELEVATOR_MONITOR_PID, spawn(fun() -> elevator_monitor() end)), %Should be init-versions of functions
	driver:start(?ELEVATOR_MONITOR_PID),
	register(?REMOTE_LISTENER_PID, spawn(fun() -> remote_listener() end)),
	register(?DRIVER_MANAGER_PID, spawn(fun() -> driver_manager() end)),

	


%The function that handles button presses should do as little as possible.....
elevator_monitor() ->
	receive
		{new_floor_reached,Floor} ->

			driver:set_floor_indicator(Floor),
			?STATE_STORAGE_PID ! {set_last_known_floor, {node(), Floor}},

			Stop_for_order = queue_module:is_floor_in_queue(node(),Floor),

			case Stop_for_order of
				true ->
					%STOP, Remove from order, Send stopped to fsm, open_door
					?DRIVER_MANAGER_PID  ! {stop_at_floor,Floor},
					queue_module:remove_from_queue(node(), Floor), %TODO Bør ikke denne funksjonen fjerne for alle heiser, ikkebare node?
					?STATE_STORAGE_PID ! {set_state, {node(), door_open}};
				
				false ->
					%Keep going
					ok
			end,

			%Just to stop in each end:
			case ((Floor == 0) or (Floor == 3)) and (not Stop_for_order) of
				true ->
					?DRIVER_MANAGER_PID  ! {at_end_floor},
					?STATE_STORAGE_PID ! {set_state, {node(), idle}},
					spawn(fun()-> order_poller() end);
				false ->
				%Keep going
					ok
			end,

			elevator_monitor();

		{button_pressed, Floor, ButtonType} ->
			driver:set_button_lamp(ButtonType,Floor,on),
			Order = #order{floor = Floor, type = ButtonType},
			Winner = order_distributer:distribute_order(Order),
			add_to_queue_on_nodes(Winner,Order),
			elevator_monitor();

		{door_closed} ->
			?STATE_STORAGE_PID ! {set_state, {node(), idle}},
			spawn(fun()-> order_poller() end),
			elevator_monitor();

		{new_destination, Direction} ->
			?DRIVER_MANAGER_PID  ! {go_to_order, Direction},
			?STATE_STORAGE_PID ! {set_state, {node(), moving}},
			elevator_monitor()
	end.

add_to_queue_on_nodes(Elevator, Order) ->
	lists:foreach(fun(Node) -> {?REMOTE_LISTENER_PID, Node} ! {add_order, Elevator, Order} end, nodes()).

remote_listener() ->
	receive
		{add_order, Elevator, Order} ->
			driver:set_button_lamp(Order#order.type,Order#order.floor,on),
			queue_module:add_to_queue(Elevator, Order);

		{update_states, Elevator, State, Direction, Last_floor} ->
			ok %TODO

	end.

driver_manager() ->
	receive
		{stop_at_floor,Floor} ->
			driver:set_motor_direction(stop),
			driver:reset_button_lights(Floor),

			driver:set_door_open_lamp(on),
			timer:sleep(3000),
			driver:set_door_open_lamp(off),
			?ELEVATOR_MONITOR_PID ! {door_closed},
			driver_manager();

		{at_end_floor} ->
			driver:set_motor_direction(stop),
			?STATE_STORAGE_PID ! {set_direction, {node(), stop}},
			driver_manager();

		{go_to_order, Direction} ->
			driver:set_motor_direction(Direction)
	end.

order_poller() -> %Checks if my elevator has place it should be, when in state idle
	Next = order_distributer:get_next_order(node()),
	case next of
		false ->
			timer:sleep(100), %TODO check poll period 
			order_poller();

		Order_floor ->

			Elevator_floor = state_storage:get_last_floor(node()), %Assume it has been updated
			Relative_position = Order_floor - Elevator_floor, %TODO reply
			?ELEVATOR_MONITOR_PID ! {new_destination, direction(Relative_position)}
	end.



%%%%%%%%%%%%%%% HELPER FUNCTIONS; MAY BE REMOVABLE %%%%%%%%%%%%%%%%%%%%%%%%%%%
direction(0) -> stop;
direction(Relative_position) when Relative_position > 0 -> up;
direction(Relative_position) when Relative_position < 0 -> down.