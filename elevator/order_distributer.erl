- module (order_distributer).
- export([distribute_order/1, merge_from_elevator/1, update_my_next/0, order_poller/1]).

-record(order,{floor,type}).

-define(NUM_FLOORS, 4).
-define(NUM_BUTTONS, 3).

order_poller(MonitorPID) ->
	update_my_next(),
	order_poller(queue_storage:get_my_next(), none, MonitorPID).

order_poller(New_order, Last_order, MonitorPID) ->
	react_to_new_poll(New_order == Last_order, New_order, MonitorPID),

	Newest_order = wait_and_get_next(300),
	order_poller(Newest_order, New_order, MonitorPID).

react_to_new_poll(_, none, _Pid) ->
	ok;
react_to_new_poll(true, _Floor_order, _Pid) ->
	ok;
react_to_new_poll(false, Floor_order, MonitorPID) ->
	Elevator_floor = state_storage:get_information(get_last_known_floor,node()), 
	Relative_position = Floor_order - Elevator_floor,
	MonitorPID ! {new_destination, direction(Relative_position)}.

wait_and_get_next(Time) ->
	timer:sleep(Time),
	order_distributer:update_my_next(),
	queue_storage:get_my_next().

update_my_next() ->
	Elevator_floor = state_storage:get_information(get_last_known_floor,node()),
	Elevator_direction = state_storage:get_information(get_direction,node()),
	Elevator_direction_int = direction_to_int(Elevator_direction),
	Outer_list = ordsets:to_list(queue_storage:get_queue_set(node(), outer)),
	Inner_list = ordsets:to_list(queue_storage:get_queue_set(node(), inner)),
	Order_list = Inner_list ++ Outer_list,
	update_my_next(Order_list, {10000, #order{floor = none, type = none}}, Elevator_floor, Elevator_direction_int).

update_my_next([],{_Best_penalty, Best_order}, _Elevator_floor, _Elevator_direction_int) -> 
	queue_storage:set_my_next(Best_order#order.floor);
update_my_next(Order_list, {Best_penalty, _Best_order}, Elevator_floor, Elevator_direction_int) ->
	[Order | Rest] = Order_list,

	Relative_position = Order#order.floor - Elevator_floor,										% Positive if pling is over elevator, else negative
	Moving_towards_pling = compare(sign(Relative_position), sign(Elevator_direction_int)),		% True if elevator moves towards pling
	Equal_direction = compare(order_type_to_int(Order#order.type), Elevator_direction_int), 	% True if elevator and signal same direction
	Distance = abs(Relative_position),

	Order_type_int = order_type_to_int(Order#order.type), 
	Penalty = position_penalty(Moving_towards_pling,Equal_direction,Distance, Order_type_int), 
	case (Penalty < Best_penalty) of 
		true ->
			update_my_next(Rest, {Penalty,Order}, Elevator_floor, Elevator_direction_int);
		false ->
			update_my_next(Rest, {Best_penalty, _Best_order}, Elevator_floor, Elevator_direction_int)
	end.

distribute_order({order, Floor, inner}) ->
	Order = #order{floor=Floor,type = inner},
	queue_storage:add_to_queue(node(),Order),
	node();
distribute_order(Order) -> 
	Memberlist = [node()|nodes()],
	Penalties = get_penalties(Memberlist,[],Order),
	Winner = choose_winner(Memberlist, Penalties, {10000, dummy@member}),
	queue_storage:add_to_queue(Winner, Order),
	Winner.


choose_winner([], _Penalties, {_Lowest_value, Member}) -> Member;
choose_winner(MemberList, Penalties, Best_so_far) ->
	[Member | Rest] = MemberList,
	choose_winner(Member, Rest, Penalties, Best_so_far).

choose_winner(Member_head, Member_rest, Penalties, {Lowest_value, Member}) ->
	[Penalty | Penalties_rest] = Penalties,
	case Penalty < Lowest_value of 
		true ->
			choose_winner(Member_rest, Penalties_rest, {Penalty, Member_head});
		false ->
			choose_winner(Member_rest, Penalties_rest, {Lowest_value, Member})
	end.

get_penalties([], Penalties, _Order) -> Penalties;
get_penalties(MemberList, Penalties, Order) ->
	[Member | Rest] = MemberList,
	get_penalty(Member, Rest, Penalties, Order).

get_penalty(Member, Rest, Penalties, Order) ->
	State = state_storage:get_information(get_state, Member),
	Elevator_floor = state_storage:get_information(get_last_known_floor,Member),
	Elevator_direction = state_storage:get_information(get_direction,Member),
	Elevator_direction_int = direction_to_int(Elevator_direction),
	Order_type_int = order_type_to_int(Order#order.type),

	Relative_position = Order#order.floor - Elevator_floor,									% Positive if pling is over elevator, else negative
	Moving_towards_pling = compare(sign(Relative_position), Elevator_direction_int),	% True if elevator moves towards pling
	Equal_direction = compare(Order_type_int, Elevator_direction_int), 						% True if elevator and signal same direction
	Distance = abs(Relative_position),	
	
	Penalty = state_penalty(State) + position_penalty(Moving_towards_pling,Equal_direction,Distance, Order_type_int),
	get_penalties(Rest,Penalties++[Penalty],Order).

state_penalty(init) -> 1000;
state_penalty(unknown) -> 1000;
state_penalty(idle) -> 7;
state_penalty(moving) -> 10;
state_penalty(door_open) -> 10;
state_penalty(stuck) -> 1000.

position_penalty(true, _Relative_position, Distance, 0) ->
	Distance;
position_penalty(false, _Relative_position, Distance, 0) ->
	Distance + 10;
position_penalty(true, false , Distance, _Type_int) ->
	(?NUM_FLOORS - Distance) + 10; 
position_penalty(false, true, Distance, _Type_int) ->
	(?NUM_FLOORS - Distance) + 2*10; 
position_penalty(false, false , Distance, _Type_int) ->
	(?NUM_FLOORS + Distance) + 10; 
position_penalty(true, true, Distance, _Type_int) ->
	Distance.

merge_from_elevator(ElevatorID)->
	Queue = ordsets:to_list(queue_storage:get_queue_set(ElevatorID,outer)),
	lists:foreach(fun(Order) -> distribute_order(Order) end, Queue).

order_type_to_int(down) -> -1; 
order_type_to_int(inner) -> 0;
order_type_to_int(up) -> 1.

direction_to_int(down) -> -1;
direction_to_int(stop) -> 0;
direction_to_int(up) -> 1;
direction_to_int(_) -> 0.

direction(0) -> stop;
direction(Relative_position) when Relative_position > 0 -> up;
direction(Relative_position) when Relative_position < 0 -> down.

compare(_X, 0) -> true;
compare(X, Y) -> X == Y.

sign(Argument) ->
	return_sign(Argument >=  0).

return_sign(true) -> 1;
return_sign(false) -> -1.
