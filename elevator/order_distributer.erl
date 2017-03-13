- module (order_distributer).
- export([distribute_order/1, update_my_next/0]).
- compile(export_all).
 -define(NUM_FLOORS, 4).
 -define(NUM_BUTTONS, 3).
- record(order,{floor,type}).



update_my_next() ->
	Elevator_floor = state_storage:get_last_floor(node()),
	Elevator_direction = state_storage:get_direction(node()),
	Elevator_direction_int = direction_to_int(Elevator_direction),
	Outer_list = ordsets:to_list(queue_module:get_queue_set(node(), outer)),
	Inner_list = ordsets:to_list(queue_module:get_queue_set(node(), inner)),
	Order_list = Inner_list ++ Outer_list,
	update_my_next(Order_list, {10000, #order{floor = none, type = none}}, Elevator_floor, Elevator_direction_int).

update_my_next([],{_Best_penalty, Best_order}, _Elevator_floor, _Elevator_direction_int) -> queue_module:set_my_next(Best_order#order.floor);

update_my_next(Order_list, {Best_penalty, _Best_order}, Elevator_floor, Elevator_direction_int) ->
	[Order | Rest] = Order_list,

	Relative_position = Order#order.floor - Elevator_floor,		% Positive if pling is over elevator, else negative
	Moving_towards_pling = sign(Relative_position) == sign(Elevator_direction_int),	% True if elevator moves towards pling
	Equal_direction = (order_type_to_int(Order#order.type) == Elevator_direction_int), 		% True if elevator and signal same direction
	Distance = abs(Relative_position),

	Penalty = position_penalty(Moving_towards_pling,Equal_direction,Distance),
	case (Penalty < Best_penalty) of 
		true ->
			update_my_next(Rest, {Penalty,Order}, Elevator_floor, Elevator_direction_int);
		false ->
			update_my_next(Rest, {Best_penalty, _Best_order}, Elevator_floor, Elevator_direction_int)
	end.

distribute_order({order, Floor, inner}) ->
	Order = #order{floor=Floor,type = inner},
	queue_module:add_to_queue(node(),Order),
	io:fwrite("Gotten in distribute_order with type inner ~n", []),
	node();
distribute_order(Order) -> 
	Memberlist = [node()|nodes()],
	Penalties = get_penalties(Memberlist,[],Order),
	Winner = choose_winner(Memberlist, Penalties, {10000, dummy@member}),
	queue_module:add_to_queue(Winner, Order),
	Winner.


choose_winner([], _Penalties, {_Lowest_value, Member}) -> Member;
choose_winner(MemberList, Penalties, Best_so_far) ->
	[Member | Rest] = MemberList,
	choose_winner(Member, Rest, Penalties, Best_so_far).

choose_winner(Member_head, Member_rest, Penalties, {Lowest_value, Member}) ->
	[Penalty | Penalties_rest] = Penalties,
	case Penalty < Lowest_value of % Har tenkt på å erstatte denne, men da får vi 7 argument? 
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
	State = state_storage:get_state(Member),
	Elevator_floor = state_storage:get_last_floor(Member),
	Elevator_direction = state_storage:get_direction(Member),
	io:fwrite("Getting penalty for member: ~w, with direction: ~w, ~n", [Member, Elevator_direction]), 
	Elevator_direction_int = direction_to_int(Elevator_direction),

	Relative_position = Order#order.floor - Elevator_floor,		% Positive if pling is over elevator, else negative
	Moving_towards_pling = sign(Relative_position) == sign(Elevator_direction_int),	% True if elevator moves towards pling
	Equal_direction = (order_type_to_int(Order#order.type) == Elevator_direction_int), 		% True if elevator and signal same direction
	Distance = abs(Relative_position),
	
	Penalty = state_penalty(State) + position_penalty(Moving_towards_pling,Equal_direction,Distance),
	get_penalties(Rest,[Penalty|Penalties], Order).


state_penalty(init) -> 1000;
state_penalty(unknown) -> 1000; %TODO evaluer denne
state_penalty(idle) -> 10;
state_penalty(moving) -> 5;
state_penalty(door_open) -> 7;
state_penalty(stuck) -> 1000.

%distance_penalty(Order,Elevator_floor) ->
%	abs(Order#order.floor - Elevator_floor).

sign(Argument) ->
	return_sign(Argument >=  0). % This defines 0 as positive


return_sign(true) -> 1;
return_sign(false) -> -1.
	
%turn_penalty(Order_floor, Order_type, Elevator_floor, Elevator_direction) ->
%	Order = #order{floor = Order_floor, type = Order_type},
%	turn_penalty(Order, Elevator_floor, direction_to_int(Elevator_direction)).
%
%turn_penalty(Order, Elevator_floor, Elevator_direction_int) ->
%	Relative_position = Order#order.floor - Elevator_floor,		% Positive if pling is over elevator, else negative
%	Moving_towards_pling = sign(Relative_position) == sign(Elevator_direction_int),	% True if elevator moves towards pling
%	Equal_direction = (order_type_to_int(Order#order.type) == Elevator_direction_int), 		% True if elevator and signal same direction
%	get_penalty(Elevator_direction_int, Moving_towards_pling, Equal_direction).



%%%%%%%%%%%%%%%%%%%%%%%%%%%% REPLACE TURN AND DISTANCE PENALTY? %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
position_penalty(_, _, 0) ->
	0;
position_penalty(true, true , Distance) ->
	Distance;

position_penalty(true, false , Distance) ->
	?NUM_FLOORS - Distance + 10; %TURN PENALTY = 10, DEFINE?

position_penalty(false, true, Distance) ->
	?NUM_FLOORS - Distance + 2*10; %TURN PENALTY = 10, DEFINE?

position_penalty(false, false , Distance) ->
	Distance + 10. %TURN PENALTY = 10, DEFINE?

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


order_type_to_int(down) -> -1; %Consider merging with function below
order_type_to_int(inner) -> 0;
order_type_to_int(up) -> 1.

direction_to_int(down) -> -1;
direction_to_int(stop) -> 0;
direction_to_int(up) -> 1.

