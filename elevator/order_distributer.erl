- module (order_distributer).
- export([distribute_order/1]).
- compile(export_all).
- record(order,{floor,type}).

distribute_order(Order) -> 
	Memberlist = [node()|nodes()],
	Penalties = getpenalties(Memberlist,[],Order),	
	Winner = choose_winner(Memberlist, Penalties, {10000, dummy@member}),
	queue_module:add_to_queue(Winner),
	Winner.

choose_winner(Memberlist, Penalties, {Lowest_value, Member}) ->
	case Memberlist of 
		[Member_head | Member_rest] -> 
			[Penalty | Penalties_rest] = Penalties,
			case Penalty < Lowest_value of 
				true ->
					choose_winner(Member_rest, Penalties_rest, {Penalty, Member_head});
				false ->
					choose_winner(Member_rest, Penalties_rest, {Lowest_value, Member})
			end;
		[] ->
			Member
	end.

getpenalties(Memberlist, Penalties, Order) ->
	case Memberlist of
		[Member | Rest] ->
			State = fsm:get_state(Member), %Can send to FSM and receive in stead, more erlangish I think
			Last_floor = fsm:get_last_floor(Member),
			Direction = fsm:get_direction(Member),
			Penalty = state_penalty(State) + distance_penalty(Order,Last_floor) + turn_penalty(Order,Last_floor,Direction),
			getpenalties(Rest,[Penalty|Penalties],order); 
		[] ->
			Penalties
		
	end.		
	
state_penalty(init) -> 1000;
state_penalty(unknown) -> 1000; %TODO evaluer denne
state_penalty(idle) -> 10;
state_penalty(moving) -> 5;
state_penalty(door_open) -> 7;
state_penalty(stuck) -> 1000.

distance_penalty(Order,Elevator_floor) ->
	abs(Order#order.floor - Elevator_floor).

sign(Argument) ->
	case(Argument > 0) of
		true -> 
			1;
		false ->
			-1
	end. 
	
turn_penalty(Order_floor, Order_type, Elevator_floor, Elevator_direction) ->
	Order = #order{floor = Order_floor, type = Order_type},
	turn_penalty(Order, Elevator_floor, direction_to_int(Elevator_direction)).

turn_penalty(Order, Elevator_floor, Elevator_direction) ->
	Relative_position = Order#order.floor - Elevator_floor,		% Positive if pling is over elevator, else negative
	Moving_towards_pling = sign(Relative_position) == sign(Elevator_direction),	% True if elevator moves towards pling
	Equal_direction = (order_type_to_int(Order#order.type) == Elevator_direction), 		% True if elevator and signal same direction
	get_penalty(Elevator_direction, Moving_towards_pling, Equal_direction).

get_penalty(0, _, _) -> 0; %Define as macros or change name? Is technically not a record, I believe
get_penalty(_dontcare, true, true) -> 0;
get_penalty(_dontcare, true, false) -> 2;
get_penalty(_dontcare, false, _) -> 20.

	

order_type_to_int(Type) ->
	case Type of
		down ->
			Num = -1;
		inner ->
			Num = 0;
		up ->
			Num = 1
	end,
	Num.

direction_to_int(Direction) ->
	case Direction of
		down ->
			Num = -1;
		stop ->
			Num = 0;
		up ->
			Num = 1
	end,
	Num.