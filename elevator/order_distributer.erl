- module (order_distributer).
- export([distribute_order/1]).
- compile(export_all).
- record(order,{floor,direction}).

 

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
			State = fsm:get_state(Member),
			Last_floor = fsm:get_last_floor(Member),
			Direction = fsm:get_direction(Member),
			Penalty = state_penalty(State) + distance_penalty(Order,Last_floor) + turn_penalty(Order,Last_floor,Direction),
			getpenalties(Rest,[Penalty|Penalties],order); 
		[] ->
			Penalties
		
	end.		
	
state_penalty(init) -> 1000;
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

turn_penalty(Order, Elevator_floor, Elevator_direction) ->
	Relative_position = Order#order.floor - Elevator_floor,		% Positive if pling is over elevator, else negative
%	case sign(Relative_position) == sign(Elevator_direction) of 	% Elevator is moving towards pling
%		true ->
%			case (Order#order.direction == Elevator_direction) of % Signal and elevator same direction
%				true ->
%					0;
%				false ->
%					2,
%			end,
%		false ->						% Elevator is moving away from pling or standing still
%			case (Elevator_direction == 0) 
%				true ->	0;
%				false -> 20,				
%			end;
%	end.

	Moving_towards_pling = sign(Relative_position) == sign(Elevator_direction),
	Equal_direction = (Order#order.direction == Elevator_direction),
	turn_penalty_record(Elevator_direction, Moving_towards_pling, Equal_direction).

turn_penalty_record(true, _dontcare, _dontcare) -> 0;
turn_penalty_record(_dontcare, true, true) -> 0;
turn_penalty_record(_dontcare, true, false) -> 2;
turn_penalty_record(_dontcare, false, _dontcare) -> 20.


		

distribute_order(Order) -> 
	Memberlist = [node()|nodes()],
	Penalties = getpenalties(Memberlist,[],Order),	
	choose_winner(Memberlist, Penalties, {10000, dummy@member}).	

