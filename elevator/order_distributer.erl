- module (order_distributer).
- export([distribute_order/1,get_next_order/1, set_next_order_really_smart/0]).
- compile(export_all).
- record(order,{floor,type}).

get_next_order(ElevatorID) -> % TODO, mulig å forbedre valget? Er det nødvendig i det hele tatt?
	In_option = queue_module:get_first_in_queue(ElevatorID,inner),
	Out_option = queue_module:get_first_in_queue(ElevatorID, outer),
	choose_next_order(ElevatorID,In_option,Out_option).

choose_next_order(_ElevatorID,empty,empty) ->
	false;
choose_next_order(_ElevatorID,Option1,empty) ->
	Option1;

choose_next_order(_ElevatorID,empty,Option2) ->
	Option2;

choose_next_order(ElevatorID,Option1,Option2) ->			
	
	Last_floor = state_storage:get_last_floor(ElevatorID),
	Direction = state_storage:get_direction(ElevatorID),

	Option1_penalty = distance_penalty(Option1,Last_floor) + turn_penalty(Option1,Last_floor,Direction),
	Option2_penalty = distance_penalty(Option1,Last_floor) + turn_penalty(Option1,Last_floor,Direction),

	choose_next_order(Option1,Option2,Option1_penalty,Option2_penalty).

choose_next_order(Option1,_Option2,Option1_penalty,Option2_penalty) when Option2_penalty >= Option1_penalty ->
	Option1;
choose_next_order(_Option1,Option2,_Option1_penalty,_Option2_penalty) ->
	Option2.




distribute_order(Order) -> 
	Memberlist = [node()|nodes()],
	io:fwrite("In distribute_order with order: ~w ~n",[Order]), %DEBUG
	Penalties = getpenalties(Memberlist,[],Order),	
	Winner = choose_winner(Memberlist, Penalties, {10000, dummy@member}),
	queue_module:add_to_queue(Winner, Order),
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
			State = state_storage:get_state(Member), %Can send to FSM and receive in stead, more erlangish I think
			Last_floor = state_storage:get_last_floor(Member),
			Direction = state_storage:get_direction(Member),
			Penalty = state_penalty(State) + distance_penalty(Order#order.floor, Last_floor) + turn_penalty(Order,Last_floor,Direction),
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

distance_penalty(Order_floor,Elevator_floor) ->
	io:fwrite("Order floor that crashes everything::: ~w ~n ", [Order_floor]),
	io:fwrite("Elevator floor that crashes everything::: ~w ~n ", [Elevator_floor]),
	abs(Order_floor - Elevator_floor).

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

get_penalty(0, _, _) -> 0;
get_penalty(_dontcare, true, true) -> 0;
get_penalty(_dontcare, true, false) -> 2;
get_penalty(_dontcare, false, _) -> 20.

	

order_type_to_int(down) -> -1; %Consider merging with function below
order_type_to_int(inner) -> 0;
order_type_to_int(up) -> 1.

direction_to_int(down) -> -1;
direction_to_int(stop) -> 0;
direction_to_int(up) -> 1.

set_next_order_really_smart() -> % TODO Work in progress, fungerer bare når retning /= stop
	Elevator_floor = state_storage:get_last_floor(node()),
	Elevator_direction = state_storage:get_direction(node()),
	Outer_list = ordsets:to_list(queue_module:get_queue_set(node(), outer)),
	Inner_list = ordsets:to_list(queue_module:get_queue_set(node(), inner)),
	Local_pid = spawn(fun() -> additional_help_function(-1, self()) end),
	lists:foreach(fun(Order) -> 
		help_function(Order, Elevator_floor, Elevator_direction, Local_pid, 1000)
	end,
	lists:merge(Outer_list,Inner_list)),
	receive 
		{ok, Next_floor} ->
			queue_module:update_next(Next_floor)
	after 2000 ->
		io:fwrite("Our smart function never returns ~n", []) %Debug
	end.		
	

additional_help_function(Last_Next_floor, Pid) ->
	receive 
		{ok, New_next_floor} ->
			additional_help_function(New_next_floor, Pid)
	after 100 ->
		ok
	end,
	Pid ! {ok, Last_Next_floor}.



help_function(Order, Elevator_floor, Elevator_direction, Pid, Best_penalty_so_far) -> % given Floor and Direction is int
	Relative_position = Order#order.floor - Elevator_floor,		% Positive if pling is over elevator, else negative
	Moving_towards_pling = sign(Relative_position) == sign(Elevator_direction),	% True if elevator moves towards pling	
	Same_direction = order_type_to_int(Order#order.type) == direction_to_int(Elevator_direction),
	Penalty = get_local_penalty(Same_direction, Moving_towards_pling, Order, Elevator_floor),
	case Penalty < Best_penalty_so_far of 
		true ->
			Pid ! {ok, Order};
		false ->
			ok
	end.

get_local_penalty(true, true, Order, Elevator_floor) ->
	distance_penalty(Order#order.floor, Elevator_floor);
get_local_penalty(true, false, Order, Elevator_floor) ->
	20 + distance_penalty(Elevator_floor, Order#order.floor); % Inverted direction_cost, 20 for switching direction

get_local_penalty(false, ture, Order, Elevator_floor) ->
	24 + distance_penalty(Elevator_floor, Order#order.floor); % Inverted direction_cost, 24 for switching direction and turning back
get_local_penalty(false, false, Order, Elevator_floor) ->
	20 + distance_penalty(Order#order.floor, Elevator_floor).

