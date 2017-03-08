-module (driver).
-export([start/0, stop/0]).
%-export( [init/0, set_motor_direction/1, set_button_lamp/3, set_floor_indicator/1, set_door_open_lamp/1, get_button_signal/2, get_floor_sensor_signal/0]).
%-compile(export_all).
%-record(order,{floor,direction}). MAY TURN OUT TO BE USEFUL?
 -define(NUM_FLOORS, 4).
 -define(NUM_BUTTONS, 3).
 -define(BUTTON_TYPES, [up,inner,down]).
 -define(SENSOR_MONITOR_PID, smpid).
 -record(button,{floor,type,state = 0}).

start() ->
	%Spawn communication thread:
    spawn(?MODULE, init_port, ["../driver/elev_port"]),
    %Wait before initializing:
    timer:sleep(100),
    %Initialize elevator, is void in c, so no return:
    init(),
    %Start sensor monitor that can send to process with PID MONITORPID in supermodule:
    spawn(fun() -> sensor_poller() end()).

stop() ->
    driver ! stop.

%%%%%%% SENSOR INPUT POLLER %%%%%%%%
sensor_poller()->
	% Start for last_floor = -1 and "no_buttons_pressed" 
	Buttons = create_buttons([],0),
	sensor_poller(-1, Buttons).

create_buttons(Buttons,Floor) ->
	if
		Floor == 0 -> %At bottom floor
			New_buttons = Buttons ++ [#button{floor=Floor,type = inner}, #button{floor=Floor,type = up}],
			create_buttons(New_buttons,Floor+1);
			
		Floor == ?NUM_FLOORS - 1  -> %At top floor
			New_buttons = Buttons ++ [#button{floor=Floor,type = down}, #button{floor=Floor,type = inner}];
			create_buttons(New_buttons,Floor+1);

		(Floor > 0) and (Floor < ?NUM_FLOORS -1)  -> %At middle floor
			New_buttons = Buttons ++ [#button{floor=Floor,type = down}, #button{floor=Floor,type = inner},#button{floor=Floor,type = up}];
			create_buttons(New_buttons,Floor+1);
			
		true -> %Out of scope
			Buttons
	end.

sensor_poller(Last_floor, Last_button_states) -> % (Variable, List)
	%Checking floor sensor input
	{driver, New_floor} = get_floor_sensor_signal(),
	case (New_floor /= Last_floor) and (New_floor /= -1) of %Reached a new floor if it is not last floor or no floor
		true ->
			?SENSOR_MONITOR_PID ! {new_floor_reached, New_floor},
			true;
		false ->
			false
	end,
	%Need to check for button sensor input
	button_sensor_poller(Last_button_states,[],0),
	sensor_poller(New_floor,New_button_states).


%%%%%%% ERL VERSIONS OF C FUNCTIONS %%%%%%%%
init() -> call_port(elev_init).
set_motor_direction(Direction) -> call_port({elev_set_motor_direction, Direction}).
set_button_lamp(ButtonType,Floor,Value) -> call_port({elev_set_button_lamp,ButtonType,Floor, Value}).
set_floor_indicator(Floor) -> call_port({elev_set_floor_indicator,Floor}).
set_door_open_lamp(Value) -> call_port({elev_set_door_open_lamp, Value}).
get_button_signal(ButtonType,Floor) -> call_port({elev_get_button_signal,ButtonType,Floor}).
get_floor_sensor_signal() -> call_port({elev_get_floor_sensor_signal}).


%%%%%%% COMMUNICATION WITH C PORT %%%%%%%%
init_port(ExtPrg) ->
    register(driver, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    loop(Port).

loop(Port) ->

    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {driver, Data}
	    end,
	    loop(Port);

	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', _Port, _Reason} ->
	    exit(port_terminated)
    end.

call_port(Msg) ->
    driver ! {call, self(), Msg},
    receive
	{driver, Result} ->
	    Result
    end.


%%%%%%% ENCODING MESSAGES FOR C PORT %%%%%%%%
encode({elev_init}) -> [1];

encode({elev_set_motor_direction, up}) -> [2,1];
encode({elev_set_motor_direction, stop}) -> [2,0];
encode({elev_set_motor_direction, down}) -> [2,-1];

encode({elev_set_button_lamp,up, Floor ,on}) -> [3,0,Floor,1];
encode({elev_set_button_lamp,inner, Floor,on}) -> [3,2,Floor,1];
encode({elev_set_button_lamp,down, Floor,on}) -> [3,1,Floor,1];

encode({elev_set_button_lamp,up, Floor , off}) -> [3,0,Floor,0];
encode({elev_set_button_lamp,inner, Floor, off}) -> [3,2,Floor,0];
encode({elev_set_button_lamp,down, Floor, off}) -> [3,1,Floor,0];

encode({elev_set_floor_indicator, Floor}) -> [4, Floor];

encode({elev_set_door_open_lamp, on}) -> [5, 1];
encode({elev_set_door_open_lamp, off}) -> [5, 0];

encode({elev_get_button_signal,up,Floor}) -> [6,0,Floor];
encode({elev_get_button_signal,inner,Floor}) -> [6,2,Floor];
encode({elev_get_button_signal,down,Floor}) -> [6,1,Floor];

encode({elev_get_floor_sensor_signal}) -> [7].