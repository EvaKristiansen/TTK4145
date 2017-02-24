-module (driver).
-export([start/1, stop/0]).
-export( [init/0, set_motor_direction/1, set_button_lamp/2, set_floor_indicator/1, set_door_open_lamp/1, get_button_signal/1, get_floor_sensor_signal/0]).

-record(order,{floor,direction}).


start(Elevator) ->
    spawn(?MODULE, init_port, ["../driver/elev_port"]).
    spawn(fun() -> new_floor_listener(-1) end. )

stop() ->
    driver ! stop.


%%%%%%% ERL VERSIONS OF C FUNCTIONS %%%%%%%%
init() -> call_port(elev_init).
set_motor_direction(Direction) -> call_port(elev_set_motor_direction, Direction).
set_button_lamp(Direction,Floor,Value) -> call_port(elev_set_button_lamp, Order, Value).
set_floor_indicator(Floor) -> call_port(elev_set_floor_indicator,Floor).
set_door_open_lamp(Value) -> call_port(elev_set_door_open_lamp, Value).
get_button_signal(Direction,Floor) -> call_port(elev_get_button_signal,Direction,Floor).
get_floor_sensor_signal() -> call_port(elev_get_floor_sensor_signal).




%%%%%%% COMMUNICATION WITH C PORT %%%%%%%%
init_port(ExtPrg) ->
    register(driver, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    loop(Port).

call_port(Msg) ->
    driver ! {call, self(), Msg},
    receive
	{driver, Result} ->
	    Result
    end.


loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {driver, decode(Data)}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit(port_terminated)
    end.

%%%%%%% ENCODING MESSAGES FOR C PORT %%%%%%%%
encode({elev_init}) -> [1];

encode({elev_set_motor_direction, up}) -> [2,1];
encode({elev_set_motor_direction, stop}) -> [2,0];
encode({elev_set_motor_direction, down) -> [2,-1];

encode({elev_set_button_lamp,up, Floor ,on) -> [3,0,Floor,1];
encode({elev_set_button_lamp,inside, Floor,on) -> [3,2,Floor,1];
encode({elev_set_button_lamp,down, Floor,on) -> [3,1,Floor,1];

encode({elev_set_button_lamp,up, Floor ,1) -> [3,0,Floor,0];
encode({elev_set_button_lamp,inside, Floor,1) -> [3,2,Floor,0];
encode({elev_set_button_lamp,down, Floor,1) -> [3,1,Floor,0];

encode({elev_set_floor_indicator, Floor}) -> [4, Floor];

encode({elev_set_door_open_lamp, on}) -> [5, 1];
encode({elev_set_door_open_lamp, off}) -> [5, 0];

encode({elev_get_button_signal,up,Floor}) -> [6,0,Floor];
encode({elev_get_button_signal,inside,Floor}) -> [6,2,Floor];
encode({elev_get_button_signal,down,Floor}) -> [6,1,Floor];

encode({elev_get_floor_sensor_signal}) -> [7];










