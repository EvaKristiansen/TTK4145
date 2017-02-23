-module (driver).
-export([start/1, stop/0]).
-export( [init/0, set_motor_direction/1, set_button_lamp/3, set_floor_indicator/1, set_door_open_lamp/1, get_current_floor/0, get_new_order/0]).



start(Elevator) ->
    spawn(?MODULE, init_port, ["../driver/elev_port"]).
    spawn(fun() -> new_floor_listener(-1) end. )

stop() ->
    driver ! stop.



init() -> call_port(elev_init).
set_motor_direction(direction) -> call_port(elev_set_motor_direction,direction).
set_button_lamp(direction, floor, value) -> call_port(elev_set_button_lamp, direction, floor, value).
set_floor_indicator(floor) -> call_port(elev_set_floor_indicator,floor).
set_door_open_lamp(value) -> call_port(elev_set_door_open_lamp, value).

new_floor_listener(LastFloor)->
	CurrentFloor = call_port(elev_get_floor_sensor_signal)

get_current_floor()->

get_new_order() ->




% C communication
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
