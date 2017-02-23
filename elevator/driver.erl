-module (driver).
-export([start/1, stop/0]).
-export( [init/0, set_motor_direction/1, set_button_lamp/2, set_floor_indicator/1, set_door_open_lamp/1, get_current_floor/0, get_new_order/0]).

-record(order,{floor,direction}).


start(Elevator) ->
    spawn(?MODULE, init_port, ["../driver/elev_port"]).
    spawn(fun() -> new_floor_listener(-1) end. )

stop() ->
    driver ! stop.


init() -> call_port(elev_init).
set_motor_direction(Direction) -> call_port(elev_set_motor_direction, Direction).
set_button_lamp(Order, Value) -> call_port(elev_set_button_lamp, Order, Value).
set_floor_indicator(Floor) -> call_port(elev_set_floor_indicator,Floor).
set_door_open_lamp(Value) -> call_port(elev_set_door_open_lamp, Value).

%UFERDIGE, LITT EKLE Å SKRIVE
get_current_floor()->

get_new_order() ->




% C communication, må skrive C-kode som denne kommuniserer med!
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

% Encoding, uferdig
encode({elev_init}) -> [1];

encode({elev_set_motor_direction, up}) -> [2,1];
encode({elev_set_motor_direction, stop}) -> [2,0];
encode({elev_set_motor_direction, down) -> [2,-1];

encode({elev_set_button_lamp,Order,on) -> [3,Order#order.direction,Order#order.floor,1];
encode({elev_set_button_lamp,Order,off) -> 3,Order#order.direction,Order#order.floor,0];

encode({elev_set_floor_indicator, Floor}) -> [8, Floor];







