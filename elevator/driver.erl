-module (driver).
-export([start/1, stop/0]).
-export([init/0, set_motor_direction/1, set_button_lamp/3, set_floor_indicator/1, set_door_open_lamp/1]). %Consider if init is necessary
-compile(export_all).

%-record(order,{floor,direction}). MAY TURN OUT TO BE USEFUL?

 -define(NUM_FLOORS, 4).
 -define(NUM_BUTTONS, 3).
 -define(BUTTON_TYPES, [up,inner,down]).
 -record(button,{floor,type,state = 0}).

start(Sensor_monitor_pid) -> %Sensor monitor pid as argument for easy read
	%Spawn communication thread:
    spawn(?MODULE, init_port, ["driver/elev_port"]),
    %Wait before initializing:
    timer:sleep(100),
    %Initialize elevator, is void in c, so no return:
    init(),
    set_motor_direction(up), %DEBUG
    %Start sensor monitor that can send to process with PID SENSOR_MONITOR_PID in supermodule:
    spawn(fun() -> sensor_poller(Sensor_monitor_pid) end()).

stop() ->
    driver ! stop.

%%%%%%% SENSOR INPUT POLLER %%%%%%%%
sensor_poller(Sensor_monitor_pid)->
	% Start for last_floor = -1 and "no buttons pressed" 
	Buttons = create_buttons([],0),
	sensor_poller(Sensor_monitor_pid, -1, Buttons).

sensor_poller(Sensor_monitor_pid, Last_floor, Buttons) -> % (Variable, List)
	%Checking floor sensor input
	New_floor = get_floor_sensor_signal(),
	case (New_floor /= Last_floor) and (New_floor /= [255]) of %Reached a new floor if it is not last floor or no floor
		true ->
			Sensor_monitor_pid ! {new_floor_reached, New_floor},
			true;
		false ->
			false
	end,
	%Need to check for button sensor input
	Updated_buttons = button_sensor_poller(Sensor_monitor_pid, Buttons,[]),
	timer:sleep(50),
	sensor_poller(Sensor_monitor_pid, New_floor,Updated_buttons).

button_sensor_poller(Sensor_monitor_pid, Old_buttons, Updated_buttons) ->
	case Old_buttons of

		[Button | Rest ] -> %Still have buttons to check
			Floor = Button#button.floor,
			ButtonType = Button#button.type,
			State = Button#button.state,
			New_state = get_button_signal(ButtonType,Floor),

			case(New_state /= State) and (New_state == 1) of %Check if there are possibilities of removing nested-case here
				true ->
					 Sensor_monitor_pid ! {button_pressed, ButtonType, Floor},
					true;
				false  ->
					false
			end,

			New_buttons = Updated_buttons ++ [#button{floor=Floor,type = ButtonType,state = New_state}],
			button_sensor_poller(Sensor_monitor_pid, Rest, New_buttons);

		[] -> %No more buttons to check, return
			Updated_buttons
	end.

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
	io:fwrite("In init_port ~n ", []), %DEBUG
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
encode(elev_init) -> [1];

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

%%%%%% HELPER FUNCTIONS %%%%%%
create_buttons(Buttons,0) -> %At bottom floor
	New_buttons = Buttons ++ [#button{floor=0,type = inner}, #button{floor=0,type = up}],
	create_buttons(New_buttons,1);

create_buttons(Buttons,Floor) when (Floor>0) and (Floor < ?NUM_FLOORS-1)-> %At middle floor
	New_buttons = Buttons ++ [#button{floor=Floor,type = down}, #button{floor=Floor,type = inner},#button{floor=Floor,type = up}],
	create_buttons(New_buttons,Floor+1);
			
create_buttons(Buttons,?NUM_FLOORS-1) -> %At top floor
	Buttons ++ [#button{floor= ?NUM_FLOORS-1 ,type = down}, #button{floor=?NUM_FLOORS-1 ,type = inner}].

