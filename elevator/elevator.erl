-module(elevator).
-export([simple_drive/0]).
-compile(export_all).
-define(SENSOR_MONITOR_PID, smpid).

%THE ELEVATOR IS ASSUMED TO START ON BOTTOM FLOOR!
simple_drive() ->
	register(?SENSOR_MONITOR_PID, spawn(?MODULE, sensor_monitor, [])), 
	driver:start(?SENSOR_MONITOR_PID),
	driver:set_motor_direction(up).

sensor_monitor() -> 
	receive

		{new_floor_reached,Floor} ->
			io:fwrite("New floor reached ~w ~n ", [Floor]); %DEBUG

		{button_pressed, ButtonType, Floor} ->
			io:fwrite("Button pressed ~w ~w ~n ", [ButtonType,Floor]) %DEBUG
			%WHEN DEBUGGING; CHECK WHAT NUMBERS BUTTONTYPE ARE; MAYBE CONVERT IN DRIVER?
	end.