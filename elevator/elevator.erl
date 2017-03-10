-module(elevator).
-export([simple_drive/0]).
-compile(export_all).
-define(SENSOR_MONITOR_PID, smpid).
-define(DRIVER_MANAGER_PID, dmpid).

%THE ELEVATOR IS ASSUMED TO START ON BOTTOM FLOOR!
simple_drive() ->
	register(?SENSOR_MONITOR_PID, spawn(fun() -> sensor_monitor() end)), 
	register(?DRIVER_MANAGER_PID, spawn(fun() -> simple_driver_manager1() end)).


start() ->
	