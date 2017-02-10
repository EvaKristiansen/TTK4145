%Heftig påvirka av Kjetil og Sivert sine koder

-module(connection).
-export([init/0]).

-define(SEND_PORT, 20013).
-define(RECEIVE_PORT, 20014).


init()->
	os:cmd("epmd -daemon"),	

	NodeName = list_to_atom("heis@" ++ get_my_list_ip()),
	net_kernel:start([NodeName, longnames, 500]),
	
	{ok, ListenSocket} = gen_udp:open(?RECEIVE_PORT,[list,{active,false}]),
	{ok, SendSocket} = gen_udp:open(?SEND_PORT, [list, {active,true}, {broadcast, true}]),
	
	Fir = spawn(fun() -> listen_for_connections(ListenSocket) end),
	Sec = spawn(fun() -> broadcast_loop(SendSocket) end),
	[Fir, Sec].
	
listen_for_connections(ListenSocket) ->
	{ok,{_,Sender,NodeName}} = gen_udp:recv(ListenSocket,0),
	case Sender of
		?SEND_PORT ->
			io:fwrite("Got message from correct port! ~n", []),
			Node = list_to_atom(NodeName),
			net_adm:ping(Node),
			listen_for_connections(ListenSocket);
		(_) ->
			listen_for_connections(ListenSocket)
	end.


broadcast_loop(SendSocket) ->
	gen_udp:send(SendSocket,{255,255,255,255},?RECEIVE_PORT,atom_to_list(node())),
	timer:sleep(5000),
	broadcast_loop(SendSocket).

get_my_list_ip() ->
	{ok, [IpTuple | _IpTail]} = inet:getif(),
	inet_parse:ntoa(element(1,IpTuple)).

