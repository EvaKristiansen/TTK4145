%Heftig påvirka av Kjetil og Sivert sine koder

-module(connection)
-compile()

init()->
	NodeName = list_to_atom("heis@" ++ get_my_list_ip()),
	net_kernel:start([NodeName, longnames, 500]),
	{ok, ListenSocket} = gen_udp:open(?RECEIVE_PORT,[binary,{active,false}]),
	{ok, SendSocket} = gen_udp:open(?SEND_PORT, [binary, {active,true}, {broadcast, true}]),
    spawn(fun() -> broadcast_loop(SendSocket) end),
	spawn(fun() -> listen_for_connections(ListenSocket) end).
	
listen_for_connections(ListenSocket) ->
	{ok,{_Adress,?SEND_PORT,NodeName}}gen_udp:recv(ListenSocket,0),
	Node = node_to_atom(NodeName),
	net_adm:ping(Node),
	listen_for_connections(ListenSocket)

		
broadcast_loop(SendSocket) ->
	gen_udp:send(SendSocket,{255,255,255,255},?RECEIVE_PORT,atom_to_binary(node())),
	timer:sleep(5000),
	broadcast_loop(SendSocket).
	
get_my_list_ip() ->
	{ok, [IpTuple | _IpTail]} = inet:getif(),
	inet_parse:ntoa(element(1,IpTuple)).
