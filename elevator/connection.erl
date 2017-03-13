%Heftig påvirka av Kjetil og Sivert sine koder

-module(connection).
-export([init/0]).

-define(SEND_PORT, 20013).
-define(RECEIVE_PORT, 20014).


init()->
	os:cmd("epmd -daemon"),	

	NodeName = list_to_atom("heis@" ++ get_my_list_ip()),
	net_kernel:start([NodeName, longnames, 500]),
	erlang:set_cookie(node(), 'monster'),
	
	{ok, ListenSocket} = gen_udp:open(?RECEIVE_PORT,[list,{active,false}]),
	{ok, SendSocket} = gen_udp:open(?SEND_PORT, [list, {active,true}, {broadcast, true}]),
	
	spawn(fun() -> listen_for_connections(ListenSocket) end),
	spawn(fun() -> broadcast_loop(SendSocket) end).
	
listen_for_connections(ListenSocket) ->
	{ok,{_,Sender,NodeName}} = gen_udp:recv(ListenSocket,0),
	react_to_connection(Sender, ListenSocket, NodeName). %Making sure we got a message from the correct place


react_to_connection(?SEND_PORT, ListenSocket, NodeName) -> 
	Node = list_to_atom(NodeName),
	establishconnection(Node),
	listen_for_connections(ListenSocket);
react_to_connection(_Sender, ListenSocket, _NodeName) ->
	listen_for_connections(ListenSocket).

get_my_list_ip() ->
	{ok, [IpTuple | _IpTail]} = inet:getif(),
	inet_parse:ntoa(element(1,IpTuple)).

broadcast_loop(SendSocket) ->
	gen_udp:send(SendSocket,{255,255,255,255},?RECEIVE_PORT,atom_to_list(node())),
	timer:sleep(5000),
	broadcast_loop(SendSocket).

establishconnection(Node) ->
	case newnode(Node) of
		true -> net_adm:ping(Node);
		false -> ok
	end.


newnode(Node) ->
	Nodelist = [node()|nodes()],
	(not lists:member(Node,Nodelist)).


