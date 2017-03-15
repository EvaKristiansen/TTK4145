-module(connection).
-export([init/1]).

-define(SEND_PORT, 20013).
-define(RECEIVE_PORT, 20014).

init(Init_listener)->
	os:cmd("epmd -daemon"),	

	Node_name = list_to_atom("heis@" ++ get_my_list_ip()),
	net_kernel:start([Node_name, longnames, 500]),
	erlang:set_cookie(node(), 'monster'),
	
	{ok, Listen_socket} = gen_udp:open(?RECEIVE_PORT,[list,{active,false}]),
	{ok, Send_socket} = gen_udp:open(?SEND_PORT, [list, {active,true}, {broadcast, true}]),
	
	spawn(fun() -> listen_for_connections(Listen_socket) end),
	spawn(fun() -> broadcast_loop(Send_socket) end),
	timer:sleep(60), 
	Init_listener ! connection_init_complete.
	
listen_for_connections(Listen_socket) ->
	{ok,{_,Sender,Node_name}} = gen_udp:recv(Listen_socket,0),
	react_to_connection(Sender, Listen_socket, Node_name). %Making sure we got a message from the correct place


react_to_connection(?SEND_PORT, Listen_socket, Node_name) -> 
	Node = list_to_atom(Node_name),
	establishconnection(Node),
	listen_for_connections(Listen_socket);
react_to_connection(_Sender, Listen_socket, _Node_name) ->
	listen_for_connections(Listen_socket).

get_my_list_ip() ->
	{ok, [Ip_tuple | _Ip_tail]} = inet:getif(),
	inet_parse:ntoa(element(1,Ip_tuple)).

broadcast_loop(Send_socket) ->
	gen_udp:send(Send_socket,{255,255,255,255},?RECEIVE_PORT,atom_to_list(node())),
	timer:sleep(5000),
	broadcast_loop(Send_socket).

establishconnection(Node) ->
	case newnode(Node) of
		true -> net_adm:ping(Node);
		false -> ok
	end.


newnode(Node) ->
	Nodelist = [node()|nodes()],
	(not lists:member(Node,Nodelist)).


