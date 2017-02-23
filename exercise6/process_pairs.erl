-module(process_pairs).
-export([start/0]).

start() ->
	spawn(fun() -> counter_start(0) end).

counter_start(N) ->
	Death = N + rand:uniform(50),
	io:format("process will die at number ~p ~n", [Death]),
	BackUpPid = spawn(fun() -> backup(N) end),
	counter(N,BackUpPid,Death).

counter(N, BackUpPid, Death) when N =< Death ->
	BackUpPid ! {ok,N},
	io:format("~p: ~p ~n", [self(),N]),
	timer:sleep(500),
	counter(N+1,BackUpPid,Death);

counter(_N, _BackUpPid, _Death) ->
	ok.

backup(Next) ->
	receive
	{ok, Next} ->
		backup(Next+1)
	after 5000 ->
		io:format("Backup taking over, ", []),
		counter_start(Next)
	end.
