-module(detest_net).
-compile(export_all).

-record(dev,{dev, ip, index, peers = []}).

start(L) ->
	% First checks if necessary. It only runs if all IPs in L do not exist yet.
	{ok,IFL} = inet:getif(),
	ExistingIps = [IP || {IP,_,_} <- IFL],
	case L -- ExistingIps of
		L ->
			setup_ext(),
			Home = self(),
			% Every IP has his own process
			{Pids,_} = lists:foldl(fun(IP,{Pids,N}) -> {[{spawn(fun() -> route(IP,N,Home) end),IP,N}|Pids], N+1} end, {[],0}, L),
			
			case wait_ack(Pids) of
				ok ->
					spawn(fun() -> Pids1 = [begin erlang:monitor(process,P),P end || {P,_,_} <- Pids], cleanup(Pids1) end),
					% Return list of pids and also inform every pid of where everyone else is
					[begin
						Pid ! {peerlist, Pids},
						Pid
					end || {Pid,_,_} <- Pids];
				{error,Faults} ->
					{error,[IP || {_,IP,_} <- Faults]}
			end;
		_ ->
			[]
	end.

wait_ack([]) ->
	ok;
wait_ack(Pids) ->
	receive
		{done,Index} ->
			wait_ack(lists:keydelete(Index,3,Pids))
	after 5000 ->
		cleanup([]),
		{error,Pids}
	end.

route(IP,Index,Home) ->
	R = create_device(IP,Index),
	Home ! {done,Index},
	route1(R).
route1(R) ->
	receive
		{peerlist,L} ->
			route1(R#dev{peers = [Pid || {Pid,_Ip,Index} <- L, Index /= R#dev.index]});
		{tuntap, Dev, Data} ->
			% tuncer:send(D#dev.dev,Data)
			[P ! Data || P <- R#dev.peers],
			route1(R);
		<<_/binary>> = Bin ->
			tuncer:send(R#dev.dev,Bin),
			route1(R);
		stop ->
			ok
	end.

cleanup([]) ->
	file:delete("procket"),
	file:delete("procket.so");
cleanup(Pids) ->
	receive
		{'DOWN',_Monitor,_,Pid,Reason} ->
			cleanup(lists:delete(Pid,Pids))
	end.

% create_devices([H|T],N) ->
create_device(IP,N) ->
	Nm = "tap"++integer_to_list(N),
	{ok, Dev} = tuncer:create(Nm, [tap, no_pi, {active, true}]),
	% io:format("Creating ~p:~p~n",[Nm,IP]),
	ok = tuncer:up(Dev, IP),
	R = #dev{dev = Dev, ip = IP, index = N}.
% 	[R|create_devices(T,N+1)];
% create_devices([],_) ->
% 	[].


setup_ext() ->
	case catch escript:script_name() of
		[_|_] ->
			{ok,Myself} = file:read_file(escript:script_name()),
			[_,BinWithoutHead] = binary:split(Myself,<<80,75,3,4>>),
			ZipBin = <<80,75,3,4,BinWithoutHead/binary>>;
		_ ->
			{ok,ZipBin} = file:read_file("detest.ez")
	end,
	case zip:extract(ZipBin,[memory]) of
		{ok,ZipFiles} ->
			[begin
				file:write_file(Name,PrBin),
				os:cmd("chmod +x "++Name)
			end || {Name,PrBin} <- ZipFiles, Name == "procket" orelse Name == "procket.so"];
		_ ->
			ok
	end.