-module(detest_net).
-export([start/1,toggle_online/2]).

toggle_online(Nm,Bool) when Bool == true; Bool == false ->
	case butil:ds_val(Nm,cfg) of
		undefined ->
			false;
		Pid ->
			Pid ! {toggle,Bool}
	end.

start(L) ->
	% First checks if necessary. It only runs if all IPs in L do not exist yet.
	{ok,IFL} = inet:getif(),
	ExistingIps = [IP || {IP,_,_} <- IFL],
	JustLIps = [IP || {IP,_Delay,_Nm} <- L],
	case JustLIps -- ExistingIps of
		JustLIps ->
			setup_ext(),
			Home = self(),
			% Every IP has his own process
			{Pids,_} = lists:foldl(fun({IP,Delay,Nm},{Pids,N}) -> 
				{[{spawn(fun() -> route(IP,Nm,N,Home,Delay) end),IP,N}|Pids], N+1} end, 
			{[],0}, L),
			
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

-record(dev,{name, dev, ip, index, peers = [], delay = {0,0},
	online = true, nextDelay = 0,
	in_buf = [], in_reversed = [], in_unreveresed = [],
	out_buf = [], out_reversed = [], out_unreveresed = []}).
route(IP,Name,Index,Home,Delay) ->
	R = create_device(IP,Index),
	Home ! {done,Index},
	erlang:send_after(10,self(),timeout),
	butil:ds_add(Name,self(),cfg),
	route1(false,R#dev{delay = {element(1,Delay)*1000,element(2,Delay)*1000}, name = Name}).


% If delay is set, packets from both directions are buffered in lists.
% Keep track of time with timer. Join frames into 10ms buffers.
% Theoretically 10ms at least. Erlang timers are not that exact so it may be skewed.
% Unpredictable delay is fine. Networks are unpredictable anyway.
route1(Time,#dev{in_reversed = []} = R) when R#dev.in_unreveresed /= [] ->
	route1(Time,R#dev{in_reversed = lists:reverse(R), in_unreveresed = []});
route1(Time,#dev{out_reversed = []} = R) when R#dev.out_unreveresed /= [] ->
	route1(Time,R#dev{out_reversed = lists:reverse(R), out_unreveresed = []});
route1({_,_,_} = Now, R) ->
	case R of
		#dev{in_reversed = [{Time,L}|_]} ->
			Diff = timer:now_diff(Now,Time),
			case Diff >= (element(1,R#dev.delay) + random:uniform(element(2,R#dev.delay))) of
				true ->
					[tuncer:send(R#dev.dev,Bin) || Bin <- L],
					Again = true,
					InRev = tl(R#dev.in_reversed);
				false ->
					Again = false,
					InRev = R#dev.in_reversed
			end;
		_ ->
			Again = false,
			InRev = R#dev.in_reversed
	end,
	case R of
		#dev{out_reversed = [{Time2,L2}|_]} ->
			Diff2 = timer:now_diff(Now,Time2),
			case Diff2 >= (element(1,R#dev.delay) + random:uniform(element(2,R#dev.delay))) of
				true ->
					[[P ! Data || P <- R#dev.peers] || Data <- L2],
					Again1 = true,
					OutRev = tl(R#dev.out_reversed);
				false ->
					Again1 = false,
					OutRev = R#dev.out_reversed
			end;
		_ ->
			Again1 = false,
			OutRev = R#dev.out_reversed
	end,
	NR = #dev{out_reversed = OutRev, in_reversed = InRev},
	case Again orelse Again1 of
		true ->
			route1(os:timestamp(),NR);
		false ->
			route2(NR)
	end;
route1(_,R) ->
	route2(R).
route2(R) ->
	receive
		{tuntap, Dev, Data} ->
			case R#dev.online of
				true when R#dev.delay /= {0,0} ->
					route1(false,R#dev{out_buf = [Data|R#dev.out_buf]});
				true ->
					[P ! Data || P <- R#dev.peers],
					route2(R);
				false ->
					route2(R)
			end;
		<<_/binary>> = Bin ->
			case R#dev.online of
				true when R#dev.delay /= {0,0} ->
					route1(false,R#dev{in_buf = [Bin|R#dev.in_buf]});
				true ->
					tuncer:send(R#dev.dev,Bin),
					route2(R);
				false ->
					route2(R)
			end;
		timeout ->
			Now = os:timestamp(),
			erlang:send_after(10,self(),timeout),
			case R#dev.in_buf of
				[] ->
					InUnrev = R#dev.in_unreveresed;
				_ ->
					InUnrev = [{Now,lists:reverse(R#dev.in_buf)}|R#dev.in_unreveresed]
			end,
			case R#dev.out_buf of
				[] ->
					OutUnrev = R#dev.out_unreveresed;
				_ ->
					OutUnrev = [{Now,lists:reverse(R#dev.out_buf)}|R#dev.out_unreveresed]
			end,
			route1(Now,R#dev{out_buf = [], in_buf = [], 
						in_unreveresed = InUnrev,
						out_unreveresed = OutUnrev});
		{toggle,Bool} ->
			route1(false,R#dev{online = Bool});
		{peerlist,L} ->
			route1(false,R#dev{peers = [Pid || {Pid,_Ip,Index} <- L, Index /= R#dev.index]});
		stop ->
			ok
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


cleanup([]) ->
	file:delete("procket"),
	file:delete("procket.so");
cleanup(Pids) ->
	receive
		{'DOWN',_Monitor,_,Pid,Reason} ->
			cleanup(lists:delete(Pid,Pids))
	end.

create_device(IP,N) ->
	Nm = "tap"++integer_to_list(N),
	{ok, Dev} = tuncer:create(Nm, [tap, no_pi, {active, true}]),
	ok = tuncer:up(Dev, IP),
	R = #dev{dev = Dev, ip = IP, index = N}.


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