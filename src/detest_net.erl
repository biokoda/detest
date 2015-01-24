-module(detest_net).
% script api
-export([toggle_online/2, set_delay/2]).
% internal call
-export([start/1]).

toggle_online(Nm,Bool) when Bool == true; Bool == false ->
	case butil:ds_val(Nm,cfg) of
		undefined ->
			false;
		Pid ->
			Pid ! {toggle,Bool}
	end.

set_delay(Nm,{N1,0}) when N1 > 0 ->
	set_delay(Nm,{N1,1});
set_delay(Nm,Delay) ->
	case butil:ds_val(Nm,cfg) of
		undefined ->
			false;
		Pid ->
			Pid ! {set_delay,Delay}
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
	online = true, 
	in_buf = [], in_reversed = [], in_unreversed = [],
	out_buf = [], out_reversed = [], out_unreversed = []}).
route(IP,Name,Index,Home,Delay1) ->
	case ok of
		_ when element(1,Delay1) > 0, element(2,Delay1) == 0 ->
			Delay = {element(1,Delay1),1};
		_ when is_integer(Delay1) ->
			Delay = {Delay1,1};
		_ ->
			Delay = Delay1
	end,
	Home ! {done,Index},
	case Delay of
		{0,0} ->
			ok;
		_ ->
			erlang:send_after(10,self(),timeout)
	end,
	butil:ds_add(Name,self(),cfg),
	R = create_device(IP,Index),
	route1(false,R#dev{delay = {element(1,Delay)*1000,element(2,Delay)*1000}, name = Name}).


% If delay is set, packets from both directions are buffered in lists.
% Keep track of time with timer. Join frames into 10ms buffers.
% Theoretically 10ms at least. Erlang timers are not that exact so it may be skewed.
% Unpredictable delay is fine. Networks are unpredictable anyway.
route1(Time,#dev{in_reversed = []} = R) when R#dev.in_unreversed /= [] ->
	route1(Time,R#dev{in_reversed = lists:reverse(R#dev.in_unreversed), in_unreversed = []});
route1(Time,#dev{out_reversed = []} = R) when R#dev.out_unreversed /= [] ->
	route1(Time,R#dev{out_reversed = lists:reverse(R#dev.out_unreversed), out_unreversed = []});
route1({_,_,_} = Now, R) ->
	case R of
		#dev{in_reversed = [{Time,L}|_]} ->
			Diff = timer:now_diff(Now,Time),
			case Diff >= (element(1,R#dev.delay) + random:uniform(element(2,R#dev.delay))) of
				true ->
					% lager:info("Sending! in rev ~p",[Diff]),
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
					% lager:info("Sending! out rev ~p",[Diff2]),
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
	NR = R#dev{out_reversed = OutRev, in_reversed = InRev},
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
		{tuntap, _Dev, Data} ->
			% lager:info("Received tuntap ~p ~p~n",[R#dev.peers,pkt:decode(Data)]),
			case R#dev.online of
				true when R#dev.delay /= {0,0} ->
					% lager:info("Buffering tuntap ~p",[length(R#dev.out_buf)]),
					route1(false,R#dev{out_buf = [Data|R#dev.out_buf]});
				% true when R#dev.delay /= {0,0} ->
				% 	route1(false,R#dev{out_unreversed = [{os:timestamp(),Data}|R#dev.out_unreversed]});
				true ->
					% lager:info("No buffering out"),
					[P ! Data || P <- R#dev.peers],
					route2(R);
				false ->
					route2(R)
			end;
		<<_/binary>> = Bin ->
			case R#dev.online of
				true when R#dev.delay /= {0,0} ->
					% lager:info("Buffering input ~p",[length(R#dev.in_buf)]),
					route1(false,R#dev{in_buf = [Bin|R#dev.in_buf]});
				% true when R#dev.delay /= {0,0} ->
				% 	route1(false,R#dev{in_unreversed = [{os:timestamp(),Bin}|R#dev.in_unreversed]});
				true ->
					% lager:info("No buffering in"),
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
					InUnrev = R#dev.in_unreversed;
				_ ->
					% lager:info("Timeout inbuf ~p",[length(R#dev.in_buf)]),
					InUnrev = [{Now,lists:reverse(R#dev.in_buf)}|R#dev.in_unreversed]
			end,
			case R#dev.out_buf of
				[] ->
					OutUnrev = R#dev.out_unreversed;
				_ ->
					% lager:info("Timeout outbuf ~p",[length(R#dev.out_buf)]),
					OutUnrev = [{Now,lists:reverse(R#dev.out_buf)}|R#dev.out_unreversed]
			end,
			route1(Now,R#dev{out_buf = [], in_buf = [], 
						in_unreversed = InUnrev,
						out_unreversed = OutUnrev});
		{toggle,Bool} ->
			route1(false,R#dev{online = Bool});
		{set_delay,Delay} ->
			case R#dev.delay of
				{0,0} when Delay /= {0,0} ->
					erlang:send_after(10,self(),timeout);
				_ ->
					ok
			end,
			route1(false,R#dev{delay = Delay});
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
		{'DOWN',_Monitor,_,Pid,_Reason} ->
			cleanup(lists:delete(Pid,Pids))
	end.

create_device(IP,N) ->
	Nm = "tap"++integer_to_list(N),
	{ok, Dev} = tuncer:create(Nm, [tap, no_pi, {active, true}]),
	ok = tuncer:up(Dev, IP),
	#dev{dev = Dev, ip = IP, index = N}.


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