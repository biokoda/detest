-module(detest_net).
-include("detest.hrl").
-export([start/0]).

-define(INTERVAL, 50).
-define(SAMPLES_PER_SEC, 1000 / ?INTERVAL).
-define(PKT_SIZE,1500).

start() ->
	spawn(fun() -> run() end).

-record(nd,{rpc_port, dist_port, offset = 0}).
-record(sock,{other, bw = 1024 * 1024, latency = 0, queue}).
-record(lsock,{accept_ref, src_node, dst_port}).
-record(dp,{sockets = #{}, nodes = #{}}).

run() ->
	erlang:send_after(?INTERVAL, self(), timeout),
	Nodes = nodes_split(butil:ds_val(nodes,etscfg)),
	run(#dp{nodes = Nodes,
		sockets = create_listeners(Nodes,Nodes,#{})}).
run(P) ->
	receive
		timeout ->
			erlang:send_after(?INTERVAL, self(), timeout),
			socket_activate(maps:to_list(P#dp.sockets)),
			run(P);
		{tcp,Socket,Bin} ->
			#sock{other = Other} = maps:get(Socket, P#dp.sockets),
			gen_tcp:send(Other, Bin),
			run(P);
		{tcp_closed,Socket} ->
			case maps:get(Socket, P#dp.sockets,undefined) of
				undefined ->
					run(P);
				#sock{other = Other} ->
					gen_tcp:close(Other),
					run(P#dp{sockets = maps:remove(Other,maps:remove(Socket,P#dp.sockets))})
			end;
		{tcp_passive,_S} ->
			run(P);
		{inet_async, LSock, _Ref, {ok, Sock}} ->
			run(accept(P,LSock, Sock));
		{inet_async, LSock, _Ref, Error} ->
			?INF("async accept error ~p",[Error]),
			run(accept(P,LSock, undefined))
	end.

socket_activate([{S,#sock{bw = Bw}}|T]) ->
	NPackets = erlang:round(Bw / ?PKT_SIZE / ?SAMPLES_PER_SEC),
	inet:setopts(S,[{active,NPackets}]),
	socket_activate(T);
socket_activate([_|T]) ->
	socket_activate(T);
socket_activate([]) ->
	ok.

create_listeners([{NdName,Nd}|T],Nodes,M) ->
	create_listeners(T,Nodes,create_listeners(NdName, Nd, Nodes, M));
create_listeners([],_,M) ->
	M.
create_listeners(NdName,Nd,[{OtherNd,Other}|T], M) when NdName /= OtherNd ->
	L1 = lsock(NdName, Other#nd.rpc_port + Nd#nd.offset, Other#nd.rpc_port),
	L2 = lsock(NdName, Other#nd.dist_port + Nd#nd.offset, Other#nd.dist_port),
	create_listeners(NdName, Nd, T, put_all([L1,L2],M));
create_listeners(A,B,[_|T],M) ->
	create_listeners(A,B,T,M);
create_listeners(_,_,[],M) ->
	M.

lsock(Nd,Port,DstPort) ->
	Opts = [binary, {packet, 0}, {reuseaddr, true},
            {keepalive, true}, {active, false}],
	{ok,LSock} = gen_tcp:listen(Port, Opts),
	{ok, Ref} = prim_inet:async_accept(LSock, -1),
	{LSock,#lsock{accept_ref = Ref, src_node = Nd, dst_port = DstPort}}.

accept(P, LSock, Sock) ->
	case is_port(Sock) of
		true ->
			case set_sockopt(LSock, Sock) of
				ok ->
					gen_tcp:controlling_process(Sock, self());
				{error, _Reason} ->
					ok
			end;
		false ->
			ok
	end,
	case prim_inet:async_accept(LSock, -1) of
		{ok, NewRef} ->
			ok;
		{error, NewRef} ->
			ok
	end,
	LInfo = maps:get(LSock, P#dp.sockets),
	{ok,Other} = gen_tcp:connect({127,0,0,1},LInfo#lsock.dst_port,[{keepalive,true},binary,{active,false},{nodelay,true},{sndbuf,1024*4},{recbuf,1024*4}]),
	Updates = [{LSock,LInfo#lsock{accept_ref = NewRef}},
		{Other, #sock{other = Sock}},
		{Sock, #sock{other = Other}}],
	P#dp{sockets = put_all(Updates, P#dp.sockets)}.

put_all([{K,H}|T],M) ->
	put_all(T,maps:put(K,H,M));
put_all([],M) ->
	M.

set_sockopt(LSocket, Socket) ->
    {ok, Mod} = inet_db:lookup_socket(LSocket),
    true = inet_db:register_socket(Socket, Mod),
    case prim_inet:getopts(LSocket, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(Socket, Opts) of
                ok -> ok;
                Error -> gen_tcp:close(Socket), Error
            end;
        Error ->
            gen_tcp:close(Socket), Error
    end.

nodes_split(Nodes) ->
    maps:from_list([begin
        {butil:toatom(butil:ds_val(name,Nd)), 
		#nd{rpc_port = butil:ds_val(rpcport,Nd,0),
		dist_port = butil:ds_val(dist_port,Nd),
		offset = butil:ds_val(connect_offset,Nd,0)}}
    end || Nd <- Nodes]).