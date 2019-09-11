-module(detest_net).
-include("detest.hrl").
-export([start/0, node_bw/2, node_latency/2, node_online/2]).

-define(INTERVAL, 3).
-define(SAMPLES_PER_SEC, (1000 / ?INTERVAL)).

start() ->
	spawn(fun() -> run() end).

node_bw(Node,Bw) ->
	call({node_bw, butil:toatom(Node), Bw}).
node_latency(Node,Latency) ->
	call({node_latency, butil:toatom(Node), Latency}).
node_online(Node,Bool) ->
	call({node_online, butil:toatom(Node), Bool}).


tm() ->
	erlang:monotonic_time(millisecond).

call(Msg) ->
	Ref = make_ref(),
	?MODULE ! {call,self(), Ref, Msg},
	receive
		{Ref,Resp} ->
			Resp
	end.

-record(pkt,{bin = <<>>, tm = 0}).
-record(nd,{rpc_port, dist_port, offset = 0, online = true}).
-record(sock,{other, src_node, dst_node, bw = 1024 * 1024, recv_int_bytes = 0, int_budget = 1024, latency = 0, queue}).
-record(lsock,{accept_ref, src_node, dst_node, dst_port, port, blocked = false}).
-record(dp,{sockets = #{}, nodes = #{}, bytes = 0}).

run() ->
	register(?MODULE,self()),
	erlang:send_after(?INTERVAL, self(), timeout),
	Nodes = nodes_split(?CFG(nodes)),
	run(#dp{nodes = Nodes,
		sockets = create_listeners(maps:to_list(Nodes),maps:to_list(Nodes),#{})}).
run(P) ->
	receive
		{call, Src, Ref, Msg} ->
			Src ! {Ref, ok},
			run(modify(P,Msg));
		timeout ->
			% ?INF("Transfered ~p",[P#dp.bytes]),
			erlang:send_after(?INTERVAL, self(), timeout),
			run(P#dp{sockets = socket_activate(maps:to_list(P#dp.sockets), P#dp.sockets)});
		{tcp,Socket,Bin} ->
			#sock{queue = Q} = SI = maps:get(Socket, P#dp.sockets),
			Now = tm(),
			NQ = queue:in(#pkt{bin = Bin, tm = Now}, Q),
			IntBytes = SI#sock.recv_int_bytes + byte_size(Bin),
			run(P#dp{bytes = P#dp.bytes + byte_size(Bin), 
				sockets = (P#dp.sockets)#{Socket => SI#sock{queue = NQ, recv_int_bytes = IntBytes}}});
		{tcp_closed,Socket} ->
			case maps:get(Socket, P#dp.sockets,undefined) of
				undefined ->
					run(P);
				#sock{other = Other} ->
					gen_tcp:close(Other),
					run(P#dp{sockets = maps:remove(Other,maps:remove(Socket,P#dp.sockets))})
			end;
		{tcp_passive,Socket} ->
			#sock{int_budget = Budget, recv_int_bytes = Bytes} = maps:get(Socket, P#dp.sockets),
			case Budget > Bytes of
				true ->
					inet:setopts(Socket,[{active,once}]),
					run(P);
				false ->
					run(P)
			end;
		{inet_async, LSock, _Ref, {ok, Sock}} ->
			% ?INF("inet_async ~p",[LSock]),
			run(accept(P,LSock, Sock));
		{inet_async, LSock, _Ref, Error} ->
			?INF("async accept error ~p",[Error]),
			run(accept(P,LSock, undefined));
		Other ->
			?INF("detest_net unknown_msg ~p",[Other]),
			run(P)
	end.

modify(P,{_What, Node, _Bw} = Op) ->
	#{Node := #nd{online = NodeOnline} = NI} = P#dp.nodes,
	case Op of
		{node_online, Node, Bool} when Bool == NodeOnline ->
			P;
		_ ->
			case Op of
				{node_online, _, true} ->
					P#dp{nodes = (P#dp.nodes)#{Node => NI#nd{online = true}}, 
						sockets = create_listeners([{Node,NI}],maps:to_list(P#dp.nodes),P#dp.sockets)};
				{node_online, _, false} ->
					Out = P#dp{sockets = mod_socks(P#dp.nodes, maps:to_list(P#dp.sockets), Op, P#dp.sockets)},
					Out#dp{nodes = (P#dp.nodes)#{Node => NI#nd{online = false}}};
				_ ->
					P#dp{sockets = mod_socks(P#dp.nodes, maps:to_list(P#dp.sockets), Op, P#dp.sockets)}
			end
	end.

mod_socks(Nodes,[{S,SI}|T], {node_bw, Node, Bw} = Op, M) when SI#sock.src_node == Node; SI#sock.dst_node == Node ->
	mod_socks(Nodes,T, Op, M#{S => sock_bw(SI,Bw)});
mod_socks(Nodes,[{S,SI}|T], {node_latency, Node, Lat} = Op, M) when SI#sock.src_node == Node; SI#sock.dst_node == Node ->
	mod_socks(Nodes,T, Op, M#{S => SI#sock{latency = Lat}});
mod_socks(Nodes,[{S,SI}|T], {node_online, Node, Bool} = Op, M) when SI#sock.src_node == Node; SI#sock.dst_node == Node ->
	case Bool of
		false ->
			gen_tcp:close(S),
			gen_tcp:close(SI#sock.other),
			mod_socks(Nodes,T, Op, maps:delete(S,maps:delete(SI#sock.other,M)));
		true ->
			mod_socks(Nodes,T, Op, M)
	end;
mod_socks(Nodes,[{S,SI}|T], {node_online, Node, Bool} = Op, M) when SI#lsock.src_node == Node; SI#lsock.dst_node == Node ->
	case Bool of
		false ->
			gen_tcp:close(S),
			mod_socks(Nodes,T, Op, maps:delete(S,M));
		true ->
			mod_socks(Nodes,T, Op, M)
	end;
mod_socks(Nodes,[_|T], Op, M) ->
	mod_socks(Nodes,T, Op, M);
mod_socks(_,[],_, M) ->
	M.


flush_q(Sock, #sock{other = Other} = SI) ->
	case queue:out(SI#sock.queue) of
		{empty,_} ->
			SI;
		{{value,Oldest},NQ} ->
			Now = tm(),
			case Oldest#pkt.tm + SI#sock.latency < Now of
				true ->
					gen_tcp:send(Other, Oldest#pkt.bin),
					flush_q(Sock, SI#sock{queue = NQ});
				false ->
					SI
			end
	end.

socket_activate([{S,#sock{recv_int_bytes = Recv, int_budget = Budget} = SI}|T], M) ->
	case ok of
		_ when Recv < Budget ->
			NRecv = 0,
			inet:setopts(S,[{active,once}]);
		_ ->
			NRecv = Recv - Budget
	end,
	socket_activate(T, M#{S => flush_q(S,SI#sock{recv_int_bytes = NRecv})});
socket_activate([_|T], M) ->
	socket_activate(T, M);
socket_activate([], M) ->
	M.

create_listeners([{NdName,Nd}|T],Nodes,M) ->
	create_listeners(T,Nodes,create_listeners(NdName, Nd, Nodes, M));
create_listeners([],_,M) ->
	M.
create_listeners(NdName,Nd,[{OtherNd,Other}|T], M) -> % when NdName /= OtherNd
	L1 = lsock(NdName, Other#nd.rpc_port + Nd#nd.offset, Other#nd.rpc_port, OtherNd),
	L2 = lsock(NdName, Other#nd.dist_port + Nd#nd.offset, Other#nd.dist_port, OtherNd),
	create_listeners(NdName, Nd, T, put_all([L1,L2],M));
% create_listeners(A,B,[_|T],M) ->
% 	create_listeners(A,B,T,M);
create_listeners(_,_,[],M) ->
	M.

lsock(Nd,Port,DstPort, DstNode) ->
	Opts = [binary, {packet, 0}, {reuseaddr, true},
            {keepalive, true}, {active, false}],
	{ok,LSock} = gen_tcp:listen(Port, Opts),
	{ok, Ref} = prim_inet:async_accept(LSock, -1),
	{LSock,#lsock{accept_ref = Ref, src_node = Nd, dst_node = DstNode, dst_port = DstPort, port = Port}}.

accept(P, LSock, Sock) ->
	case is_port(Sock) of
		true ->
			case set_sockopt(LSock, Sock) of
				ok ->
					gen_tcp:controlling_process(Sock, self());
				{error, _Reason} ->
					?INF("async accept fail ~p",[_Reason]),
					ok
			end;
		false ->
			ok
	end,
	case prim_inet:async_accept(LSock, -1) of
		{ok, NewRef} ->
			ok;
		{error, NewRef} ->
			?INF("async accept error"),
			ok
	end,
	LInfo = maps:get(LSock, P#dp.sockets),
	Opts = [{keepalive,true},binary,{active,false},{nodelay,true},{sndbuf,1024*4},{recbuf,1024*4}],
	case gen_tcp:connect({127,0,0,1},LInfo#lsock.dst_port,Opts) of
		{ok,Other} ->
			BW = ?CFG(internode_bw),
			DF = sock_bw(#sock{queue = queue:new()}, BW),
			OI = DF#sock{other = Sock, dst_node = LInfo#lsock.src_node, src_node = LInfo#lsock.dst_node},
			SI = DF#sock{other = Other,src_node = LInfo#lsock.src_node, dst_node = LInfo#lsock.dst_node},
			Updates = [{LSock,LInfo#lsock{accept_ref = NewRef}},
				{Other, OI},
				{Sock, SI}];
		_ ->
			gen_tcp:close(Sock),
			Updates = [{LSock,LInfo#lsock{accept_ref = NewRef}}]
	end,
	P#dp{sockets = put_all(Updates, P#dp.sockets)}.

sock_bw(SI,BW) ->
	SI#sock{bw = BW, int_budget = erlang:round(BW / ?SAMPLES_PER_SEC)}.


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
	% ?INF("Nodes ~p",[Nodes]),
    maps:from_list([begin
        {butil:toatom(butil:ds_val(name,Nd)), 
		#nd{rpc_port = butil:ds_val(rpcport,Nd,0),
		dist_port = butil:ds_val(dist_port,Nd),
		offset = butil:ds_val(connect_offset,Nd,0)}}
    end || Nd <- Nodes]).