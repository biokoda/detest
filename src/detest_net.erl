-module(detest_net).
-include("detest.hrl").
-export([start/0, add_node/1,node_bw/2, node_latency/2, 
	node_online/2, isolation_group_remove/1, isolation_group_set/2]).
-export([reg_caller/0, unreg_caller/0, majority_node/0,
	shape_traffic_rand/0,shape_traffic_stop/0, shape_traffic_set/1, shape_traffic_get/0]).
-export([call/1]).
-define(INTERVAL, 3).
-define(SAMPLES_PER_SEC, (1000 / ?INTERVAL)).

start() ->
	detest_shaper:run(),
	spawn(fun() -> run() end).

node_bw(Node,Bw) ->
	call({node_bw, butil:toatom(Node), Bw}).
node_latency(Node,Latency) ->
	call({node_latency, butil:toatom(Node), Latency}).

% This disables any automatic shaping
node_online(Node,Bool) ->
	case Bool of
		true ->
			detest_shaper:call({isolation_group_remove,Node});
		false ->
			detest_shaper:call({isolation_group_set,Node})
	end,
	call({node_online, butil:toatom(Node), Bool}).
% This disables any automatic shaping
isolation_group_set(Nodes1,Id) ->
	Nodes = [butil:toatom(Node) || Node <- Nodes1],
	detest_shaper:call({isolation_group_set,Nodes,Id}),
	call({group_set, Nodes, Id}).
% This disables any automatic shaping
isolation_group_remove(Id) ->
	detest_shaper:call({isolation_group_remove,Id}),
	call({group_remove, Id}).
% Should not be called client side
add_node(Node) ->
	detest_shaper:cast({add_node,Node}),
	call({add_node, butil:toatom(Node)}).

% Registers self for executing requests
reg_caller() ->
	detest_shaper:call(reg_caller).
% Must unreg whenever there is a delay in executing requests or complete stop from caller
unreg_caller() ->
	detest_shaper:call(unreg_caller).
% Get a node that is safe to call and request must succeed.
% If network in flux, will block.
majority_node() ->
	detest_shaper:call(majority_node).
% Change network conditions every 1-20s.
% Always keeps a majority of nodes online.
% Randomly sets latency between 0 - 1s for online nodes
% Can take node offline by shutting down, set very high latency or isolates network.
shape_traffic_rand() ->
	detest_shaper:call(shape_traffic_rand).
shape_traffic_get() ->
	detest_shaper:call(shape_traffic_get).
shape_traffic_set(Mods) ->
	detest_shaper:call({shape_traffic_set,Mods}).
shape_traffic_stop() ->
	detest_shaper:call(shape_traffic_stop).



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
-record(nd,{rpc_port, dist_port, offset = 0, online = true, groups = [], initialized = false}).
-record(sock,{src_node, dst_node, other, bw = 1024 * 1024, recv_int_bytes = 0, int_budget = 1024, latency = 0, queue}).
-record(lsock,{src_node, dst_node, accept_ref, dst_port, port, blocked = false}).
-record(dp,{sockets = #{}, nodes = #{}, bytes = 0}).

run() ->
	register(?MODULE,self()),
	erlang:send_after(?INTERVAL, self(), timeout),
	Nodes = nodes_parse(),
	Nodes1 = [{NdNm, Nd#nd{initialized = true}} || {NdNm,Nd} <- maps:to_list(Nodes) ],
	run(#dp{nodes = maps:from_list(Nodes1),
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
			case maps:get(Socket, P#dp.sockets,undefined) of
				undefined ->
					run(P);
				#sock{queue = Q} = SI ->
					Now = tm(),
					NQ = queue:in(#pkt{bin = Bin, tm = Now}, Q),
					IntBytes = SI#sock.recv_int_bytes + byte_size(Bin),
					run(P#dp{bytes = P#dp.bytes + byte_size(Bin), 
				sockets = (P#dp.sockets)#{Socket => SI#sock{queue = NQ, recv_int_bytes = IntBytes}}})
			end;
		{tcp_closed,Socket} ->
			case maps:get(Socket, P#dp.sockets,undefined) of
				undefined ->
					run(P);
				#sock{other = Other} ->
					gen_tcp:close(Other),
					run(P#dp{sockets = maps:remove(Other,maps:remove(Socket,P#dp.sockets))})
			end;
		{tcp_passive,Socket} ->
			case maps:get(Socket, P#dp.sockets,undefined) of
				undefined ->
					run(P);
				#sock{int_budget = Budget, recv_int_bytes = Bytes} ->
					case Budget > Bytes of
						true ->
							inet:setopts(Socket,[{active,once}]),
							run(P);
						false ->
							run(P)
					end
			end;
		{inet_async, LSock, _Ref, {ok, Sock}} ->
			% ?INF("inet_async ~p",[LSock]),
			run(accept(P,LSock, Sock));
		{inet_async, _LSock, _Ref, _Error} ->
			?INF("async accept error ignore ~p",[_Error]),
			run(P);
		Other ->
			?INF("detest_net unknown_msg ~p",[Other]),
			run(P)
	end.

modify(P,{group_set, Nodes, Id}) ->
	ModNodes = [{NdNm,NI#nd{groups = butil:lists_add(Id, NI#nd.groups)}} || 
		{NdNm,NI} <- maps:to_list(P#dp.nodes), lists:member(NdNm,Nodes) andalso lists:member(Id,NI#nd.groups) == false],
	?INF("group_set ~p ~p ~p",[ModNodes,Nodes,P#dp.nodes ]),
	case ModNodes of
		[] ->
			P;
		_ ->
			NP = P#dp{nodes = put_all(ModNodes,P#dp.nodes)},
			Op = {group_set, Nodes},
			NP#dp{sockets = mod_socks(NP#dp.nodes, maps:to_list(NP#dp.sockets), Op, NP#dp.sockets)}
	end;
modify(P,{group_remove, Id}) ->
	ModNodes = [{NdNm,NI#nd{groups = lists:delete(Id, NI#nd.groups)}} || 
		{NdNm,NI} <- maps:to_list(P#dp.nodes), lists:member(Id,NI#nd.groups)],
	case ModNodes of
		[] ->
			P;
		_ ->
			Op = {group_remove, Id},
			NSocks = mod_socks(P#dp.nodes, maps:to_list(P#dp.sockets), Op, P#dp.sockets),
			UpdatedNodes = put_all(ModNodes,P#dp.nodes),
			P#dp{nodes = UpdatedNodes,
				sockets = create_listeners(maps:to_list(UpdatedNodes),maps:to_list(UpdatedNodes),NSocks)}
	end;
modify(P,{add_node, Node}) ->
	case maps:get(Node,P#dp.nodes, undefined) of
		undefined ->
			#{Node := NI} = nodes_parse(),
			Nodes = maps:put(Node, NI, P#dp.nodes),
			P#dp{nodes = Nodes, 
				sockets = create_listeners([{Node,NI}],maps:to_list(Nodes),P#dp.sockets)};
		_ ->
			P
	end;
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

src_node(#sock{src_node = Src}) ->
	Src;
src_node(#lsock{src_node = Src}) ->
	Src.
dst_node(#sock{dst_node = Dst}) ->
	Dst;
dst_node(#lsock{dst_node = Dst}) ->
	Dst.

mod_socks(Nodes,[{S,SI}|T], {group_remove, GrpId} = Op, M) ->
	SrcNd = src_node(SI),
	DstNd = dst_node(SI),
	SrcNI = maps:get(SrcNd, Nodes),
	DstNI = maps:get(DstNd, Nodes),
	MemberSrc = lists:member(GrpId, SrcNI#nd.groups),
	MemberDst = lists:member(GrpId, DstNI#nd.groups),
	case MemberSrc orelse MemberDst of
		false ->
			mod_socks(Nodes,T, Op, M);
		true ->
			MemberSrc1 = lists:delete(GrpId, SrcNI#nd.groups),
			MemberDst1 = lists:delete(GrpId, DstNI#nd.groups),
			NoCommon = (SrcNI#nd.groups -- DstNI#nd.groups) == SrcNI#nd.groups,
			case ok of
				_ when MemberSrc1 == [], MemberDst1 == [] ->
					mod_socks(Nodes,T, Op, M);
				_ when NoCommon ->
					mod_socks(Nodes,T, Op, sock_close(S, SI, M));
				_ ->
					mod_socks(Nodes,T, Op, M)
			end
	end;
mod_socks(Nodes,[{S,SI}|T], {group_set, GNodes} = Op, M) ->
	?INF("group_set ~p",[GNodes]),
	SrcNd = src_node(SI),
	DstNd = dst_node(SI),
	MemberSrc = lists:member(SrcNd, GNodes),
	MemberDst = lists:member(DstNd, GNodes),
	case MemberSrc orelse MemberDst of
		true when MemberSrc andalso MemberDst ->
			mod_socks(Nodes,T, Op, M);
		true ->
			SrcNI = maps:get(SrcNd, Nodes),
			DstNI = maps:get(DstNd, Nodes),
			case (SrcNI#nd.groups -- DstNI#nd.groups) /= SrcNI#nd.groups of
				% they have a common group
				true -> 
					mod_socks(Nodes,T, Op, M);
				false ->
					mod_socks(Nodes,T, Op, sock_close(S, SI, M))
			end;
		false ->
			mod_socks(Nodes,T, Op, M)
	end;
mod_socks(Nodes,[{S,SI}|T], {node_bw, Node, Bw} = Op, M) when SI#sock.src_node == Node; SI#sock.dst_node == Node ->
	mod_socks(Nodes,T, Op, M#{S => sock_bw(SI,Bw)});
mod_socks(Nodes,[{S,SI}|T], {node_latency, Node, Lat} = Op, M) when SI#sock.src_node == Node; SI#sock.dst_node == Node ->
	mod_socks(Nodes,T, Op, M#{S => SI#sock{latency = Lat}});
mod_socks(Nodes,[{S,SI}|T], {node_online, Node, Bool} = Op, M) when SI#sock.src_node == Node; SI#sock.dst_node == Node ->
	case Bool of
		false when SI#sock.src_node /= SI#sock.dst_node ->
			mod_socks(Nodes,T, Op, M);
		false ->
			mod_socks(Nodes,T, Op, sock_close(S, SI, M));
		true ->
			mod_socks(Nodes,T, Op, M)
	end;
mod_socks(Nodes,[{S,SI}|T], {node_online, Node, Bool} = Op, M) when SI#lsock.src_node == Node; SI#lsock.dst_node == Node ->
	case Bool of
		false when SI#lsock.src_node /= SI#lsock.dst_node ->
			mod_socks(Nodes,T, Op, M);
		false ->
			mod_socks(Nodes,T, Op, sock_close(S,SI,M));
		true ->
			mod_socks(Nodes,T, Op, M)
	end;
mod_socks(Nodes,[_|T], Op, M) ->
	mod_socks(Nodes,T, Op, M);
mod_socks(_,[],_, M) ->
	M.

sock_close(S, SI, M) when element(1,SI) == sock ->
	gen_tcp:close(S),
	gen_tcp:close(SI#sock.other),
	maps:remove(S,maps:remove(SI#sock.other,M));
sock_close(S, SI, M) when element(1,SI) == lsock ->
	?INF("sock_close ~p",[SI]),
	gen_tcp:close(S),
	maps:remove(S,M).

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
		_ when Recv =< Budget ->
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

create_listeners([{_NdName,#nd{online = false}}|T],Nodes,M) ->
	create_listeners(T,Nodes,M);
create_listeners([{NdName,Nd}|T],Nodes,M) ->
	create_listeners(T,Nodes,create_listeners(NdName, Nd, Nodes, M));
create_listeners([],_,M) ->
	M.
create_listeners(NdName,Nd,[{OtherNd,Other}|T], M) -> % when NdName /= OtherNd
	Groups = Nd#nd.groups,
	OtherGroups = Other#nd.groups,
	?INF("create listeners ~p ~p ~p ~p",[NdName, OtherNd, Groups, OtherGroups]),
	case ok of
		_ when Groups == [], OtherGroups == [] ->
			Doit = true;
		_ ->
			% If this this node and other node have a common group
			Doit = (Groups -- OtherGroups) /= Groups
	end,
	case Doit of
		true when NdName == OtherNd, Nd#nd.initialized ->
			Changes = [];
		true ->
			L1 = lsock(NdName, Other#nd.rpc_port + Nd#nd.offset, Other#nd.rpc_port, OtherNd),
			L2 = lsock(NdName, Other#nd.dist_port + Nd#nd.offset, Other#nd.dist_port, OtherNd),
			Changes = L1++L2;
		false ->
			Changes = []
	end,
	create_listeners(NdName, Nd, T, put_all(Changes,M));
% create_listeners(A,B,[_|T],M) ->
% 	create_listeners(A,B,T,M);
create_listeners(_,_,[],M) ->
	M.

lsock(Nd,Port,DstPort, DstNode) ->
	Opts = [binary, {packet, 0}, {reuseaddr, true},
            {keepalive, true}, {active, false}],
	case gen_tcp:listen(Port, Opts) of
		{ok,LSock} ->
			?INF("lsock create from=~p  to=~p, port=~p",[Nd,DstNode,Port]),
			{ok, Ref} = prim_inet:async_accept(LSock, -1),
			[{LSock,#lsock{accept_ref = Ref, src_node = Nd, dst_node = DstNode, dst_port = DstPort, port = Port}}];
		_ ->
			[]
	end.

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
	case maps:get(LSock, P#dp.sockets, undefined) of
		undefined ->
			P;
		LInfo ->
			Opts = [{keepalive,true},binary,{active,false},{nodelay,true},{sndbuf,1024*4},{recbuf,1024*4}],
			case gen_tcp:connect({127,0,0,1},LInfo#lsock.dst_port,Opts) of
				{ok,Other} ->
					BW = ?CFG(internode_bw),
					DF = sock_bw(#sock{queue = queue:new()}, BW),
					?INF("Accepted sock from=~p to=~p",[LInfo#lsock.src_node, LInfo#lsock.dst_node]),
					OI = DF#sock{other = Sock, dst_node = LInfo#lsock.src_node, src_node = LInfo#lsock.dst_node},
					SI = DF#sock{other = Other,src_node = LInfo#lsock.src_node, dst_node = LInfo#lsock.dst_node},
					Updates = [{LSock,LInfo#lsock{accept_ref = NewRef}},
						{Other, OI},
						{Sock, SI}];
				_ ->
					gen_tcp:close(Sock),
					Updates = [{LSock,LInfo#lsock{accept_ref = NewRef}}]
			end,
			P#dp{sockets = put_all(Updates, P#dp.sockets)}
	end.

sock_bw(SI,BW) ->
	SI#sock{bw = BW, int_budget = erlang:round(BW / ?SAMPLES_PER_SEC)}.


put_all([{K,H}|T],M) ->
	put_all(T,maps:put(K,H,M));
put_all([],M) ->
	M.

set_sockopt(LSocket, Socket) ->
	case inet_db:lookup_socket(LSocket) of
		{error,E} ->
			gen_tcp:close(Socket),
			{error,E};
		{ok, Mod} ->
    		true = inet_db:register_socket(Socket, Mod),
    		case prim_inet:getopts(LSocket, [active, nodelay, keepalive, delay_send, priority, tos]) of
        		{ok, Opts} ->
            		case prim_inet:setopts(Socket, Opts) of
                		ok -> ok;
                		Error -> gen_tcp:close(Socket), Error
            		end;
        		Error ->
            		gen_tcp:close(Socket), Error
			end
    end.

nodes_parse() ->
	Nodes = ?CFG(nodes),
	% ?INF("Nodes ~p",[Nodes]),
    maps:from_list([begin
		Node = butil:toatom(butil:ds_val(distname,Nd)),
		detest_shaper:cast({add_node,Node}),
        {Node, readnd(Nd)}
    end || Nd <- Nodes]).

readnd(Nd) ->
	#nd{rpc_port = butil:ds_val(rpcport,Nd,0),
		dist_port = butil:ds_val(dist_port,Nd),
		offset = butil:ds_val(connect_offset,Nd,0)}.

