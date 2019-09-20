-module(detest_shaper).
-include("detest.hrl").
-export([call/1,cast/1, run/0]).

run() ->
    spawn(fun() -> shaper() end).
cast(Msg) ->
    case whereis(?MODULE) of
		undefined = Pid ->
			exit(normal);
		Pid ->
			ok
	end,
    Pid ! {call,undefined, make_ref(), Msg},
    ok.
call(Msg) ->
	case whereis(?MODULE) of
		undefined = Pid ->
			exit(normal);
		Pid ->
			ok
	end,
	Ref = make_ref(),
	Pid ! {call,self(), Ref, Msg},
	receive
		{Ref,Resp} ->
			Resp
	end.

wait_runner() ->
	case whereis(detest_net) of
		undefined ->
			timer:sleep(20),
			wait_runner();
		Pid ->
			Pid
	end.

-record(sp,{runner, callers = [], nodes = {},
    node_mods = #{},
    isolated_nodes = [], 
    isolation_groups = [],
    transition = false, paused_callers = [], calls = [], 
    shaping = false, reshape_timer, shape_mod_fixed = #{}}).
shaper() ->
	register(?MODULE,self()),
	Run = wait_runner(),
    link(Run),
	erlang:monitor(process,Run),
    Me = self(),
    spawn(fun() ->
        erlang:monitor(process,Me),
        receive
            {'DOWN',_Ref,_,_Pid,normal} ->
                ok;
            {'DOWN',_Ref,_,_Pid,Reason} ->
                ?INF("shaper died ~p",[Reason]),
                ok
        end
    end),
	shaper(#sp{runner = Run}).
shaper(P) ->
	receive
		{'DOWN',_Ref,_,Pid,_} when Pid == P#sp.runner ->
			ok;
		{'DOWN',_Ref,_,Pid,_} ->
			shaper(shaper_call(P,{Pid,undefined},unreg_caller));
        reshape ->
            ?INF("Reshape timer"),
            case P#sp.shaping of
                true ->
                    shaper(transition(timer(P#sp{transition = true})));
                false ->
                    shaper(unwrap(reshape(P#sp{calls = [], reshape_timer = undefined}),P#sp.calls))
            end;
		{call, From, Ref, Msg} ->
			shaper(shaper_call(P,{From,Ref},Msg))
	end.

timer(P) ->
    P#sp{reshape_timer = erlang:send_after(1000 + rand:uniform(20000), self(), reshape)}.

shaper_call(P, {Pid,Ref}, shape_traffic_get) ->
    butil:safesend(Pid,{Ref,P#sp.node_mods}),
    P;
shaper_call(P, {Pid,Ref}, {shape_traffic_set,Mods}) ->
    shaper_call(P#sp{shape_mod_fixed = Mods},{Pid,Ref}, shape_traffic_start);
shaper_call(P, {Pid,Ref}, {isolation_group_remove,Id}) ->
    butil:safesend(Pid,{Ref,ok}),
    P#sp{isolation_groups = lists:delete(Id,P#sp.isolation_groups)};
shaper_call(P, {Pid,Ref}, {isolation_group_set,_Nodes,Id}) ->
    Grps = butil:lists_add(Id, P#sp.isolation_groups),
    case P#sp.isolation_groups of
        [] ->
            shaper_call(P#sp{isolation_groups = Grps},{Pid,Ref}, shape_traffic_stop);
        _ ->
            butil:safesend(Pid,{Ref,ok}),
            P#sp{isolation_groups = Grps}
    end;
shaper_call(P,{Pid,Ref}, shape_traffic_stop) ->
    case P#sp.shaping of
        false when P#sp.reshape_timer == undefined ->
            butil:safesend(Pid,{Ref,ok}),
            P;
        false ->
            erlang:cancel_timer(P#sp.reshape_timer),
            self() ! reshape,
            P#sp{reshape_timer = undefined, calls = [{{Pid,Ref}, shape_traffic_stop}|P#sp.calls]};
        true ->
            erlang:cancel_timer(P#sp.reshape_timer),
            self() ! reshape,
            P#sp{reshape_timer = undefined, shaping = false, calls = [{{Pid,Ref}, shape_traffic_stop}|P#sp.calls]}
    end;
shaper_call(P,{Pid,Ref}, shape_traffic_rand) ->
    shaper_call(P#sp{shape_mod_fixed = #{}},{Pid,Ref}, shape_traffic_start);
shaper_call(P,{Pid,Ref}, shape_traffic_start) ->
    butil:safesend(Pid,{Ref,ok}),
    case P#sp.shaping of
        true ->
            P;
        false ->
            ?INF("shape_traffic_start ~p ~p",[P#sp.callers, P#sp.paused_callers]),
            transition(timer(P#sp{shaping = true, transition = true}))
    end;
shaper_call(P,{Pid,Ref}, {add_node,Nd}) ->
	butil:safesend(Pid,{Ref,ok}),
	P#sp{nodes = list_to_tuple(butil:lists_add(Nd,tuple_to_list(P#sp.nodes)))};
shaper_call(#sp{transition = true} = P,{Pid,Ref}, majority_node) ->
    case lists:keytake(Pid, 1, P#sp.callers) of
        {value,Caller,NewCallers} ->
            transition(P#sp{callers = NewCallers, paused_callers = [Caller|P#sp.paused_callers], calls = [{{Pid,Ref},majority_node}|P#sp.calls]});
        _ ->
            butil:safesend(Pid,{Ref,undefined}),
            P
    end;
shaper_call(P,{Pid,Ref}, majority_node) ->
    case lists:keyfind(Pid, 1, P#sp.callers) of
        false ->
            butil:safesend(Pid,{Ref,undefined}),
            P;
        _ ->
            butil:safesend(Pid,{Ref, randnd(P)}),
            P
    end;
shaper_call(#sp{transition = true} = P,{Pid,Ref}, reg_caller) ->
	case lists:keymember(Pid,1,P#sp.paused_callers) of
		false ->
            butil:safesend(Pid,{Ref,ok}),
			MonRef = erlang:monitor(process,Pid),
			P#sp{paused_callers = [{Pid,MonRef}|P#sp.paused_callers]};
		true ->
			P
	end;
shaper_call(#sp{transition = false} = P,{Pid,Ref}, reg_caller) ->
	butil:safesend(Pid,{Ref,ok}),
	case lists:keymember(Pid,1,P#sp.callers) of
		false ->
			MonRef = erlang:monitor(process,Pid),
			P#sp{callers = [{Pid,MonRef}|P#sp.callers]};
		true ->
			P
	end;
shaper_call(P,{Pid,Ref}, unreg_caller) ->
	butil:safesend(Pid,{Ref,ok}),
	case lists:keyfind(Pid,1,P#sp.callers) of
		false ->
			case lists:keyfind(Pid,1,P#sp.paused_callers) of
				false ->
					P;
				{_,MonRef} ->
					erlang:demonitor(MonRef,[flush]),
					P#sp{paused_callers = lists:keydelete(Pid,1,P#sp.paused_callers)}
			end;
		{_,MonRef} ->
            ?INF("Unreg caller ~p",[Pid]),
			erlang:demonitor(MonRef,[flush]),
			transition(P#sp{callers = lists:keydelete(Pid,1,P#sp.callers)})
	end.

randnd(P) ->
    element(rand:uniform(tuple_size(P#sp.nodes)), P#sp.nodes).

transition(#sp{transition = true, callers = [], calls = Calls} = P) ->
	unwrap(reshape(P#sp{transition = false, callers = P#sp.paused_callers, calls = []}), Calls);
transition(P) ->
	P.

reshape(#sp{shaping = true} = P) ->
    % Remove and set new modifications
    node_cleanup(maps:to_list(P#sp.node_mods)),
    case maps:size(P#sp.shape_mod_fixed) of
        0 ->
            Nodes = tuple_to_list(P#sp.nodes),
            NewMods = set_offline(Nodes,set_latencies(Nodes,#{}));
        NewMods ->
            ok
    end,
    node_set(maps:to_list(NewMods)),
    {OnlineNodes,IsolatedNodes} = split_offline(tuple_to_list(P#sp.nodes), [], [], NewMods),
    ?INF("Network shape: ~p, non-majority:~p",[NewMods,IsolatedNodes]),
    P#sp{node_mods = NewMods, nodes = list_to_tuple(OnlineNodes), isolated_nodes = IsolatedNodes};
reshape(P) ->
    node_cleanup(maps:to_list(P#sp.node_mods)),
    P#sp{node_mods = #{}}.

split_offline([Node|T],Online,Offline,Mods) ->
    case maps:get(Node,Mods,undefined) of
        undefined ->
            split_offline(T,[Node|Online],Offline,Mods);
        Obj ->
            case is_offline(Obj) of
                true ->
                    split_offline(T,Online,[Node|Offline],Mods);
                false ->
                    split_offline(T,[Node|Online],Offline,Mods)
            end
    end;
split_offline([],Online,Offline,_) ->
    {Online,Offline}.

set_latencies([Node|T],Obj) ->
    ObjMods = maps:get(Node,Obj,[]),
    Lat = rand:uniform(1000),
    % detest_net:call({node_latency, Node, Lat}),
    set_latencies(T,Obj#{Node => [{latency, Lat}|ObjMods]});
set_latencies([],Obj) ->
    Obj.

set_offline(Nodes,Obj) ->
    set_offline(length(Nodes) div 2, Nodes, [], Obj).
set_offline(Max,_, Offlines, Obj) when Max == length(Offlines) ->
    Obj;
set_offline(Max,[Node|T], Offlines, Obj) ->
    case rand:uniform(2) of
        1 ->
            set_offline(Max,T, Offlines, Obj);
        2 ->
            ObjMods = maps:get(Node,Obj,[]),
            OfflineType = offline_type(Node),
            case OfflineType of
                {latency,X} ->
                    set_offline(Max, T, [Node|Offlines], Obj#{Node => [{latency,X}|lists:keydelete(latency,1,ObjMods)]});
                _ ->
                    set_offline(Max, T, [Node|Offlines], Obj#{Node => [OfflineType|ObjMods]})
            end
    end;
set_offline(_Max,[],_,Obj) ->
    Obj.

is_offline(Mods) ->
    lists:member(offline,Mods) orelse lists:member(stop,Mods) orelse lists:member({latency,20000},Mods).

offline_type(_Node) ->
    case rand:uniform(3) of
        1 ->
            % detest_node:call({node_online, Node, false}),
            offline;
        2 ->
            % detest_net:call({node_latency, Node, 20000}),
            {latency,20000};
        3 ->
            % detest:stop_node(Node),
            stop
    end.

node_set([{Node,Mods}|T]) ->
    [detest_net:call({node_latency, Node, Lat}) || {latency,Lat} <- Mods],
    [detest_net:call({node_online, Node, false}) || offline <- Mods],
    [detest:stop_node(Node) || stop <- Mods],
    node_set(T);
node_set([]) ->
    ok.

node_cleanup([{Node,Mods}|T]) ->
    [detest_net:call({node_latency, Node, 0}) || {latency,_} <- Mods],
    [detest_net:call({node_online, Node, true}) || offline <- Mods],
    [begin
        NodeInfo = butil:findobj(distname,Node,?CFG(nodes)),
        detest:add_node(NodeInfo)
    end || stop <- Mods],
    node_cleanup(T);
node_cleanup([]) ->
    ok.

unwrap(P,[{{Pid,Ref},Msg}|T]) ->
    unwrap(shaper_call(P,{Pid,Ref}, Msg), T);
unwrap(P,[]) ->
    P.
