-module(detest).
-export([main/1,ez/0]).
% API for test module
-export([add_node/1,add_node/2, stop_node/1]).
-define(PATH,".detest").
-define(INF(F,Param),
	case butil:ds_val(quiet,etscfg) of 
	true ->
		ok;
	_ ->
		io:format("~p ~p: ~s~n",[time(),?MODULE,io_lib:fwrite(F,Param)])
	end).
-define(INF(F),?INF(F,[])).

ez() ->
	Ebin = filelib:wildcard("ebin/*"),
	Deps = filelib:wildcard("deps/*/ebin/*"),
	Files = 
	[begin
		{ok,Bin} = file:read_file(Fn),
		{filename:basename(Fn),Bin}
	end || Fn <- Ebin++Deps], 
	%["procket","procket.so"]
	{ok,{_,Bin}} = zip:create("detest.ez",Files,[memory]),
	file:write_file("detest.ez",Bin).

add_node(P1) ->
	add_node(P1,[]).
add_node(P1,NewCfg) ->
	DistName = distname(P1),
	% update configs if we received them
	GlobCfgs = butil:ds_val(global_cfg,NewCfg,butil:ds_val(global_cfg,etscfg)),
	NodeCfgs = butil:ds_val(per_node_cfg,NewCfg,butil:ds_val(per_node_cfg,etscfg)),
	butil:ds_add(global_cfg,GlobCfgs,etscfg),
	butil:ds_add(per_node_cfg,NodeCfgs,etscfg),
	% add node to nodes
	P = [{distname, DistName}|P1],
	case butil:findobj(distname,DistName,butil:ds_val(nodes,etscfg)) of
		false ->
			Nodes = butil:ds_val(nodes,etscfg);
		_ ->
			Nodes = [P|butil:ds_val(nodes,etscfg)],
			butil:ds_add(nodes,Nodes,etscfg)
	end,

	write_global_cfgs(),
	write_per_node_cfgs(),

	Pid = start_node(P),
	RunPids = butil:ds_val(runpids,etscfg),
	butil:ds_add(runpids,[Pid|RunPids],etscfg),
	ok = connect(Nodes),
	ok = wait_app(Nodes,butil:ds_val(wait_for,etscfg),butil:ds_val(scriptload,etscfg)),
	DistName.

stop_node([_|_] = Nd) ->
	stop_node(distname(Nd));
stop_node(Nm) when is_atom(Nm) ->
	Nm ! stop,
	wait_pids([whereis(Nm)]).

main([]) ->
	io:format("Command: ./detest <options> yourscript.erl~n"++
		"Options:~n"
		"-h    print help~n"++
		"-v    print stdout from nodes~n"++
		"-q    quiet~n");
main(["-h"]) ->
	main([]);
main(Param) ->
	[ScriptNm|_] = lists:reverse(Param),
	ets:new(etscfg, [named_table,public,set,{read_concurrency,true}]),
	case lists:member("-v",Param) of
		true ->
			butil:ds_add(verbose,true,etscfg);
		_ ->
			ok
	end,
	case lists:member("-q",Param) of
		true ->
			butil:ds_add(quiet,true,etscfg);
		_ ->
			ok
	end,
	[] = os:cmd(epmd_path() ++ " -daemon"),
	application:ensure_all_started(lager),

	case compile:file(ScriptNm,[binary,return_errors,{parse_transform, lager_transform}]) of
		{ok,Mod,Bin} ->
			ScriptLoad = {Mod,Bin,filename:basename(ScriptNm)},
			code:load_binary(Mod, filename:basename(ScriptNm), Bin);
		Err ->
			ScriptLoad = Mod = undefined,
			?INF("Unable to compile: ~p",[Err]),
			halt(1)
	end,
	Cfg = apply(Mod,cfg,[]),
	GlobCfgs = proplists:get_value(global_cfg,Cfg,[]),
	NodeCfgs = proplists:get_value(per_node_cfg,Cfg,[]),
	Nodes1 = proplists:get_value(nodes,Cfg),
	Nodes = [[{distname,distname(Nd)}|Nd] || Nd <- Nodes1],

	butil:ds_add(global_cfg,GlobCfgs,etscfg),
	butil:ds_add(per_node_cfg,NodeCfgs,etscfg),
	butil:ds_add(nodes,Nodes,etscfg),
	butil:ds_add(cmd,butil:ds_val(cmd,Cfg,""),etscfg),
	butil:ds_add(wait_for,butil:ds_val(wait_for,Cfg),etscfg),
	butil:ds_add(scriptload,ScriptLoad,etscfg),

	os:cmd("rm -rf "++?PATH),

	% case detest_net:start([{butil:ip_to_tuple(ndaddr(Nd)),
	% 						proplists:get_value(delay,Nd,{0,0}),
	% 						ndnm(Nd)} || Nd <- Nodes]) of
	% 	{error,NetPids} ->
	% 		DetestName = undefined,
	% 		?INF("Unable to setup interfaces. Run as sudo? Kill your vpn app?~nFailed:~p",[NetPids]),
	% 		halt(1);
	% 	{ok,NetPids,DetestName} ->
	% 		ok
	% end,
	% NetPids = [],
	
	compile_cfgs(GlobCfgs++NodeCfgs),

	% create files in etc
	write_global_cfgs(Nodes,GlobCfgs),
	write_per_node_cfgs(Nodes,NodeCfgs),

	{ok, _} = net_kernel:start(['detest@127.0.0.1', longnames]),
	erlang:set_cookie(node(),'detest'),

	case catch apply(Mod,setup,[?PATH]) of
		{'EXIT',Err0} ->
			?INF("Setup failed ~p",[Err0]),
			halt(1);
		_ ->
			ok
	end,
	
	% spawn nodes
	RunPids = [start_node(Nd,Cfg) || Nd <- Nodes],
	butil:ds_add(runpids,RunPids,etscfg),

	DistNames = [{butil:ds_val(name,Nd),butil:ds_val(distname,Nd)} || Nd <- Nodes],
	
	case connect(Nodes) of
		{error,Node} ->
			?INF("Unable to connect to ~p",[Node]);
		ok ->
			timer:sleep(500),
			case wait_app(Nodes,butil:ds_val(wait_for,Cfg),ScriptLoad) of
				{error,Node} ->
					?INF("Timeout waiting for app to start on ~p",[Node]);
				ok ->
					case catch apply(Mod,run,[DistNames,?PATH]) of
						{'EXIT',Err1} ->
							?INF("Test failed ~p",[Err1]);
						_ ->
							?INF("Test finished.")
					end
			end
	end,

	do_stop(Mod,Cfg).

do_stop(Mod,Cfg) ->
	RunPids = butil:ds_val(runpids,etscfg),
	{StopMod,StopFunc,StopArg} = butil:ds_val(stop,Cfg,{init,stop,[]}),
	% Nodelist may have changed, read it from ets
	Nodes = butil:ds_val(nodes,etscfg),
	timer:sleep(800),
	Pids = [spawn(fun() -> rpc:call(distname(Nd),StopMod,StopFunc,StopArg) end) || Nd <- Nodes],
	wait_pids(Pids),
	% [butil:safesend(NetPid,stop) || NetPid <- NetPids],
	% If nodes still alive, kill them forcefully
	[RP ! stop || RP <- RunPids],
	timer:sleep(200),
	% wait_pids(NetPids),
	apply(Mod,cleanup,[?PATH]).

start_node(Nd) ->
	start_node(Nd,butil:ds_val(cmd,etscfg,"")).
start_node(Nd,[{_,_}|_] = GlobCfg) ->
	start_node(Nd,butil:ds_val(cmd,GlobCfg,""));
start_node(Nd,GlobCmd) ->
	AppPth = lists:flatten([?PATH,"/",ndnm(Nd),"/etc/app.config"]),
	RunCmd = butil:ds_val(cmd,Nd,GlobCmd),
	io:format("Glob cmd ~p, runcmd ~p~n",[GlobCmd,RunCmd]),
	case filelib:is_regular(AppPth) of
		true ->
			AppCmd = " -config "++filename:absname(AppPth);
		false ->
			AppCmd = ""
	end,
	VmArgs = lists:flatten([?PATH,"/",ndnm(Nd),"/etc/vm.args"]),
	case filelib:is_regular(VmArgs) of
		true ->
			VmCmd = " -args_file "++filename:absname(VmArgs);
		false ->
			VmCmd = ""
	end,

	Ebins = string:join([filename:absname(Nm) || Nm <- ["ebin"|filelib:wildcard("deps/*/ebin")]]," "),
	Cmd = "erl -noshell -noinput -setcookie detest -name "++atom_to_list(butil:ds_val(distname,Nd))++
			" -pa "++Ebins++" "++AppCmd++VmCmd++RunCmd,
	timer:sleep(300),
	run(butil:ds_val(distname,Nd),Cmd).

write_global_cfgs() ->
	write_global_cfgs(butil:ds_val(nodes,etscfg),butil:ds_val(global_cfg,etscfg)).
write_global_cfgs(Nodes,GlobCfgs) ->
	[begin
		filelib:ensure_dir([?PATH,"/",ndnm(Nd),"/etc"]),
		[begin
			FBin = render_cfg(G,[{nodes,Nodes}]),
			Nm = [?PATH,"/",ndnm(Nd),"/etc/",filename:basename(dtlnm(G))],
			filelib:ensure_dir(Nm),
			ok = file:write_file(Nm,FBin)
		end || G <- GlobCfgs]
	 end || Nd <- Nodes].

write_per_node_cfgs() ->
	write_global_cfgs(butil:ds_val(nodes,etscfg),butil:ds_val(per_node_cfg,etscfg)).
write_per_node_cfgs(Nodes,NodeCfgs) ->
	[begin
		[begin
			FBin = render_cfg(NC,Nd),
			Nm = [?PATH,"/",ndnm(Nd),"/etc/",filename:basename(dtlnm(NC))],
			filelib:ensure_dir(Nm),
			ok = file:write_file(Nm,FBin)
		end || NC <- NodeCfgs]
	end || Nd <- Nodes].

wait_app(L,App,ScriptLoad) ->
	wait_app(L,App,ScriptLoad,os:timestamp()).
wait_app([H|T],App,ScriptLoad,Started) ->
	Node = butil:ds_val(distname,H),
	case timer:now_diff(os:timestamp(),Started) > 30*1000000 of
		true ->
			{error,H};
		_ ->
			case rpc:call(Node,application,which_applications,[],1000) of
				[_|_] = L ->
					case App == undefined orelse lists:keymember(App,1,L) of
						true ->
							load_modules(Node,ScriptLoad),
							wait_app(T,App,ScriptLoad,Started);
						false ->
							wait_app([H|T],App,ScriptLoad,Started)
					end;
				_ ->
					wait_app([H|T],App,ScriptLoad,Started)
			end
	end;
wait_app([],_,_,_) ->
	ok.

run(Name,[_|_] = Cmd) when is_atom(Name) ->
	?INF("Running ~s",[Cmd]),
	spawn(fun() ->
		register(Name,self()),
		Port = open_port({spawn,Cmd},[exit_status,use_stdio,binary,stream]),
		{os_pid,OsPid} = erlang:port_info(Port,os_pid),
		run(Port,OsPid)
	end);
run(Port,OsPid) ->
	receive
		{Port,{exit_status,_Status}} ->
			ok;
		{Port,{data,Bin}} ->
			case butil:ds_val(verbose,etscfg) of
				true ->
					io:format("~s",[Bin]);
				_ ->
					ok
			end,
			run(Port,OsPid);
		stop ->
			os:cmd("kill "++integer_to_list(OsPid))
	end.

wait_pids([undefined|T]) ->
	wait_pids(T);
wait_pids([H|T]) ->
	case erlang:is_process_alive(H) of
		true ->
			timer:sleep(100),
			wait_pids([H|T]);
		false ->
			wait_pids(T)
	end;
wait_pids([]) ->
	ok.

connect(L) ->
	connect(L,os:timestamp()).
connect([H|T],Start) ->
	case timer:now_diff(os:timestamp(),Start) > 10000000 of
		true ->
			{error,H};
		_ ->
			?INF("Connecting to ~p",[ndnm(H)]),
			Node = butil:ds_val(distname,H),
			case net_kernel:hidden_connect(Node) of
				true ->
					case net_adm:ping(Node) of
		        		pang ->
		        			timer:sleep(100),
							connect([H|T],Start);
						pong ->
							connect(T,os:timestamp())
					end;
				false ->
					timer:sleep(100),
					connect([H|T],Start)
			end
	end;
connect([],_) ->
	ok.

% Every node also has test and this module loaded
load_modules(Node,{Mod,Bin,Filename}) ->
	{module,Mod} = rpc:call(Node,code,load_binary,[Mod,Filename,Bin]),
	{_,ThisBin,ThisNm} = code:get_object_code(?MODULE),
	{module,?MODULE} = rpc:call(Node,code,load_binary,[?MODULE,ThisNm,ThisBin]).

render_cfg(Cfg,P) ->
	case Cfg of
		{FN,Param} ->
			ok;
		FN ->
			Param = []
	end,
	case apply(modnm(FN),render,[[{basepath,?PATH}|P]++Param]) of
		{ok,Bin} ->
			Bin;
		Err ->
			?INF("Error rendering ~p~nParam:~p~nError:~p",[FN,P,Err]),
			halt(1)
	end.

modnm(FN) ->
	list_to_atom(filename:basename(FN)).

ndnm([{_,_}|_] = N) ->
	ndnm(proplists:get_value(name,N,""));
ndnm(N) ->
	NS = atom_to_list(N),
	[NS1|_] = string:tokens(NS,"@"),
	NS1.

distname([{_,_}|_] = N) ->
	distname(proplists:get_value(name,N,""));
distname(N) ->
	case os:type() of
		% {unix,linux} ->
		% 	N;
		{_,_} ->
			[Nm|_] = string:tokens(butil:tolist(N),"@"),
			list_to_atom(Nm++"@127.0.0.1")
	end.

% ndaddr([{_,_}|_] = N) ->
% 	ndaddr(proplists:get_value(name,N,""));
% ndaddr(N) ->
% 	NS = atom_to_list(N),
% 	[_,Addr] = string:tokens(NS,"@"),
% 	Addr.

dtlnm(G) ->
	case G of
		{GNm,_} ->
			GNm;
		GNm ->
			GNm
	end.

compile_cfgs(L) ->
	[begin
		case erlydtl:compile_file(dtlnm(Cfg), list_to_atom(filename:basename(dtlnm(Cfg))),[{out_dir,false},return]) of
			ok ->
				ok;
			{ok,_} ->
				ok;
			{ok,_,_} ->
				ok;
			Err ->
				?INF("Error compiling ~p~n~p",[dtlnm(Cfg),Err]),
				halt(1)
		end
	end || Cfg <- L].


epmd_path() ->
  ErtsBinDir = filename:dirname(element(2,file:get_cwd())),
  Name = "epmd",
  case os:find_executable(Name, ErtsBinDir) of
    false ->
      case os:find_executable(Name) of
        false ->
          ?INF("Could not find epmd.~n"),
          halt(1);
        GlobalEpmd ->
          GlobalEpmd
      end;
    Epmd ->
      Epmd
  end.
