-module(detest).
% run for running detest embedded. dist erlang must be running.
% main for running detest as escript.
-export([main/1,run/1,run/2,run/3,ez/0]).
% API for test module
-export([add_node/1,add_node/2, stop_node/1, ip/1]).
-define(PATH,".detest").
-define(INF(F,Param),
	case butil:ds_val(quiet,etscfg) of 
	true ->
		ok;
	_ ->
		io:format("~p ~p: ~s~n",[time(),?MODULE,io_lib:fwrite(F,Param)])
	end).
-define(INF(F),?INF(F,[])).

ip(Distname) ->
	[_,IP] = string:tokens(butil:tolist(Distname),"@"),
	IP.

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
	% node might already be in cfg
	case butil:findobj(distname,DistName,butil:ds_val(nodes,etscfg)) of
		false ->
			Nodes = [P|butil:ds_val(nodes,etscfg)],
			butil:ds_add(nodes,Nodes,etscfg);
		_ ->
			butil:ds_val(nodes,etscfg)
	end,

	write_global_cfgs(),
	write_per_node_cfgs(),

	Pid = start_node(P),
	RunPids = butil:ds_val(runpids,etscfg),
	butil:ds_add(runpids,[Pid|RunPids],etscfg),

	ok = connect([P]),
	ok = wait_app([P],butil:ds_val(wait_for_app,etscfg),butil:ds_val(scriptload,etscfg)),
	DistName.

stop_node([_|_] = Nd) ->
	stop_node(distname(Nd));
stop_node(Nm) when is_atom(Nm) ->
	Nm ! stop,
	wait_pids([whereis(Nm)]).



main([]) ->
	io:format("Command: ./detest <options> yourscript.erl <script_params>~n"++
		"Options:~n"
		"-h    print help~n"++
		"-v    print stdout from nodes~n"++
		"-q    quiet~n");
main(["-h"]) ->
	main([]);
main(Param) ->
	ets:new(etscfg, [named_table,public,set,{read_concurrency,true}]),
	butil:ds_add(verbose,lists:member("-v",Param),etscfg),
	butil:ds_add(quiet,lists:member("-q",Param),etscfg),
	butil:ds_add(stopby,halt,etscfg),
	butil:ds_add(basepath,?PATH,etscfg),
	butil:ds_add(dostdin,true,etscfg),
	case script_param(Param) of
		{ScriptNm,ScriptArg} ->
			ok;
		_ ->
			ScriptNm = ScriptArg = undefined,
			io:format("Invalid params~n"),
			halt(1)
	end,
	[] = os:cmd(epmd_path() ++ " -daemon"),
	application:ensure_all_started(lager),
	case os:type() of
		{unix,linux} ->
			case os:cmd("sudo whoami") of
				"root"++_ ->
					application:start(damocles);
				_ ->
					ok
			end;
		_ ->
			ok
	end,
	case compile:file(ScriptNm,[binary,return_errors,{parse_transform, lager_transform}]) of
		{ok,Mod,Bin} ->
			ScriptLoad = {Mod,Bin,filename:basename(ScriptNm)},
			code:load_binary(Mod, filename:basename(ScriptNm), Bin);
		Err ->
			ScriptLoad = Mod = undefined,
			?INF("Unable to compile: ~p",[Err]),
			halt(1)
	end,
	{ok, _} = net_kernel:start(['detest@127.0.0.1', longnames]),
	erlang:set_cookie(node(),'detest'),

	run(Mod,ScriptArg,ScriptLoad).

stop(Reason) ->
	case butil:ds_val(stopby,etscfg) of
		halt ->
			halt(1);
		_ ->
			throw(Reason)
	end.

% Run will execute until Mod:run concludes and erlang nodes have stopped.
% Mod - test module atom
% ScriptArg - list of script arguments
% Opts - [{basepath,".detest"},{verbose,true},{quiet,true},..]
run(Mod) ->
	run(Mod,[]).
run(Mod,ScriptArg) ->
	run(Mod,ScriptArg, []).
run(Mod,ScriptArg,[{_,_}|_] = Opts) ->
	case ets:info(etscfg,size) of
		undefined ->
			ets:new(etscfg, [named_table,public,set,{read_concurrency,true}]);
		_ ->
			ets:delete_all_objects(etscfg)
	end,
	[butil:ds_add(K,V,etscfg) || {K,V} <- Opts],
	case  butil:ds_val(basepath,etscfg) of
		undefined ->
			butil:ds_add(basepath,?PATH,etscfg);
		_ ->
			ok
	end,
	run(Mod,ScriptArg,code:get_object_code(Mod));
run(Mod,ScriptArg,{Mod,_ModBin,_ModFilename} = ScriptLoad) ->
	Cfg = apply(Mod,cfg,[ScriptArg]),
	GlobCfgs = butil:ds_val(global_cfg,Cfg,[]),
	NodeCfgs = butil:ds_val(per_node_cfg,Cfg,[]),
	Nodes1 = butil:ds_val(nodes,Cfg),
	Nodes = [[{distname,distname(Nd)}|Nd] || Nd <- Nodes1],

	butil:ds_add(global_cfg,GlobCfgs,etscfg),
	butil:ds_add(per_node_cfg,NodeCfgs,etscfg),
	butil:ds_add(nodes,Nodes,etscfg),
	butil:ds_add(cmd,butil:ds_val(cmd,Cfg,""),etscfg),
	butil:ds_add(wait_for_app,butil:ds_val(wait_for_app,Cfg),etscfg),
	butil:ds_add(scriptload,ScriptLoad,etscfg),
	butil:ds_add(erlcmd,butil:ds_val(erlcmd,GlobCfgs,"erl"),etscfg),
	butil:ds_add(erlenv,butil:ds_val(erlenv,GlobCfgs,[]),etscfg),
	butil:ds_add(connect_timeout,butil:ds_val(connect_timeout,Cfg,10000),etscfg),
	butil:ds_add(app_wait_timeout,butil:ds_val(app_wait_timeout,Cfg,30000),etscfg),

	os:cmd("rm -rf "++butil:ds_val(basepath,etscfg)),

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

	DistNames = [{butil:ds_val(name,Nd),butil:ds_val(distname,Nd)} || Nd <- Nodes],
	ScriptParam = DistNames++[{path,butil:ds_val(basepath,etscfg)},{args,ScriptArg},{damocles,butil:is_app_running(damocles)}],

	case catch apply(Mod,setup,[ScriptParam]) of
		{'EXIT',Err0} ->
			?INF("Setup failed ~p",[Err0]),
			stop({error,setup,Err0});
		_ ->
			ok
	end,
	
	% spawn nodes
	RunPids = [start_node(Nd,Cfg) || Nd <- Nodes],
	butil:ds_add(runpids,RunPids,etscfg),
		
	case connect(Nodes) of
		{error,Node} ->
			Pid1 = Pid2 = undefined,
			?INF("Unable to connect to ~p",[Node]);
		ok ->
			timer:sleep(500),
			case wait_app(Nodes,butil:ds_val(wait_for_app,Cfg),ScriptLoad) of
				{error,Node} ->
					Pid1 = Pid2 = undefined,
					?INF("Timeout waiting for app to start on ~p",[Node]);
				ok ->
					{Pid1,_} = spawn_monitor(fun() -> runproc(Mod,ScriptParam) end),
					{Pid2,_} = spawn_monitor(fun() -> stdinproc() end)
			end
	end,
	wait_done(Pid1,Pid2),
	do_stop(Mod,Cfg, ScriptParam).

wait_done(Pid1,Pid2) ->
	case is_pid(Pid1) andalso is_pid(Pid2) of
		true ->
			receive
				% no matter which process exits, kill both
				{'DOWN',_Ref,_,Pid,_} when Pid == Pid1; Pid == Pid2 ->
					exit(Pid1,stop),
					exit(Pid2,stop)
			end;
		false ->
			ok
	end.

prompt_continue() ->
	case io:get_line("Enter c to continue: ") of
		"c"++_ ->
			ok;
		"C"++_ ->
			ok;
		_A ->
			prompt_continue()
	end.

stdinproc() ->
	case butil:ds_val(dostdin,etscfg) of
		true ->
			case io:get_line("Enter q to quit test: ") of
				"q"++_ ->
					ok;
				"Q"++_ ->
					ok;
				_A ->
					stdinproc()
			end;
		_ ->
			timer:sleep(infinity)
	end.

runproc(Mod,ScriptParam) ->
	register(runproc,self()),
	case catch apply(Mod,run,[ScriptParam]) of
		{'EXIT',Err1} ->
			?INF("Test failed ~p",[Err1]);
		_ ->
			?INF("Test finished.")
	end.

script_param(["-"++_N|T]) ->
	script_param(T);
script_param([N|T]) ->
	case filename:extension(N) of
		".erl" ->
			{N,T};
		_ ->
			script_param(T)
	end;
script_param([]) ->
	false.

do_stop(Mod,Cfg,ScriptParam) ->
	RunPids = butil:ds_val(runpids,etscfg),
	{StopMod,StopFunc,StopArg} = butil:ds_val(stop,Cfg,{init,stop,[]}),
	% Nodelist may have changed, read it from ets
	Nodes = butil:ds_val(nodes,etscfg),
	timer:sleep(800),
	Pids = [spawn(fun() -> rpc:call(distname(Nd),StopMod,StopFunc,StopArg,10000) end) || Nd <- Nodes],
	wait_pids(Pids),
	% [butil:safesend(NetPid,stop) || NetPid <- NetPids],
	application:stop(damocles),
	% If nodes still alive, kill them forcefully
	[RP ! stop || RP <- RunPids],
	timer:sleep(200),
	% wait_pids(NetPids),
	apply(Mod,cleanup,[ScriptParam]),
	ok.

start_node(Nd) ->
	start_node(Nd,etscfg).
start_node(Nd,GlobCfg) ->
	start_node(Nd,butil:ds_val(cmd,GlobCfg,""),butil:ds_val(erlcmd,GlobCfg,"erl"),butil:ds_val(erlenv,GlobCfg,[])).
start_node(Nd,GlobCmd,ErlCmd1,ErlEnv1) ->
	AppPth = lists:flatten([butil:ds_val(basepath,etscfg),"/",ndnm(Nd),"/etc/app.config"]),
	RunCmd = butil:ds_val(cmd,Nd,GlobCmd),
	ErlCmd = butil:ds_val(erlcmd,Nd,ErlCmd1),
	ErlEnv = butil:ds_val(erlenv,Nd,ErlEnv1),
	case filelib:is_regular(AppPth) of
		true ->
			AppCmd = " -config "++filename:absname(AppPth);
		false ->
			AppCmd = ""
	end,
	VmArgs = lists:flatten([butil:ds_val(basepath,etscfg),"/",ndnm(Nd),"/etc/vm.args"]),
	case filelib:is_regular(VmArgs) of
		true ->
			VmCmd = " -args_file "++filename:absname(VmArgs);
		false ->
			VmCmd = ""
	end,

	%Ebins = [filename:absname(Nm) || Nm <- ["ebin"|filelib:wildcard("deps/*/ebin")]]," "),
	Ebins = string:join(["ebin"|filelib:wildcard("deps/*/ebin")]," "),
	Cmd = ErlCmd++" -noshell -noinput -setcookie "++butil:tolist(erlang:get_cookie())++" -name "++atom_to_list(butil:ds_val(distname,Nd))++
			" -pa "++Ebins++" "++AppCmd++VmCmd++" "++RunCmd,
	case butil:ds_val(extrun,Nd) of
		true ->
			?INF("extrun set for node. Run it manually now. Command detest would use (in current folder): ~n~p",[Cmd]),
			prompt_continue(),
			runerl(butil:ds_val(distname,Nd),undefined,[]);
		_ ->
			timer:sleep(300),
			runerl(butil:ds_val(distname,Nd),Cmd,ErlEnv)
	end.

write_global_cfgs() ->
	write_global_cfgs(butil:ds_val(nodes,etscfg),butil:ds_val(global_cfg,etscfg)).
write_global_cfgs(Nodes,GlobCfgs) ->
	[begin
		filelib:ensure_dir([butil:ds_val(basepath,etscfg),"/",ndnm(Nd),"/etc"]),
		[begin
			FBin = render_cfg(G,[{nodes,Nodes}]),
			Nm = [butil:ds_val(basepath,etscfg),"/",ndnm(Nd),"/etc/",filename:basename(dtlnm(G))],
			filelib:ensure_dir(Nm),
			ok = file:write_file(Nm,FBin)
		end || G <- GlobCfgs]
	 end || Nd <- Nodes].

write_per_node_cfgs() ->
	write_per_node_cfgs(butil:ds_val(nodes,etscfg),butil:ds_val(per_node_cfg,etscfg)).
write_per_node_cfgs(Nodes,NodeCfgs) ->
	[begin
		[begin
			FBin = render_cfg(NC,Nd),
			Nm = [butil:ds_val(basepath,etscfg),"/",ndnm(Nd),"/etc/",filename:basename(dtlnm(NC))],
			filelib:ensure_dir(Nm),
			ok = file:write_file(Nm,FBin)
		end || NC <- NodeCfgs]
	end || Nd <- Nodes].

wait_app(L,App,ScriptLoad) ->
	wait_app(L,App,ScriptLoad,os:timestamp(),butil:ds_val(app_wait_timeout,etscfg)).
wait_app([H|T],App,ScriptLoad,Started,Timeout) ->
	Node = butil:ds_val(distname,H),
	case timer:now_diff(os:timestamp(),Started) > Timeout*1000 of
		true ->
			{error,H};
		_ ->
			case rpc:call(Node,application,which_applications,[],1000) of
				[_|_] = L ->
					case App == undefined orelse lists:keymember(App,1,L) of
						true ->
							load_modules(Node,ScriptLoad),
							wait_app(T,App,ScriptLoad,Started,Timeout);
						false ->
							wait_app([H|T],App,ScriptLoad,Started,Timeout)
					end;
				_ ->
					wait_app([H|T],App,ScriptLoad,Started,Timeout)
			end
	end;
wait_app([],_,_,_,_) ->
	ok.


runerl(Name,Cmd,ErlEnv) when is_atom(Name) ->
	?INF("Running ~s",[Cmd]),
	spawn(fun() ->
		register(Name,self()),
		case Cmd of
			[_|_] ->
				Port = open_port({spawn,Cmd},[exit_status,use_stdio,binary,stream,{env,ErlEnv}]),
				{os_pid,OsPid} = erlang:port_info(Port,os_pid);
			_ ->
				Port = undefined,
				OsPid = undefined
		end,
		runerl(Port,OsPid)
	end).
runerl(Port,OsPid) ->
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
			runerl(Port,OsPid);
		stop when is_integer(OsPid) ->
			os:cmd("kill "++integer_to_list(OsPid));
		stop ->
			ok
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
	connect(L,os:timestamp(),butil:ds_val(connect_timeout,etscfg)).
connect([H|T],Start,Timeout) ->
	case timer:now_diff(os:timestamp(),Start) > Timeout*1000 of
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
							connect([H|T],Start,Timeout);
						pong ->
							connect(T,os:timestamp(),Timeout)
					end;
				false ->
					timer:sleep(100),
					connect([H|T],Start,Timeout)
			end
	end;
connect([],_,_) ->
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
	case apply(modnm(FN),render,[[{basepath,butil:ds_val(basepath,etscfg)}|P]++Param]) of
		{ok,Bin} ->
			Bin;
		Err ->
			?INF("Error rendering ~p~nParam:~p~nError:~p",[FN,P,Err]),
			stop({error,cfg_render,FN,Err})
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
	case butil:ds_val(N,etscfg) of
		undefined ->
			HaveDamocles = butil:is_app_running(damocles),
			case os:type() of
				{unix,linux} when HaveDamocles ->
					IP = butil:to_ip(detest_net:find_free_ip()),
					damocles:add_interface(IP),
				 	Nm = list_to_atom(butil:tolist(N)++"@"++IP);
				_ ->
					Nm = list_to_atom(hd(string:tokens(butil:tolist(N),"@"))++"@127.0.0.1")
			end,
			butil:ds_add(N,Nm,etscfg),
			Nm;
		Nm ->
			Nm
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
				stop({error,compile,Cfg,Err})
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
          stop({error,noepmd});
        GlobalEpmd ->
          GlobalEpmd
      end;
    Epmd ->
      Epmd
  end.

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