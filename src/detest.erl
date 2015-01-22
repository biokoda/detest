-module(detest).
-export([main/1,ez/0]).
-define(PATH,".detest").
-define(INF(F,Param),io:format("~p ~p:~p ~s~n",[time(),?MODULE,?LINE,io_lib:fwrite(F,Param)])).
-define(INF(F),?INF(F,[])).
ez() ->
	Ebin = filelib:wildcard("ebin/*"),
	Deps = filelib:wildcard("deps/*/ebin/*"),
	Files = 
	[begin
		{ok,Bin} = file:read_file(Fn),
		{filename:basename(Fn),Bin}
	end || Fn <- Ebin++Deps],
	{ok,{_,Bin}} = zip:create("detest.ez",Files,[memory]),
	file:write_file("detest.ez",Bin).

main([]) ->
	?INF("Missing run script parameter.");
main([ScriptNm]) ->
	[] = os:cmd(epmd_path() ++ " -daemon"),
	case compile:file(ScriptNm,[binary,return_errors]) of
		{ok,Mod,Bin} ->
			code:load_binary(Mod, filename:basename(ScriptNm), Bin);
		Err ->
			Mod = undefined,
			?INF("Unable to compile: ~p",[Err]),
			halt(1)
	end,
	Cfg = apply(Mod,cfg,[]),
	GlobCfgs = proplists:get_value(global_cfg,Cfg),
	NodeCfgs = proplists:get_value(per_node_cfg,Cfg),
	Nodes = proplists:get_value(nodes,Cfg),
	Path = ?PATH,
	% Path = proplists:get_value(path,Cfg,"test/run"),
	% script path without name of script
	% LowestPath = filename:join(lists:reverse(tl(lists:reverse(filename:split(filename:absname(escript:script_name())))))),
	% case lists:prefix(LowestPath, filename:absname(Path)) of
	% 	true ->
	% 		ok;
	% 	false ->
	% 		io:format("Test path must not be higher than path of executable~n"),
	% 		halt(1)
	% end,
	os:cmd("rm -rf "++Path),
	
	compile_cfgs(GlobCfgs++NodeCfgs),

	% create files in etc
	[begin
		filelib:ensure_dir([Path,"/",ndnm(Nd),"/etc"]),
		[begin
			FBin = render_cfg(G,[{nodes,Nodes}]),
			Nm = [Path,"/",ndnm(Nd),"/etc/",filename:basename(dtlnm(G))],
			filelib:ensure_dir(Nm),
			ok = file:write_file(Nm,FBin)
		end || G <- GlobCfgs]
	 end || Nd <- Nodes],

	[begin
		[begin
			FBin = render_cfg(NC,Nd),
			Nm = [Path,"/",ndnm(Nd),"/etc/",filename:basename(dtlnm(NC))],
			filelib:ensure_dir(Nm),
			ok = file:write_file(Nm,FBin)
		end || NC <- NodeCfgs]
	end || Nd <- Nodes],

	setup_dist(),	

	case catch apply(Mod,setup,[?PATH]) of
		{'EXIT',Err0} ->
			?INF("Setup failed ~p",[Err0]),
			halt(1);
		_ ->
			ok
	end,
	
	% spawn nodes
	RunPids = 
	[begin
		RunCmd = proplists:get_value(cmd,Nd,""),
		AppPth = lists:flatten([Path,"/",ndnm(Nd),"/etc/app.config"]),
		case filelib:is_regular(AppPth) of
			true ->
				AppCmd = " -config "++filename:absname(AppPth);
			false ->
				AppCmd = ""
		end,
		VmArgs = lists:flatten([Path,"/",ndnm(Nd),"/etc/vm.args"]),
		case filelib:is_regular(VmArgs) of
			true ->
				VmCmd = " -args_file "++filename:absname(VmArgs);
			false ->
				VmCmd = ""
		end,

		Ebins = string:join([filename:absname(Nm) || Nm <- ["ebin"|filelib:wildcard("deps/*/ebin")]]," "),
		Cmd = "erl -noshell -noinput -pa "++Ebins++" "++AppCmd++VmCmd++" "++RunCmd,
		spawn(fun() -> run(Cmd) end)
	end || Nd <- Nodes],
	
	case connect(Nodes,0) of
		{error,Node} ->
			?INF("Unable to connect to ~p",[Node]);
		ok ->
			% Give it time to start up
			timer:sleep(5000),
			case catch apply(Mod,run,[?PATH]) of
				{'EXIT',Err1} ->
					?INF("Test failed ~p",[Err1]);
				_ ->
					?INF("Test finished")
			end
	end,

	% [Pid ! stop || Pid <- RunPids],
	Pids = [spawn(fun() -> rpc:call(proplists:get_value(distname,Nd),init,stop,[]) end) || Nd <- Nodes],
	wait_pids(Pids),
	apply(Mod,cleanup,[?PATH]).


run(Cmd) ->
	% receive
	% 	stop ->
	% 		exit(normal)
	% 	after 0 ->
	% 		ok
	% end,
	?INF("Running ~p",[Cmd]),
	_X = os:cmd(Cmd),
	% io:format("Node finished. ~p~n",[X]).
	ok.

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

connect([H|T],N) when N < 100 ->
	Node = proplists:get_value(distname,H,""),
	case net_kernel:hidden_connect(Node) of
		true ->
			case net_adm:ping(Node) of
        		pang ->
        			timer:sleep(100),
					connect([H|T],N+1);
				_ ->
					connect(T,0)
			end;
		false ->
			timer:sleep(100),
			connect([H|T],N+1)
	end;
connect([H|_],_N) ->
	{error,H};
connect([],_) ->
	ok.

render_cfg(Cfg,P) ->
	case Cfg of
		{FN,Param} ->
			ok;
		FN ->
			Param = []
	end,
	case apply(modnm(FN),render,[[{basepath,?PATH}|P]++Param]) of
		{ok,Bin} ->
			case string:str(FN,"vm.args") of
				0 ->
					ok;
				_ ->
					setup_dist(iolist_to_binary(Bin))
			end,
			Bin;
		Err ->
			?INF("Error rendering ~p~nParam:~p~nError:~p",[FN,P,Err]),
			halt(1)
	end.

modnm(FN) ->
	list_to_atom(filename:basename(FN)).

ndnm([{_,_}|_] = N) ->
	ndnm(proplists:get_value(distname,N,""));
ndnm(N) ->
	NS = atom_to_list(N),
	[NS1|_] = string:tokens(NS,"@"),
	NS1.

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

setup_dist() ->
	setup_dist(<<>>).
setup_dist(VmBin) ->
	case whereis(net_kernel) of
		undefined when VmBin /= <<>> ->
		  	Lines = string:tokens(binary_to_list(VmBin),"\r\n"),
			parse_args(Lines,[]);
		undefined ->
			{ok, _} = net_kernel:start([setname("a@127.0.0.1"), longnames]);
		_ ->
			ok
	end.

parse_args([" "++Rem|T],L) ->
	parse_args([Rem|T],L);
parse_args(["#"++_|T],L) ->
	parse_args(T,L);
parse_args(["-name " ++ Namestr|T],L) ->
	Curname = rem_spaces(Namestr),
	Myname = setname(Curname),
	% io:format("Name ~p ~p~n",[Myname,Namestr]),
	case node() == 'nonode@nohost' of
	    true ->
	    	{ok, _} = net_kernel:start([Myname, longnames]);
	    _ ->
			ok
	end,
	parse_args(T,[{node,list_to_atom(Curname)},{myname,Myname}|L]);
parse_args(["-sname " ++ Namestr|T],L) ->
	Curname = rem_spaces(Namestr),
	Myname = setname(Curname),
	case node() == 'nonode@nohost' of
		true ->
	    	{ok, _} = net_kernel:start([Myname, shortnames]);
	    _ ->
	      	ok
	end,
	parse_args(T,[{node,list_to_atom(Curname)},{myname,Myname}|L]);
parse_args(["-setcookie "++N|T],L) ->
	erlang:set_cookie(node(), list_to_atom(rem_spaces(N))),
	parse_args(T,L);
parse_args([_|T],L) ->
  parse_args(T,L);
parse_args([],L) ->
	L.

setname(Namestr) ->
	{MS,S,MiS} = now(),
	Nm = integer_to_list(MS*1000000000000 + S*1000000 + MiS),
	case string:tokens(rem_spaces(Namestr),"@") of
		[_Name,Addr] ->
			list_to_atom(Nm++"@"++Addr);
		[_Name] ->
			list_to_atom(Nm)
	end.

rem_spaces(Str) ->
	lists:filter(fun(X) -> X /= $\s end,Str).

epmd_path() ->
  ErtsBinDir = filename:dirname(element(2,file:get_cwd())),
  Name = "epmd",
  case os:find_executable(Name, ErtsBinDir) of
    false ->
      case os:find_executable(Name) of
        false ->
          io:format("Could not find epmd.~n"),
          halt(1);
        GlobalEpmd ->
          GlobalEpmd
      end;
    Epmd ->
      Epmd
  end.