-module(detest).
-export([main/1]).
-define(PATH,".detest").

main([]) ->
	io:format("Missing run script parameter.~n");
main([ScriptNm]) ->
	% [code:add_path(Dep) || Dep <- filelib:wildcard("deps/*/ebin")],
	% code:add_path("ebin"),
	[] = os:cmd(epmd_path() ++ " -daemon"),
	case compile:file(ScriptNm,[binary,return_errors]) of
		{ok,Mod,Bin} ->
			code:load_binary(Mod, filename:basename(ScriptNm), Bin);
		Err ->
			Mod = undefined,
			io:format("Unable to compile: ~p~n",[Err]),
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

	% spawn nodes
	[begin
		Cmd = proplists:get_value(cmd,Nd,""),
		AppPth = lists:flatten([Path,"/",ndnm(Nd),"/etc/app.config"]),
		case filelib:is_regular(AppPth) of
			true ->
				AppCmd = " -config "++AppPth;
			false ->
				AppCmd = ""
		end,
		VmArgs = lists:flatten([Path,"/",ndnm(Nd),"/etc/vm.args"]),
		case filelib:is_regular(VmArgs) of
			true ->
				VmCmd = " -args_file "++VmArgs;
			false ->
				VmCmd = ""
		end,
		% spawn(fun() -> os:cmd("erl -pa ebin deps/*/ebin "++AppCmd++VmCmd++" "++Cmd) end)
		io:format("erl -pa ebin deps/*/ebin "++AppCmd++VmCmd++" "++Cmd++"~n")
	end || Nd <- Nodes],
	halt(1),

	connect(Nodes,0),

	case catch apply(Mod,run,[]) of
		{'EXIT',Err1} ->
			io:format("Test crashed ~p~n",[Err1]);
		_ ->
			ok
	end,
	[spawn(fun() -> rpc:call(proplists:get_value(distname,Nd),init,stop,[]) end) || Nd <- Nodes],
	timer:sleep(200).


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
connect([H|_],N) ->
	io:format("Timeout connect to ~p~n",[H]),
	halt(1);
connect([],_) ->
	ok.

render_cfg(Cfg,P) ->
	case Cfg of
		{FN,Param} ->
			ok;
		FN ->
			Param = []
	end,
	io:format("Render ~p~n",[P++Param]),
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
			io:format("Error rendering ~p~nParam:~p~nError:~p~n",[FN,P,Err]),
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
				io:format("Error compiling ~p~n~p~n",[dtlnm(Cfg),Err]),
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
	io:format("Name ~p ~p~n",[Myname,Namestr]),
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
  ErtsBinDir = filename:dirname(escript:script_name()),
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