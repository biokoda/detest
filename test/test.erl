-module(test).
% mandatory detest functions
-export([cfg/1,run/1,setup/1,cleanup/1]).
% test functions
-export([call_start/1,call_receive/1]).
% assert macros
-include_lib("eunit/include/eunit.hrl").

-define(ND1,[{name,node1}]).
-define(ND2,[{name,node2}]).
-define(ND3,[{name,node3}]).


cfg(_TestArgs) ->
	% KV = [{Param1,Val1}],
	[
	 
	 %%  global_cfg is a list of config files for your nodes. They are template files (erlydtl).
	 %%  When rendering, they get nodes value as a parameter + whatever you add here.
	 % {global_cfg,[{"test/nodes.yaml",[{fixedvals,KV}]},"test/withoutparams.yaml"]},
	 
	 %%  Config files per node. For every node, its property list is added when rendering.
	 %%  if name contains app.config or vm.args it gets automatically added to run node command
	 %%  do not set cookie or name of node in vm.args this is set by detest
	 % {per_node_cfg,["test/app.config"]},
	 
	 %%  cmd is appended to erl execute command, it should execute your app.
	 %%  It can be set for every node individually. Add it to that list if you need it, it will override this value.
	 %%  You should just start your app with this command and not do anything else. Leave other code for run call.
	 {cmd,"-s lager"},

	 %%  which app to wait for to consider node started
	 {wait_for_app,lager},

	 %%  List of nodes that will be started initially. Additional nodes and this cfg can be change during test.
	 %%  It must be a property list. It must have {name,nameofnode} parameter. Name of node must not be a full
	 %%  distributed name. Detest will add @someip to the end. 
	 {nodes,[?ND1,?ND2]}

	 %%  What RPC to execute for stopping nodes.
	 % {stop,{init,stop,[]}},

	 %%  optional command to start erlang with (def. is "erl")
	 % {erlcmd,"../otp/bin/cerl -valgrind"},
	 
	 %%  optional environment variables for erlang (def. is [])
	 % {erlenv,[{"VALGRIND_MISC_FLAGS","-v --leak-check=full --tool=memcheck --track-origins=yes  "++
     %                                "--suppressions=../otp/erts/emulator/valgrind/suppress.standard --show-possibly-lost=no"}]},
     
     %%  in ms, how long to wait to connect to node. If running with valgrind it takes a while. (def. is 10000)
     % {connect_timeout,60000},
     
     %%  in ms, how long to wait for application start once node is started (def. is 30000)
     % {app_wait_timeout,60000*5}
	].


setup(_Param) ->
	ok.

cleanup(_Param) ->
	ok.


run(Param) ->
	lager:info("Script params: ~p",[Param]),
	% Get full dist name for RPC.
	Node1 = proplists:get_value(node1,Param),
	Node2 = proplists:get_value(node2,Param),
	{ok,Node2} = rpc:call(Node1,?MODULE,call_start,[Node2]),
	{ok,Node1} = rpc:call(Node2,?MODULE,call_start,[Node1]),

	Node3 = detest:add_node(?ND3),
	{ok,Node1} = rpc:call(Node3,?MODULE,call_start,[Node1]),
	ok.


% This module is loaded inside every executed node. So we can rpc to these functions on every node.
call_start(Nd) ->
	lager:info("Calling from=~p to=~p, at=~p~n",[node(), Nd, time()]),
	rpc:call(Nd,?MODULE,call_receive,[node()]).

call_receive(From) ->
	lager:info("Received call on=~p from=~p, at=~p~n",[node(), From, time()]),
	{ok,node()}.
