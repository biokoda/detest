-module(test).
% mandatory detest functions
-export([cfg/0,run/2,setup/1,cleanup/1]).
% test functions
-export([call_start/1,call_receive/1]).
% assert macros
-include_lib("eunit/include/eunit.hrl").

cfg() ->
	[{cmd,"-eval \"begin "++
			"application:ensure_all_started(lager),"
		    "{ok,Mod,Bin} = compile:file('test/test.erl',[binary,return_errors,{parse_transform, lager_transform}]),"++
			"code:load_binary(Mod, 'test.erl', Bin) "++
			"end\""
			},
	 {nodes,[
	 		 [{name,node1},{delay,1000}], 
	 		 [{name,node2}]
	 		]}
	].

setup(_Pth) ->
	ok.

cleanup(_Pth) ->
	ok.


run(Nodes,_Pth) ->
	% Get dist name for RPC.
	Node1 = proplists:get_value(node1,Nodes),
	Node2 = proplists:get_value(node2,Nodes),
	{ok,Node2} = rpc:call(Node1,?MODULE,call_start,[Node2]),
	{ok,Node1} = rpc:call(Node2,?MODULE,call_start,[Node1]),
	ok.


call_start(Nd) ->
	lager:info("Calling from=~p to=~p, at=~p~n",[node(), Nd, time()]),
	rpc:call(Nd,?MODULE,call_receive,[node()]).

call_receive(From) ->
	lager:info("Received call on=~p from=~p, at=~p~n",[node(), From, time()]),
	{ok,node()}.
