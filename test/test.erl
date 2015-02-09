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
	[
	 % {global_cfg,["test/nodelist.cfg"]},
	 {cmd,"-s lager"},
	 {wait_for_app,lager},
	 {nodes,[?ND1,?ND2]}
	].


setup(_Param) ->
	ok.

cleanup(_Param) ->
	ok.


run(Param) ->
	lager:info("Script params: ~p",[Param]),
	% Get dist name for RPC.
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
