-module(test).
% mandatory detest functions
-export([cfg/0,run/1,setup/1,cleanup/1]).
% test functions
-export([call_start/1,call_receive/1]).
% assert macros
-include_lib("eunit/include/eunit.hrl").
-define(ND1,'node1@192.168.100.111').
-define(ND2,'node2@192.168.100.112').

cfg() ->
	[{cmd,"-eval \"begin "++
			"application:ensure_all_started(lager),"
		    "{ok,Mod,Bin} = compile:file('test/test.erl',[binary,return_errors,{parse_transform, lager_transform}]),"++
			"code:load_binary(Mod, 'test.erl', Bin) "++
			"end\""
			},
	 {nodes,[
	 		 [{distname,?ND1},{delay,1000}], 
	 		 [{distname,?ND2}]
	 		]}
	].

setup(_Pth) ->
	ok.

cleanup(_Pth) ->
	ok.


run(_Pth) ->
	rpc:call(?ND1,?MODULE,call_start,[?ND2]),
	timer:sleep(5000),
	% rpc:call(?ND2,?MODULE,call_start,[?ND1]),
	ok.


call_start(Nd) ->
	lager:info("Calling from=~p to=~p, at=~p~n",[node(), Nd, time()]),
	rpc:call(Nd,?MODULE,call_receive,[node()]).

call_receive(From) ->
	lager:info("Received call on=~p from=~p, at=~p~n",[node(), From, time()]).
