-module(dist_test).
-compile(export_all).
-define(ND1,'node1@127.0.0.1').
-define(ND2,'node2@127.0.0.1').
-define(ND3,'node3@127.0.0.1').

cfg() ->
	[
		% config template files that need all node info (parameter is {nodes,...} from this list) 
		{global_cfg,["test/nodes.yaml"]},
		% config files per node (parameters are k/v pairs for every node)
		{per_node_cfg,["test/app.config","test/vm.args"]},
		% distname is mandatory parameter
		{nodes,[{distname,?ND1},{name,"node1"},{path,"testdb/node1"},{rpcport,3080},{log,"log/node1"},
				% When node is run disttest sets -config, -pa, -args_file, what is requires is command to execute 
				%  the server. Either -s, -eval
				{cmd,"-eval \"application:start(actordb_core)\""}]}
	].

run() ->
	ok.

