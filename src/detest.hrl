-define(PATH,".detest").
-define(INF(F,Param),
	case butil:ds_val(quiet,etscfg) of
	true ->
		ok;
	_ ->
		io:format("~p ~p: ~s~n",[time(),?MODULE,io_lib:fwrite(F,Param)])
	end).
-define(INF(F),?INF(F,[])).
