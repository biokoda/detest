#!/usr/bin/env escript

main(BinFiles) ->
  %% Add ebin paths to our path
  true = code:add_path("ebin"),
  ok = code:add_paths(filelib:wildcard("deps/*/ebin")),
  
  %% Read the contents of the files in ebin(s)
  Files1 = lists:flatmap(fun(Dir) -> load_files(Dir) end, ["ebin"|filelib:wildcard("deps/*/ebin")]),
  Files = [{Fn,element(2,file:read_file(Fn))} || Fn <- BinFiles]++Files1,
  
  case zip:create("mem", Files, [memory]) of
    {ok, {"mem", ZipBin}} ->
      %% Archive was successfully created. Prefix that with header and write to "edis" file
      Script = <<"#!/usr/bin/env escript\n%%! +Bc +K true  -smp enable\n", ZipBin/binary>>,
      case file:write_file("detest", Script) of
        ok -> ok;
        {error, WriteError} ->
          io:format("Failed to write detest: ~p\n", [WriteError]),
          halt(1)
      end;
    {error, ZipError} ->
      io:format("Failed to construct detest archive: ~p\n", [ZipError]),
      halt(1)
  end,
  
  %% Finally, update executable perms for our script
  case os:type() of
    {unix,_} ->
      [] = os:cmd("chmod u+x detest"),
      ok;
    _ ->
      ok
  end.

load_files(Dir) ->
  [read_file(Filename, Dir) || Filename <- filelib:wildcard("*", Dir)].

read_file(Filename, Dir) ->
  {ok, Bin} = file:read_file(filename:join(Dir, Filename)),
  {Filename, Bin}.