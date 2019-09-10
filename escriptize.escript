#!/usr/bin/env escript


main(BinFiles1) ->
  BinFiles = ["deps/bkdcore/ebin/butil.beam","deps/bkdcore/ebin/bjson.beam","deps/bkdcore/ebin/bmochinum.beam"] ++ BinFiles1,
  Apps = [erlydtl,lager,merl,goldrush,eunit_formatters,damocles],

  %% Add ebin paths to our path
  true = code:add_path("ebin"),
  ok = code:add_paths(filelib:wildcard("deps/*/ebin")),

  %% Read the contents of the files in ebin(s)
  Files1 = [begin
    FileList = filelib:wildcard("deps/"++atom_to_list(Dir)++"/ebin/*.*") ++ filelib:wildcard("ebin/*.*"),
    [{filename:basename(Nm),element(2,file:read_file(Nm))} || Nm <- FileList]
  end || Dir <- Apps],

  Files = [{filename:basename(Fn),element(2,file:read_file(Fn))} || Fn <- BinFiles]++lists:flatten(Files1),

  case zip:create("mem", Files, [memory]) of
    {ok, {"mem", ZipBin}} ->
      %% Archive was successfully created. Prefix that with header and write to "edis" file
      Script = <<"#!/usr/bin/env escript\n%%! +Bc +K true -start_epmd false -epmd_module detest_pmd -smp enable\n", ZipBin/binary>>,
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
