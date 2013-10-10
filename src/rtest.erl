-module(rtest).

%% run a test using erl -pa ebin -run rtest test -run erlang halt

-include_lib("eunit/include/eunit.hrl").

schemas_test_() ->
  %% Figure out where the schema tests are
  {module,jsongen} = code:ensure_loaded(jsongen),
  {file,File} = code:is_loaded(jsongen),
  DirName = filename:dirname(File),
  TestDir = DirName++"/../tests",
  ?debugFmt("Test directory is~n~p~n",[TestDir]),
  Filenames = filelib:wildcard(TestDir++"/*.jsch"),
  ?debugFmt("Will test schemas~n~p~n",[Filenames]),
  lists:map
    (fun (SchemaFile) ->
	 case jsonschema:read_file(SchemaFile) of
	   {ok,Schema} ->
	     Basename = filename:rootname(filename:basename(SchemaFile)),
	     {"schema \""++Basename++"\"",test_schema(Schema)};
	   Other ->
	     ?debugFmt
		("Reading schema~n  ~s~nfailed with error ~p~n",
		 [SchemaFile,Other]),
	     throw(error)
	 end
     end, Filenames).

test_schema(Schema) ->
  IsEmpty = jsonschema:keyword(Schema,"empty",false),
  fun () ->
      Generator = jsongen:json(Schema),
      if
	IsEmpty ->
	  Succeeds =
	    try Value=pick_n_values(Generator,1), {false,Value}
	    catch _:_ -> true end,
	  Succeeds == true;
	true ->
	  pick_n_values(Generator,10)
      end
  end.

pick_n_values(Generator,N) when N>0 ->      
  eqc_gen:pick(Generator),
  pick_n_values(Generator,N-1);
pick_n_values(_Generator,_N) -> 
  ok.

  
