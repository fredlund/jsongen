-module(jsg_rtest).

-export([check/0,test_schema_file/1]).

%% run a test using erl -pa ebin -run rtest test -run erlang halt

-include_lib("eunit/include/eunit.hrl").

check() ->
  case ?MODULE:test() of
    ok ->
      halt(0);
    error ->
      halt(1)
  end.

schemas_test_() ->
  %% Figure out where the schema tests are
  {module,jsongen} = code:ensure_loaded(jsongen),
  {file,File} = code:is_loaded(jsongen),
  DirName = filename:dirname(File),
  TestDir = DirName++"/../tests",
  ?debugFmt("Test directory is~n~p~n",[TestDir]),
  Filenames = filelib:wildcard(TestDir++"/*.jsch"),
  ?debugFmt("Will test schemas~n~p~n",[Filenames]),
  lists:foldl
    (fun (SchemaFile,Acc) ->
	 try jsg_jsonschema:read_schema(SchemaFile) of
	   {ok,Schema} ->
	     Basename = filename:rootname(filename:basename(SchemaFile)),
	     [{"schema \""++Basename++"\"",
	       {timeout,
		5,
		test_schema(Schema)}}|Acc];
	   Other ->
	     ?debugFmt
		("Reading schema~n  ~s~nfailed with error ~p~n",
		 [SchemaFile,Other]),
	     Acc
	 catch _:_ ->
	     ?debugFmt
	       ("*** Error: could not read JSON schema ~s~n",
		[SchemaFile]),
	     ?debugFmt
	       ("Stacktrace:~n~p~n",
		[erlang:get_stacktrace()]),
	     Acc
	 end
     end, [], Filenames).

test_schema_file([Filename]) ->
  {ok,Schema} = jsg_jsonschema:read_schema(Filename),
  (test_schema(Schema))().
  
test_schema(Schema) ->
  IsEmpty = jsg_jsonschema:keyword(Schema,"empty",false),
  fun () ->
      Generator = jsongen:json(Schema),
      if
	IsEmpty ->
	  Succeeds =
	    try Value=pick_n_values(Generator,1,Schema), {false,Value}
	    catch _:_ -> true end,
	  Succeeds == true;
	true ->
	  pick_n_values(Generator,10,Schema)
      end
  end.

pick_n_values(Generator,N,Schema) when N>0 ->      
  Value = eqc_gen:pick(Generator),
  case jsg_json_validate:validate(Value,Schema) of
    true -> ok;
    maybe -> ok;
    Other ->
      io:fwrite
	("~n*** ERROR: Validation returns value: ~p~n",[Other]),
      io:fwrite
	("~nValue: ~s~n",[jsg_json:encode(Value)]),      
      io:fwrite
	("~nSchema: ~s~n", [jsg_json:encode(Schema)]),
      throw(validation_error)
  end,
  pick_n_values(Generator,N-1,Schema);
pick_n_values(_Generator,_N,_Schema) -> 
  ok.

