-module(java_validator).
-compile(export_all).

start_validator() ->
    case jsg_store:get(java_validator) of
	{ok,_} ->
	    ok;
	_ ->
	    case code:which(java) of
    non_existing ->
      io:format
	("*** Error: the Java Erlang library is not accessible.~n"),
      throw(bad);
    _ ->
      ok
  end,
  ModuleLocation =
    code:which(?MODULE),
  JarDir1 =
    filename:dirname(ModuleLocation)++
    "/../../json_schema_validator/build/libs",
    Jars = 
	case file:list_dir(JarDir1) of
	    {ok,L1} when L1=/=[] -> 
		lists:map(fun (Jar) -> JarDir1++"/"++Jar end, L1);
	    _ ->
		JarDir2 = 
		    filename:dirname(ModuleLocation)++
		    "/../priv/json_schema_validator/build/libs",
		case file:list_dir(JarDir2) of
		    {ok,L2} when L2=/=[] -> 
			lists:map(fun (Jar) -> JarDir2++"/"++Jar end, L2);
		    _ ->
			io:format
			  ("*** Error: could not access the java validator jar file.~n"++
			       "It should be located in ~p~n",
			   [JarDir2]),
			throw(bad)
		end
	end,
  {ok,N} =
    java:start_node
      ([
	{add_to_java_classpath,Jars},
	{java_exception_as_value,true}
       %%,{java_verbose,"FINE"}
       ]),
  Factory =
	ensure_not_exception
	(java:call_static
	   (N,'com.github.fge.jsonschema.main.JsonSchemaFactory','byDefault',[])),
  jsg_store:put(java_validator,Factory),
  jsg_store:put(java_node,N)
    end.

validate(RawSchema,JSON) ->
  {ok,Factory} = 
    jsg_store:get(java_validator),
  N =
    java:node_id(Factory),
  JSONText =
    binary_to_list(iolist_to_binary(JSON)),
  JavaJson =
	ensure_not_exception(
   java:call_static
      (N,
       'com.github.fge.jackson.JsonLoader',
       'fromString',
       [JSONText])),
  Validator =
    case jsg_store:get({java_schema,RawSchema}) of
      {ok,V} ->
	V;
      _ ->
	{ok,CWD} = 
	  file:get_cwd(),
	AbsSchema =
	  mk_absolute_refs(CWD,RawSchema),
	JavaSchema =
		ensure_not_exception(
	  java:call_static
	    (N,
	     'com.github.fge.jackson.JsonLoader',
	     'fromString',
	     [binary_to_list(iolist_to_binary(mochijson2:encode(AbsSchema)))])),
	SchemaValidator =
		ensure_not_exception(
	  java:call(Factory,getJsonSchema,[JavaSchema])),
	jsg_store:put({java_schema,RawSchema},SchemaValidator),
	SchemaValidator
    end,
  Report = ensure_not_exception(java:call(Validator,validate,[JavaJson])),
  case ensure_not_exception(java:call(Report,isSuccess,[])) of
    true ->
      true;
    false ->
      io:format
	("*** Error: validation error~n~s~ndoes not validate against~n~p~n",
	 [JSONText,RawSchema]),
      BadLevels =
	[ensure_not_exception(java:call_static
	   (N,'com.github.fge.jsonschema.core.report.LogLevel',valueOf,
	    ["ERROR"])),
	 ensure_not_exception(java:call_static
	   (N,'com.github.fge.jsonschema.core.report.LogLevel',valueOf,
	    ["FATAL"]))],
      do_print_report(BadLevels,Report),
      false
  end.

mk_absolute_refs(CWD,{struct,Props}) ->
  {struct,
   lists:map
     (fun ({Key,Value}) ->
	  case Key of 
	    <<"$ref">> ->
	      case binary_to_list(Value) of
		[$h,$t,$t,$p,$:|_] ->
		  {Key,Value};
		[$f,$i,$l,$e,$:|Filename] ->
		  {Key,list_to_binary("file:"++CWD++"/"++Filename)};
		Filename ->
		  {Key,list_to_binary("file:"++CWD++"/"++Filename)}
	      end;
	    _ -> {Key,mk_absolute_refs(CWD,Value)}
	  end
      end,
      Props)};
mk_absolute_refs(CWD,L) when is_list(L) ->
  lists:map(fun (Item) -> mk_absolute_refs(CWD,Item) end, L);
mk_absolute_refs(_CWD,V) ->
  V.

do_print_report(BadLevels,Report) ->
  do_print_report1(BadLevels,ensure_not_exception(java:call(Report,iterator,[]))).
do_print_report1(BadLevels,ReportIterator) ->
  case java:call(ReportIterator,hasNext,[]) of
    true ->
      Item = java:call(ReportIterator,next,[]),
      LogLevel = ensure_not_exception(java:call(Item,getLogLevel,[])),
      case lists:any(fun (BadLevel) -> java:eq(LogLevel,BadLevel) end, BadLevels) of
	true ->
	  Message = ensure_not_exception(java:call(Item,getMessage,[])),
	  io:format("Validation message: ~s~n",[ensure_not_exception(java:string_to_list(Message))]);
	false ->
	  ok
      end,
      do_print_report1(BadLevels,ReportIterator);
    false ->
      ok
  end.
  
ensure_not_exception({java_exception,Exc}) ->
    io:format("Java error: "),
    java:print_stacktrace(Exc),
    throw(bad);
ensure_not_exception(null) ->
    io:format("null pointer returned~n"),
    throw(bad);
ensure_not_exception(Other) ->
    Other.

	
	
      
  
