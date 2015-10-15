-module(js_links_machine).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").
-include_lib("jsongen.hrl").


%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.


api_spec() ->
  #api_spec{}.

initial_state() ->
  PrivateState = 
    case exists_private_function(initial_state,0) of
      true ->
	(private_module()):initial_state();
      false ->
	void
    end,
  #state
    {static_links=initial_links(),
     initialized=false,
     dynamic_links=jsl_dynamic_links:initialize(20),
     private_state=PrivateState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_pre(State) ->
  not(State#state.initialized).

start_args(_State) ->
  [].

start() ->
  %%jsg_store:open_clean_db(),
  jsg_utils:clear_schema_cache(),
  true = ets:match_delete(jsg_store,{{object,'_'},'_'}),
  true = ets:match_delete(jsg_store,{{term,'_'},'_'}),
  true = ets:match_delete(jsg_store,{{link,'_'},'_'}),
  true = ets:match_delete(jsg_store,{{reverse_link,'_'},'_'}),
  httpc:reset_cookies().

start_post(State,_,_) ->
  State#state{initialized=true}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

link_pre(State) ->
  State#state.initialized.

link_args(State) ->
  case jsl_dynamic_links:is_empty(State#state.dynamic_links) of
    true ->
      ?LET(FinalLink,
	   eqc_gen:oneof(State#state.static_links),
	   gen_call(FinalLink));
    false ->
      ?LET
	 (FinalLink,
	  eqc_gen:
	     frequency
	       ([{7,
		  ?LET
		    (Title,
		     eqc_gen:oneof
		       (jsl_dynamic_links:titles
			  (State#state.dynamic_links)),
		     eqc_gen:oneof
		       (jsl_dynamic_links:links
			  (Title,State#state.dynamic_links)))},
		{1,
		 eqc_gen:oneof(State#state.static_links)}]),
	   gen_call(FinalLink))
      end.

link_pre(State,[Link,_]) ->
  (State#state.initialized==true) 
    andalso ((jsg_links:link_type(Link)==static)
	     orelse 
	     jsl_dynamic_links:is_element(Link,State#state.dynamic_links))
    andalso link_permitted(State,Link).

link_permitted(State,Link) ->
  make_call(link_permitted,fun link_permitted_int/2,[State,Link]).

link_permitted_int(_State,_Link) ->
  true.

gen_call(Link) ->
  ?LET(Parms,
       gen_http_request(Link),
       [Link,Parms]).

link_post(State,Args,Result) ->
  try make_call(postcondition,fun postcondition_int/3,[State,Args,Result])
  catch Class:Reason ->
      io:format
	("Warning: postcondition/3 raises exception ~p~n",
	 [Reason]),
      StackTrace = erlang:get_stacktrace(),
      erlang:raise(Class,Reason,StackTrace)
  end.

postcondition_int(_State,Args,Result) ->
  case validate_call_not_error_result(Args,Result) of
    true ->
      case http_result_code(Result) of
	200 ->
	  validate_call_result_body(Args,Result);
	Other ->
	  io:format
	    ("~n*** Error: postcondition error: for http call~n~s~nhttp responded with result code ~p, expected result code 200~n",
	     [format_http_call(Args),Other]),
	  false
      end;
    _ ->
      io:format("validation failed~n"),
      false
  end.

validate_call_not_error_result(Args,Result) ->
  case http_result_type(Result) of
    ok ->
      true;
    {error,_Error} ->
      io:format
	("~n*** Error: postcondition error: for http call~n~s~nhttp responded with error ~p~n",
	 [format_http_call(Args),http_error(Result)]),
      false
  end.

validate_call_result_body(Args,Result) ->
  Link = jsg_links:link_def(call_link(Args)),
  Schema = jsg_links:link_schema(call_link(Args)),
  case jsg_jsonschema:propertyValue(Link,"targetSchema") of
    undefined ->
      true;
    TargetSchema ->
      RealTargetSchema = jsg_links:get_schema(TargetSchema,Schema),
      case response_has_json_body(Result) of
	false ->
	  false;
	true ->
	  Body = http_body(Result),
	  Validator = get_option(validator),
	  try Validator:validate(RealTargetSchema,Body)
	  catch _Class:Reason ->
	      io:format
		("~n*** Error: postcondition error: for http call~n~s~n"++
		   "the JSON value~n~s~n"++
		   "did not validate against the schema~n~s~n"++
		   "due to error~n~p~n",
		 [format_http_call(Args),
		  Body,
		  mochijson2:encode(RealTargetSchema),
		  Reason]),
	      io:format
		("Stacktrace:~n~p~n",
		 [erlang:get_stacktrace()]),
	      false
	  end
      end
  end.

link_next(State,Result,Args) ->
  try make_call(next_state,fun next_state_int/3,[State,Result,Args])
  catch Class:Reason ->
      io:format
	("Warning: next_state/3 raises exception ~p~n",
	 [Reason]),
      StackTrace = erlang:get_stacktrace(),
      erlang:raise(Class,Reason,StackTrace)
  end.

next_state_int(State,Result,[Link,_]) ->
  case Result of
    {ok,{{_,_Code,_},_Headers,_Body}} ->
      LinksToAdd =
	case response_has_body(Result) of
	  true ->
	    JSONbody = mochijson2:decode(http_body(Result)),
	    jsg_links:extract_dynamic_links
	      (Link,JSONbody,jsg_links:intern_object(JSONbody));
	  _ ->
	    []
	end,
      State#state
	{
	dynamic_links=
	  lists:foldl
	    (fun (DLink,DLs) ->
		 jsl_dynamic_links:add_link(DLink,DLs)
	     end, State#state.dynamic_links, LinksToAdd)
       };
    _Other ->
      State
  end.

make_call(ExternalFunction,InternalFunction,Args) ->
  [{private_module,Module}] = 
    ets:lookup(js_links_machine_data,private_module),
  {arity,Arity} = erlang:fun_info(InternalFunction,arity),
  case exists_private_function(ExternalFunction,Arity+1) of
    true ->
      apply(Module,ExternalFunction,[InternalFunction|Args]);
    false ->
      ?LOG
	("function ~p:~p/~p missing~n",
	 [Module,ExternalFunction,Arity+1]),
      apply(InternalFunction,Args)
  end.

exists_private_function(Function,Arity) ->
  [{private_module,Module}] = 
    ets:lookup(js_links_machine_data,private_module),
  try Module:module_info(exports) of
      Exports -> lists:member({Function,Arity},Exports)
  catch _:_ -> false end.

private_module() ->
  [{private_module,Module}] = 
    ets:lookup(js_links_machine_data,private_module),
  Module.

initial_links() ->
  [{initial_links,Links}] =
    ets:lookup(js_links_machine_data,initial_links),
  Links.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_http_request(Link) ->
  ?LET({Body,QueryParms},
       {generate_body(Link),generate_parameters(Link)},
       begin
	 PreURI = jsg_links:link_calculated_href(Link),
	 RequestType = jsg_links:link_request_type(Link),
	 EncodedParms = encode_generated_parameters(QueryParms),
	 case re:split(PreURI,"\\?") of
	   [_] ->
	     {PreURI,RequestType,Body,EncodedParms};
	   [BinaryURI,BinaryParms] -> 
	     {binary_to_list(BinaryURI),RequestType,Body,
	      split_parms(BinaryParms)++EncodedParms}
	 end
       end).

generate_body(Link) ->
  Sch = jsg_links:link_def(Link),
  Schema = jsg_jsonschema:propertyValue(Sch,"schema"),
  RequestType = jsg_links:link_request_type(Link),
  case may_have_body(RequestType) of
    true when Schema=/=undefined -> 
      jsongen:json(Schema);
    _ -> 
      undefined
  end.

generate_parameters(Link) ->
  Sch = jsg_links:link_def(Link),
  Schema = jsg_jsonschema:propertyValue(Sch,"schema"),
  QuerySchema = jsg_jsonschema:propertyValue(Sch,"querySchema"),
  RequestType = jsg_links:link_request_type(Link),
  case may_have_body(RequestType) of
    true when QuerySchema=/=undefined ->
      jsongen:json(QuerySchema);
    false when QuerySchema=/=undefined ->
      jsongen:json(QuerySchema);
    false when Schema=/=undefined ->
      jsongen:json(Schema);
    _ ->
      undefined
  end.

may_have_body(get) ->
  false;
may_have_body(delete) ->
  false;
may_have_body(_) ->
  true.

split_parms(BinaryParms) ->
  case re:split(BinaryParms,"&") of
    [_] ->
      [Key,Value] = re:split(BinaryParms,"="),
      [{binary_to_list(Key),binary_to_list(Value)}];
    Assignments ->
      lists:flatmap(fun split_parms/1, Assignments)
  end.

link(Link,_HTTPRequest={URI,RequestType,Body,QueryParms}) ->
  case jsg_links:link_title(Link) of
    String when is_list(String) ->
      Key = list_to_atom(String),
      {ok,Stats} = jsg_store:get(stats),
      NewValue =
	case lists:keyfind(Key,1,Stats) of
	  false -> 1;
	  {_,N} -> N+1
	end,
      jsg_store:put(stats,lists:keystore(Key,1,Stats,{Key,NewValue}));
    _ -> ok
  end,
  Result = http_request(URI,RequestType,Body,QueryParms,Link),
  case response_has_body(Result) of
    true ->
      ResponseBody = http_body(Result),
      case length(ResponseBody)>1024 of
	true ->
	  jsg_store:put(last_body,{body,ResponseBody}),
	  {P1,{P2,P3,_}} = Result,
	  {P1,{P2,P3,ets_body}};
	false ->
	  jsg_store:put(last_body,has_body),
	  Result
      end;
    false ->
      jsg_store:put(last_body,no_body),
      Result
  end.

format_http_call([_,{URI,RequestType,Body,Params}]) ->
  format_http_call(URI,RequestType,Body,Params).

format_http_call(PreURI,RequestType,Body,Params) ->
  BodyString =
    case Body of
      {ok,JSON} ->
	io_lib:format(" body=~s",[mochijson2:encode(JSON)]);
      _ ->
	""
    end,
  URI = 
    case Params of
      [] -> PreURI;
      _ -> PreURI++"?"++encode_parameters(Params)
    end,
  io_lib:format
    ("~s using ~s~s",
     [URI,string:to_upper(atom_to_list(RequestType)),BodyString]).

has_body(get) ->
  false;
has_body(delete) ->
  false;
has_body(_) ->
  true.

encode_generated_parameters(Parms) ->
  case Parms of
    {ok,{struct,L}} ->
      lists:map
	(fun ({Key,Value}) ->
	     {to_list(Key), to_list(Value)}
	 end, L);
    _ -> []
  end.

to_list(B) when is_binary(B) ->
  binary_to_list(B);
to_list(I) when is_integer(I) ->
  integer_to_list(I).

encode_parameters([]) -> "";
encode_parameters([{Key,Value}|Rest]) -> 
  Continuation = 
    if
      Rest==[] -> "";
      true -> "&"++encode_parameters(Rest)
    end,
  Key++"="++encode(Value)++Continuation.

encode(String) when is_list(String) ->
  http_uri:encode(String).

http_request(PreURI,Type,Body,QueryParms,Link) ->
  %%io:format("URI: ~s cookies are ~p~n",[PreURI,httpc:which_cookies()]),
  URI =
    case QueryParms of
      [] -> PreURI;
      _ -> PreURI++"?"++encode_parameters(QueryParms)
    end,
  URIwithBody =
    case Body of
      {ok,RawBody} ->
	{URI,[],
	 "application/json",
	 iolist_to_binary(mochijson2:encode(RawBody))};
      _ ->
	{URI,[]}
    end,
  Timeout = get_option(timeout),
  Request = [Type,URIwithBody,[{timeout,Timeout}],[]],
  %%io:format("Request=~p~n",[Request]),
  case get_option(show_uri) of
    true -> io:format("Accessing URI ~p~n",[URI]);
    false -> ok
  end,
  {ElapsedTime,Result} =
    case get_option(simulation_mode) of
      false ->
	timer:tc(httpc,request,Request);
    true -> 
	TargetSchema =
	  jsg_links:get_schema(jsg_links:link_targetSchema(Link)),
	ResponseBody =
	  case TargetSchema of
	    undefined ->
	      eqc_gen:pick(jsongen:anyType());
	    _ ->
	      eqc_gen:pick(jsongen:json(TargetSchema))
	  end,
	EncodedBody = mochijson2:encode(ResponseBody),
	Headers = {"HTTP/1.1",200,"OK"},
	StatusLine = [{"content-length",integer_to_list(length(EncodedBody))},
		      {"content-type","application/json;charset=UTF-8"}],
	{1000,{ok,{Headers,StatusLine,EncodedBody}}}
  end,
  case get_option(show_http_timing) of
    true -> io:format("http request took ~p milliseconds~n",[ElapsedTime/1000]);
    false -> ok
  end,
  Result.

http_result_type({ok,_}) ->
  ok;
http_result_type(Other) ->
  Other.

http_error({error,Error}) ->
  Error.

http_headers({ok,{_,Headers,_}}) ->
  Headers.

http_body({ok,{_,_,Body}}) ->
  case Body of
    ets_body ->
      {ok,{body,RealBody}} = jsg_store:get(last_body),
      RealBody;
    _ ->
      Body
  end.

has_ets_body({ok,{_,_,Body}}) ->
  case Body of
    ets_body ->
      true;
    _ ->
      false
  end.

http_status_line({ok,{StatusLine,_,_}}) ->
  StatusLine.

http_version(Result) ->
  case http_status_line(Result) of
    {Version,_,_} ->
      Version
  end.

http_result_code(Result) ->
  case http_status_line(Result) of
    {_,ResultCode,_} ->
      ResultCode
  end.

http_reason_phrase(Result) ->
  case http_status_line(Result) of
    {_,_,ReasonPhrase} ->
      ReasonPhrase
  end.

http_response_is_ok(Result) ->
  case http_result_type(Result) of
    ok ->  http_result_code(Result)==200;
    _ -> false
  end.

http_content_length(Result) ->
  Headers = http_headers(Result),
  proplists:get_value("content-length",Headers).

http_content_type(Result) ->
  Headers = http_headers(Result),
  proplists:get_value("content-type",Headers).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Probably non-ok responses can have a body too...
response_has_body(Result) ->
  case http_result_type(Result) of
    ok -> 
      ContentLength = http_content_length(Result),
      if
	ContentLength=/=undefined ->
	  ContLen = list_to_integer(ContentLength),
	  ContLen>0;
	true -> 
	  false
      end;
    _ -> false
  end.

response_has_json_body(Result) ->
  case response_has_body(Result) of
    true -> string:str(http_content_type(Result), "application/json") >= 0;
    false -> false
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_table(PrivateModule,Links) ->
  case ets:info(js_links_machine_data) of
    undefined ->
      ok;
    _ ->
      [{pid,Pid}] = ets:lookup(js_links_machine_data,pid),
      exit(Pid,kill),
      ets:delete(js_links_machine_data)
  end,
  spawn
    (fun () ->
	 ets:new(js_links_machine_data,[named_table,public]),
	 ets:insert(js_links_machine_data,{pid,self()}),
	 wait_forever()
     end),
  wait_until_stable(),
  ets:insert(js_links_machine_data,{private_module,PrivateModule}),
  ets:insert(js_links_machine_data,{initial_links,Links}).

wait_until_stable() ->
  case ets:info(js_links_machine_data) of
    L when is_list(L) ->
      ok;
    _ ->
      wait_until_stable()
  end.

wait_forever() ->
  receive _ -> wait_forever() end.

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop_ok() ->
  ?FORALL
     (Cmds, eqc_dynamic_cluster:dynamic_commands(?MODULE),
      ?CHECK_COMMANDS
	 ({H, DS, Res},
	  ?MODULE,
	  Cmds,
	  begin
	    %%io:format("Res size is ~p~n",[erts_debug:size(Res)]),
	    %%io:format("DS size is ~p~n",[erts_debug:size(DS)]),
	    %%io:format("length(H)=~p~n",[length(H)]),
	    %%[{P1,P2,P3}|_] = lists:reverse(H),
	    %%io:format("P1(1).size=~p~n",[erts_debug:size(P1)]),
	    %%io:format("P2(1).size=~p~n",[erts_debug:size(P2)]),
	    %%io:format("P3(1).size=~p~n",[erts_debug:size(P3)]),
	    %%io:format("P1=~p~n",[P1]),
	    %%io:format("P2=~p~n",[P2]),
	    %%io:format("P3=~p~n",[P3]),
	    %%io:format("H size is ~p~n",[erts_debug:size(H)]),
	    %%io:format("H=~p~nDS=~p~n",[H,DS]),
	    if
	      Res == ok ->
		true;
	      true ->
		print_counterexample(Cmds,H,DS,Res),
		false
	    end
	  end)).

print_counterexample(Cmds,H,_DS,Reason) ->
  io:format("~nTest failed with reason ~p~n",[Reason]),
  {FailingCommandSequence,_} = lists:split(length(H)+1,Cmds),
  ReturnValues = 
    case Reason of
      {exception,_} ->
	(lists:map(fun ({_,_,Result}) -> Result end, H))++[Reason];
      _ ->
	(lists:map(fun ({_,_,Result}) -> Result end, H))
    end,
  io:format("~nCommand sequence:~n"),
  io:format("---------------~n~n"),
  print_commands(lists:zip(tl(FailingCommandSequence),ReturnValues)),
  io:format("~n~n").

print_commands([]) ->
  ok;
print_commands([{_Call={call,_,initialize,_,_},_Result}|Rest]) ->
  print_commands(Rest);
print_commands([{Call={call,_,follow_link,_,_},Result}|Rest]) ->
  Title = call_link_title(Call),
  TitleString = 
    if 
      Title==undefined ->
	"Link ";
      true ->
	io_lib:format("Link ~p ",[Title])
    end,
  ResultString =
    case http_result_type(Result) of
      {error,Error} -> 
	io_lib:format(" ->~n    error ~p~n",[Error]);
      ok ->
	ResponseCode = http_result_code(Result),
	case response_has_body(Result) of
	  true -> 
	    Body =
	      case has_ets_body(Result) of
		true -> "<<abstracted_body>>";
		false -> http_body(Result)
	      end,
	    io_lib:format
	      (" ->~n    ~p with body ~s",
	       [ResponseCode,Body]);
	  false ->
	    io_lib:format
	      (" ->~n     ~p",
	       [ResponseCode])
	end
    end,
  io:format
    ("~saccess ~s~s~n~n",
     [TitleString,format_http_call(Call),ResultString]),
  print_commands(Rest).
  
test() ->
  Validator = get_option(validator),
  io:format("Validator is ~p~n",[Validator]),
  Validator:start_validator(),
  jsg_store:put(stats,[]),
  case eqc:quickcheck(eqc:on_output(fun eqc_printer/2,prop_ok())) of
    false ->
      io:format("~n~n***FAILED~n");
    true ->
      io:format("~n~nPASSED~n",[])
  end,
  print_stats().

run_statem(PrivateModule,Files) ->
  run_statem(PrivateModule,Files,[]).

run_statem(PrivateModule,Files,Options) ->
  inets:start(),
  case proplists:get_value(cookies,Options) of
    true ->
      ok = httpc:set_options([{cookies,enabled}]);
    _ ->
      ok
  end,
  case collect_links(Files) of
    [] ->
      io:format
	("*** Error: no independent links could be found among the files ~p~n",
	 [Files]),
      throw(bad);
    Links ->
      js_links_machine:init_table(PrivateModule,Links)
  end,
  check_and_set_options(Options),
  js_links_machine:test().

print_stats() ->
  {ok,Stats} = jsg_store:get(stats),
  TotalCalls =
    lists:foldl(fun ({_,N},Acc) -> N+Acc end, 0, Stats),
  SortedStats =
    lists:sort(fun ({_,N},{_,M}) -> N>=M end, Stats),
  io:format("~nLink statistics:~n-------------------~n"),
  lists:foreach
    (fun ({Name,NumCalls}) ->
	 Percentage = (NumCalls/TotalCalls)*100,
	 io:format("~p: ~p calls (~p%)~n",[Name,NumCalls,Percentage])
     end, SortedStats).

%% To make eqc not print the horrible counterexample
eqc_printer(Format,String) ->
  case Format of
    "~p~n" -> ok;
    _ -> io:format(Format,String)
  end.

check_and_set_options(Options) ->
  ParsedOptions =
    lists:map
      (fun (Option) ->
	   {Prop,Value} = ParsedOption =
	     case Option of
	       {Atom,Val} when is_atom(Atom) -> {Atom,Val};
	       Atom when is_atom(Atom) -> {Atom,true}
	     end,
	 case Prop of
	   cookies when is_boolean(Value) -> 
	     if
	       Value -> 
		 ok = httpc:set_options([{cookies,enabled}]);
	       true ->
		 ok
	     end,
	     ParsedOption;
	   timeout when is_integer(Value),Value>0 -> ParsedOption;
	   simulation_mode when is_boolean(Value) -> ParsedOption;
	   show_http_timing when is_boolean(Value) -> ParsedOption;
	   show_uri when is_boolean(Value) -> ParsedOption;
	   validator when is_atom(Value) -> ParsedOption;
	   Other ->
	     io:format
	       ("*** Error: option ~p not recognized~n",
		[Other]),
	     throw(bad)
	 end
     end, Options),
  NewParsedOptions1 =
    case proplists:get_value(validator,ParsedOptions) of
      undefined -> [{validator,java_validator}|ParsedOptions];
      _ -> ParsedOptions
    end,
  NewParsedOptions2 =
    case proplists:get_value(timeout,NewParsedOptions1) of
      undefined -> [{timeout,1500}|NewParsedOptions1];
      _ -> NewParsedOptions1
    end,
  ets:insert(js_links_machine_data,{options,NewParsedOptions2}).

get_option(Atom) when is_atom(Atom) ->
  [{_,Options}] = ets:lookup(js_links_machine_data,options),
  proplists:get_value(Atom,Options,false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

call_link_title(Call) ->
  jsg_links:link_title(call_link(Call)).

call_link([Link,_]) ->
  Link.

json_call_body([_,{_,_,BodyArg,_}]) ->
  case BodyArg of
    {ok,Body} -> Body
  end.

get_json_body(Result) ->
  case response_has_json_body(Result) of
    true -> mochijson2:decode(http_body(Result))
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

collect_links(Files) ->
  lists:flatmap(fun collect_links_from_file/1, Files).

collect_links_from_file(File) ->
  FileSchema = {struct,[{<<"$ref">>,list_to_binary(File)}]},
  lists:map
    (fun (Link={link,Props}) ->
	 {link,[{type,static},
		{calculated_href,jsg_links:link_href(Link)}|Props]}
     end, collect_schema_links(FileSchema,false)).

collect_schema_links(RawSchema, DependsOnObject) ->
  Schema = jsg_links:get_schema(RawSchema),
  %% Find all schemas, and retrieve links
  case jsg_jsonschema:links(Schema) of
    undefined ->
      [];
    Links when is_list(Links) ->
      lists:foldl
	(fun ({N,Link},Ls) ->
	     Dependency = depends_on_object_properties(Link),
	     if
	       Dependency==DependsOnObject ->
		 [{link,[{link,N},{schema,RawSchema}]}|Ls];
	       true ->
		 Ls
	     end
	 end, [], lists:zip(lists:seq(1,length(Links)),Links))
  end.

depends_on_object_properties(Link) ->
  case jsg_jsonschema:propertyValue(Link,"href") of
    Value when is_binary(Value) ->
      Href = binary_to_list(Value),
      Template = uri_template:parse(Href),
      (jsg_jsonschema:propertyValue(Link,"isRelative")==true) orelse
	(lists:any(fun ({var, _, _}) -> true;
		       (_) -> false
		   end, Template))
  end.


  

