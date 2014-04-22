-module(js_links_machine).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

-record(state,{links,private_state=void}).


%% We assume an ets table initialised with links

api_spec() ->
  #api_spec{}.

initial_state() ->
  [{private_module,Module}] =
    ets:lookup(js_links_machine_data,private_module),
  PrivateState = 
    Module:initial_state(),
  [{initial_links,Links}] =
    ets:lookup(js_links_machine_data,initial_links),
  #state{links=sets:from_list(Links),private_state=PrivateState}.

command(State) ->
  eqc_gen:oneof
  ([
    {call, ?MODULE, follow_link, [Link]} || Link <- sets:to_list(State#state.links)
   ]).

callouts(_,_) ->
  ?EMPTY.

precondition(State,Call) ->
  case Call of
    {_, _, follow_link, [Link], _} ->
      sets:is_element(Link,State#state.links)
  end.

postcondition(State,Call,Result) ->
  io:format("result is ~p~n",[Result]),
  case Result of
    {normal,{200,_}} ->
      true;
    _ ->
      false
  end.

next_state(State,Result,Call) ->
  case Call of
    {_, ?MODULE, follow_link, [Link], _} ->
      case Result of
	{normal,{200,Body}} -> 
	  %%io:format("normal result: extracting links~n",[]),
	  NewLinks =
	    jsg_links:extract_dynamic_links(Link,mochijson2:decode(Body)),
	  State#state{links=sets:union(sets:from_list(NewLinks),State#state.links)};
	_ -> State
      end;
    _ -> io:format("Call was~n~p~n",[Call]), State
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

follow_link(Link) ->
  URI = jsg_links:compute_uri(Link),
  io:format("~nfollow_link: URI is ~p~n",[URI]),
  RequestType = jsg_links:request_type(Link),
  Argument = jsg_links:generate_argument(Link),
  Result =
    case Argument of
      {ok,Body} ->
	case RequestType of
	  get ->
	    http_request_with_headers(URI,RequestType,encode_headers(Body));
	  _ ->
	    http_request_with_body(URI,RequestType,mochijson2:encode(Body))
	end;
      _ ->
	http_request(URI,RequestType)
    end,
  analyze_http_result(Result).

encode_headers(X) ->
  X.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


http_request_with_body(URI,Type,Body) ->
  httpc:request
    (Type,
     {URI,
      [],
      "application/json",
      iolist_to_binary(Body)},
     [{timeout,1500}],
     []).

http_request_with_headers(URI,Type,Headers) ->
  httpc:request
    (Type,
     {URI,
      Headers},
     [{timeout,1500}],
     []).

http_request(URI,Type) ->
  httpc:request
    (Type,
     {URI,[]},
     [{timeout,1500}],
     []).
  
analyze_http_result(Result) ->
  case Result of 
    {ok,{StatusLine,_Headers,RespBody}} ->
      case StatusLine of
	{_,ResponseCode,_} ->
	  {normal,{ResponseCode,RespBody}};
	_ ->
	  {other,Result}
      end;
    _ -> 
      {other,Result}
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
  ?FORALL(Cmds, eqc_dynamic_cluster:dynamic_commands(?MODULE),
	  ?CHECK_COMMANDS
	     ({H, DS, Res},
	      ?MODULE,
	      Cmds,
	      begin
		pretty_commands
		  (?MODULE,
		   Cmds,
		   {H, DS, Res},
		   Res == ok)
	      end)).

test() ->
  case eqc:quickcheck(prop_ok()) of
    false ->
      io:format
	("~n~n***FAILED; counterexample is~n~n~p~n",
	 [eqc:counterexample()]);
    true ->
      io:format
	("~n~nPASSED~n",[])
  end.

