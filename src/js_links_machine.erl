-module(js_links_machine).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").


%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.


-record(state,{static_links,links,private_state=void}).


%% We assume an ets table initialised with links

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
    {static_links=sets:from_list(initial_links()),
     links=sets:new(),
     private_state=PrivateState}.

command(State) ->
  make_call(command,fun command_int/1, [State]).

command_int(State) ->
  Alternatives =
    [
     {call, ?MODULE, follow_link, [Link,gen_http_request(Link)]} ||
      Link <-
	sets:to_list
	  (sets:union
	     (State#state.static_links,
	      State#state.links)),
      link_permitted(State,Link)
    ],
  eqc_gen:oneof(Alternatives).

callouts(_,_) ->
  ?EMPTY.

link_permitted(State,Link) ->
  make_call(link_permitted,fun link_permitted_int/2,[State,Link]).

link_permitted_int(State,Link) ->
  true.

precondition(State,Call) ->
  make_call(precondition,fun precondition_int/2,[State,Call]).

precondition_int(State,Call) ->
  case Call of
    {_, _, follow_link, [Link,_], _} ->
      sets:is_element(Link,State#state.static_links) orelse
	sets:is_element(Link,State#state.links),
      link_permitted(State,Link)
  end.

postcondition(State,Call,Result) ->
  make_call(postcondition,fun postcondition_int/3,[State,Call,Result]).

postcondition_int(State,Call,Result) ->
  ?LOG("result is ~p~n",[Result]),
  case Result of
    {normal,{200,_}} ->
      true;
    _ ->
      false
  end.

next_state(State,Result,Call) ->
  make_call(next_state,fun next_state_int/3,[State,Result,Call]).

next_state_int(State,Result,Call) ->
  case Call of
    {_, ?MODULE, follow_link, [Link,_], _} ->
      case Result of
	{normal,{200,Body}} -> 
	  %%io:format("normal result: extracting links~n",[]),
	  NewLinks =
	    jsg_links:extract_dynamic_links(Link,mochijson2:decode(Body)),
	  State#state{links=sets:union(sets:from_list(NewLinks),State#state.links)};
	_ -> State
      end;
    _ -> ?LOG("Call was~n~p~n",[Call]), State
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
  URI = jsg_links:compute_uri(Link),
  RequestType = jsg_links:request_type(Link),
  Argument = jsg_links:generate_argument(Link),
  {URI,RequestType,Argument}.

follow_link(Link,HTTPRequest={URI,RequestType,Argument}) ->
  ?LOG("~nfollow_link: URI is ~p; request ~p~n",[URI,RequestType]),
  Result =
    case Argument of
      {ok,Body} ->
	case has_body(RequestType) of
	  true ->
	    http_request_with_body(URI,RequestType,mochijson2:encode(Body));
	  false ->
	    http_request_with_headers(URI,RequestType,encode_headers(Body))
	end;
      _ ->
	http_request(URI,RequestType)
    end,
  analyze_http_result(Result).

has_body(get) ->
  false;
has_body(delete) ->
  false;
has_body(_) ->
  true.

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
  ?FORALL
     (Cmds, eqc_dynamic_cluster:dynamic_commands(?MODULE),
      ?CHECK_COMMANDS
	 ({H, DS, Res},
	  ?MODULE,
	  Cmds,
	  begin
	    print_counterexample(Cmds,H,DS,Res),
	    Res == ok
	  end)).

print_counterexample(Cmds,H,DS,ok) ->
  ok;
print_counterexample(Cmds,H,DS,Reason) ->
  io:format("~nTest failed with reason ~p~n",[Reason]),
  {FailingCommandSequence,_} = lists:split(length(H)+1,Cmds),
  io:format
    ("len(failedCommands)=~p len(history)=~p~n",
     [length(FailingCommandSequence),length(H)]),
  ReturnValues = 
    case Reason of
      {exception,_} ->
	(lists:map(fun ({_,_,Result}) -> Result end, H))++[Reason];
      _ ->
	(lists:map(fun ({_,_,Result}) -> Result end, H))
    end,
  io:format("Commands:~n"),
  print_commands(lists:zip(tl(FailingCommandSequence),ReturnValues)),
  io:format("~n").

print_commands([]) ->
  ok;
print_commands([{Call={call,_,follow_link,[_,Request],_},Result}|Rest]) ->
  Title = test:link_title(Call),
  TitleString = 
    if 
      Title==undefined ->
	"link ";
      true ->
	io_lib:format("link ~p ",[Title])
    end,
  {URI,RequestType,Body} = Request,
  BodyString =
    case Body of
      {ok,JSON} ->
	io_lib:format(" with body ~s",[mochijson2:encode(JSON)]);
      _ ->
	""
    end,
  ResultString =
    io_lib:format(" -> ~p",[Result]),
  io:format
    ("~saccess ~s using type ~p~s~s~n",
     [TitleString,URI,RequestType,BodyString,ResultString]),
  print_commands(Rest).
  
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

