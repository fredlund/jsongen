-module(js_links_machine).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

-record(state,{links=[],private_state=void}).


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
  #state{links=Links,private_state=PrivateState}.

command(State) ->
  eqc_gen:oneof
  ([
    {call, ?MODULE, follow_link, [Link]} || Link <- State#state.links
   ]).

callouts(_,_) ->
  ?EMPTY.

precondition(State,Call) ->
  case Call of
    {_, _, follow_link, [Link], _} -> lists:member(Link,State#state.links)
  end.

postcondition(State,Call,Result) ->
  true.

next_state(State,Result,Call) ->
  State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

follow_link(Link) ->
  io:format
    ("will follow ~p~n",
     [Link]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_table(PrivateModule,Links) ->
  case ets:info(js_links_machine_data) of
    true ->
      [{pid,Pid}] = ets:lookup(js_links_machine_data,pid),
      exit(Pid,kill),
      ets:delete(js_links_machine_data);
    undefined ->
      ok
  end,
  spawn
    (fun () ->
	 ets:new(js_links_machine_data,[named_table,public]), wait_forever()
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
	  ?CHECK_COMMANDS({H, DS, Res}, ?MODULE, Cmds,
	  begin
	    pretty_commands(?MODULE, Cmds, {H, DS, Res},
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
