-module(test).

-compile(export_all).

-record(state,{static_links,links,private_state=void}).

initial_state() -> 
  [].

next_state(Super,State,Result,Call) ->
  case link_title(Call) of
    "reset" ->
      State#state{links=sets:new(),private_state=initial_state()};
    "add_question" -> 
      JSON = get_json_body(Result),
      Qid = jsg_jsonschema:propertyValue(JSON,"qid"),
      State#state{private_state=[Qid|State#state.private_state]};
    _ -> Super(State,Result,Call)
  end.

postcondition(Super,State,Call,Result) ->
  case link_title(Call) of
    "add_answer" ->
      io:format("In add_answer postcondition~n",[]),
      {_,_,{ok,Body}} = http_request(Call),
      io:format("Body is ~p~n",[Body]),
      io:format("State is ~p~n",[State#state.private_state]),
      io:format("Result is ~p~n",[Result]),
      Qid = jsg_jsonschema:propertyValue(Body,"qid"),
      ShouldSucceed = lists:member(Qid,State#state.private_state),
      case Result of
	{normal,{Code,ResultBody}} ->
	  (ShouldSucceed andalso (Code==200))
	    orelse ((not(ShouldSucceed)) andalso (Code==409))
      end;
    Other ->
      Super(State,Call,Result)
  end.
      
link_title(Call) ->
  {link,LD} = ?MODULE:link(Call),
  proplists:get_value(title,LD).

http_request(Call) ->
  case Call of
    {_, _, follow_link, [_,Request], _} ->
      Request
  end.

link(Call) ->
  case Call of
    {_, _, follow_link, [Link={link,LD},_], _} ->
      Link
  end.

get_json_body({normal,{_,Body}}) ->
  mochijson2:decode(Body).



  

