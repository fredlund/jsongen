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
      Qid = binary_to_list(jsg_jsonschema:propertyValue(JSON,"qid")),
      State#state{private_state=[Qid|State#state.private_state]};
    _ -> Super(State,Result,Call)
  end.

post_condition(Super,State,Call,Result) ->
  case link_title(Call) of
    "add_answer" ->
      {_,_,Body} = http_request(Call),
      Qid = jsg_jsonschema:propertyValue(Body,"qid"),
      ShouldSucceed = lists:member(Qid,State#state.private_data),
      case AResult of
	{normal,{Code,ResultBody}} ->
	  (ShouldSucceed andalso (Code==200))
	    orelse ((not(ShouldSucceed)) andalso (Code==409))
      end;
    _ -> Super(State,Call,Result)
  end.
      
link_title(Call) ->
  {link,LD} = link(Call),
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
  to_json(Body).


  

