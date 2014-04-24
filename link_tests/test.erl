-module(test).

-compile(export_all).

-record(state,{static_links,links,private_state={false,[]}}).


%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.


initial_state() -> 
  {false,[]}.

link_permitted(Super,State,{link,LD}) ->
  Init = element(1,State#state.private_state),
  case proplists:get_value(title,LD) of
    "reset" ->
      not(Init);
    _ ->
      Init
  end.

next_state(Super,State,Result,Call) ->
  case link_title(Call) of
    "reset" ->
      State#state{links=sets:new(),private_state={true,[]}};
    "add_question" -> 
      JSON = get_json_body(Result),
      Qid = jsg_jsonschema:propertyValue(JSON,"qid"),
      State#state{private_state={true,[Qid|element(2,State#state.private_state)]}};
    _ -> Super(State,Result,Call)
  end.

postcondition(Super,State,Call,Result) ->
  case link_title(Call) of
    "add_answer" ->
      {_,_,{ok,Body}} = http_request(Call),
      Qid = jsg_jsonschema:propertyValue(Body,"qid"),
      ShouldSucceed = lists:member(Qid,element(2,State#state.private_state)),
      case js_links_machine:http_result_type(Result) of
	ok -> 
	  ResultCode = js_links_machine:http_result_code(Result),
	  (ShouldSucceed andalso (ResultCode==200))
	    orelse ((not(ShouldSucceed)) andalso (ResultCode==409));
	_ ->
	  false
      end;
    Other ->
      Super(State,Call,Result)
  end.
      

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

link_title(Call) ->
  {link,LD} = ?MODULE:link(Call),
  proplists:get_value(title,LD).

link_schema(Call) ->
  {link,LD} = ?MODULE:link(Call),
  proplists:get_value(schema,LD).

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



  

