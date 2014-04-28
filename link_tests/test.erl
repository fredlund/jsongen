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
      case js_links_machine:validate_call_not_error_result(Call,Result) of
	true ->
	  {_,_,{ok,Body}} = http_request(Call),
	  Qid = jsg_jsonschema:propertyValue(Body,"qid"),
	  ExpectedCode =
	    case lists:member(Qid,element(2,State#state.private_state)) of
	      true -> 200;
	      false -> 409
	    end,
	  ResultCode = js_links_machine:http_result_code(Result),
	  if
	    ResultCode == ExpectedCode ->
	      js_links_machine:validate_call_result_body(Call,Result);
	    true -> 
	      io:format
		("~n*** Error: postcondition error: for http call~n~s~nhttp responded with result code ~p, expected result code ~p~n",
		 [js_links_machine:format_http_call(Call),ResultCode,ExpectedCode]),
	      false
	  end;
	_ -> false
      end;
    _ -> Super(State,Call,Result)
  end.
      

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

get_json_body(Result) ->
  mochijson2:decode(js_links_machine:http_body(Result)).



  

