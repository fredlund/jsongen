-module(qa).

-compile(export_all).

-include_lib("jsongen/include/jsongen.hrl").

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.


initial_state() -> {false,[]}.

link_permitted(Super,State,Link) ->
  HasReset = element(1,js_links_machine:private_state(State)),
  case jsg_links:link_title(Link) of
    "reset" -> not(HasReset);
    _ -> HasReset
  end.

next_state(Super,State,Result,Call) ->
  case js_links_machine:call_link_title(Call) of
    "reset" ->
      js_links_machine:set_private_state({true,[]},State);
    "add_question" -> 
      JSON = js_links_machine:get_json_body(Result),
      Qid = jsg_jsonschema:propertyValue(JSON,"qid"),
      OldQids = element(2,js_links_machine:private_state(State)),
      Super
      (js_links_machine:set_private_state({true,[Qid|OldQids]},State),
       Result,
       Call);
    _ -> 
      Super(State,Result,Call)
  end.

postcondition(Super,State,Call,Result) ->
  case js_links_machine:call_link_title(Call) of
    "add_answer" ->
      case js_links_machine:validate_call_not_error_result(Call,Result) of
	true ->
	  CallBody = js_links_machine:json_call_body(Call),
	  Qid = jsg_jsonschema:propertyValue(CallBody,"qid"),
          PrivateState = js_links_machine:private_state(State),
          QidExists = lists:member(Qid,element(2,PrivateState)),
	  ExpectedCode = if QidExists -> 200; true -> 409 end,
	  ResultCode = js_links_machine:http_result_code(Result),
	  if
	    ResultCode == ExpectedCode ->
	      js_links_machine:validate_call_result_body(Call,Result);
	    true -> false
	  end;
	_ -> false
      end;
    _ -> Super(State,Call,Result)
  end.



  

