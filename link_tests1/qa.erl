-module(qa).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("jsongen/include/jsongen.hrl").

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.

initial_state() -> void.

compose_alternatives(_,State,Alternatives) ->
  freq_alternatives
    ([{3,whatever,"/q/"},
      {3,whatever,"/a/"},
      {5,post,"/a"},
      {5,post,"/q"},
      {1,whatever,"/q"},
      {1,whatever,"/a"}],
     Alternatives).

link_permitted(Super,State,Link) ->
  case jsg_links:link_title(Link) of
    "reset" -> jsg_links_utils:private_state(State)==void;
    _ -> true
  end.

next_state(Super,State,Result,Call) ->
  case js_links_machine:call_link_title(Call) of
    "reset" -> 
      Super
      (jsg_links_utils:set_private_state([],State),
       Result,
       Call);
    "add_question" -> 
      JSON = js_links_machine:get_json_body(Result),
      Qid = jsg_jsonschema:propertyValue(JSON,"qid"),
      OldQids = js_links_machine:private_state(State),
      Super
      (jsg_links_utils:set_private_state([Qid|OldQids],State),
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
          PrivateState = jsg_links_utils:private_state(State),
          QidExists = lists:member(Qid,PrivateState),
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

