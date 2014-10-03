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
  Freqs=
    lists:map
      (fun (Gen={call, _, follow_link, [Link={link,LinkData},_]}) ->
	   RequestType = jsg_links:request_type(Link),
	   L = proplists:get_value(link,LinkData),
	   Href = jsg_jsonschema:propertyValue(L,"href"),
	   freq_comp
	     (Gen,
	      binary_to_list(Href),
	      RequestType,
	      [{3,whatever,"/q/"},
	       {3,whatever,"/a/"},
	       {5,post,"/a"},
	       {5,post,"/q"},
	       {1,whatever,"/q"},
	       {1,whatever,"/a"}],
	      1)
       end, Alternatives),
  %%io:format("Result=~n~p~n",[Freqs]),
  eqc_gen:frequency(Freqs).
	      
link_permitted(Super,State,Link) ->
  case jsg_links:link_title(Link) of
    "reset" -> js_links_machine:private_state(State)==void;
    _ -> true
  end.

freq_comp(Generator,String,_,[],Default) ->
  {Default,Generator};
freq_comp(Generator,String,RequestType1,
	  [{Weight,RequestType2,First}|Rest],Default) ->
  case {string:str(String,First),request_type_match(RequestType1,RequestType2)} of
    {N,true} when N>0 ->
      {Weight,Generator};
    _ ->
      freq_comp(Generator,String,RequestType1,Rest,Default)
  end.

request_type_match(_, whatever) ->
  true;
request_type_match(T1,T2) ->
  T1==T2.

funItem(Arg,Context) ->
  %%io:format
    %%("eqc generator called with context=~n~p~n and Arg=~p~n",
     %%[Context,Arg]),
  <<"hola">>.

next_state(Super,State,Result,Call) ->
  case js_links_machine:call_link_title(Call) of
    "reset" -> 
      Super
      (js_links_machine:set_private_state([],State),
       Result,
       Call);
    "add_question" -> 
      JSON = js_links_machine:get_json_body(Result),
      Qid = jsg_jsonschema:propertyValue(JSON,"qid"),
      OldQids = js_links_machine:private_state(State),
      Super
      (js_links_machine:set_private_state([Qid|OldQids],State),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

var(Spec,{ok,{link,LinkData}}) ->
  Variables = proplists:get_value(vars,LinkData),
  Var = jsg_jsonschema:propertyValue(Spec,"var"),
  Value = proplists:get_value(list_to_atom(binary_to_list(Var)),Variables),
  Value.
