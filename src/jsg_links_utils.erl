-module(jsg_links_utils).

-include("jsongen.hrl").

-export([private_state/1,set_private_state/2,freq_alternatives/2,var/2]).
-export([remove_dynamic_links/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_private_state(NewPrivateState,State) ->
  State#state{private_state=NewPrivateState}.

private_state(State) ->
  State#state.private_state.

remove_dynamic_links(State) ->
  State#state{dynamic_links=jsl_dynamic_links:initialize(20)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

freq_alternatives(Freqs,Alternatives) ->
  FreqAlternatives = 
    lists:map
      (fun (Link) ->
	   RequestType = jsg_links:link_request_type(Link),
	   Href = jsg_links:link_href(Link),
	   freq_comp
	     (Link,
	      binary_to_list(Href),
	      RequestType,
	      Freqs,
	      1)
       end, Alternatives),
  eqc_gen:frequency(FreqAlternatives).

freq_comp(Generator,_String,_,[],Default) ->
  {Default,Generator};
freq_comp(Generator,_String,_RequestType,
	  [{Weight,whatever,""}|_Rest],_Default) ->
  {Weight,Generator};
freq_comp(Generator,_String,RequestType,
	  [{Weight,RequestType,""}|_Rest],_Default) ->
  {Weight,Generator};
freq_comp(Generator,String,RequestType1,
	  [{Weight,RequestType2,First}|Rest],Default) ->
  case {string:str(String,First),
	request_type_match(RequestType1,RequestType2)} of
    {N,true} when N>0 ->
      {Weight,Generator};
    _ ->
      freq_comp(Generator,String,RequestType1,Rest,Default)
  end.

request_type_match(_, whatever) ->
  true;
request_type_match(T1,T2) ->
  T1==T2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

var(Spec,{ok,{link,LinkData}}) ->
  Variables = proplists:get_value(vars,LinkData),
  Var = jsg_jsonschema:propertyValue(Spec,"var"),
  Value = proplists:get_value(list_to_atom(binary_to_list(Var)),Variables),
  Value.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

