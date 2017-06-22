-module(jsl_dynamic_links).

-export([ add_link/2
	, initialize/1
	, is_element/2
	, is_empty/1
	, get_link/1
	, size/1
	, titles/1
	, links/1, links/2
	]).

-record(dlinks,{limit,links,size}).

initialize(Limit) ->
  #dlinks{limit=Limit,links=[],size=0}.

is_empty(Struct) ->
  Struct#dlinks.links==[].

intern_link(Link) ->
  %% This is far from process safe...
  case jsg_store:get({link,Link}) of
    {ok,N} -> N;
    _ -> 
      Counter =
	case jsg_store:get(object_counter) of
	  {ok,Cnt} ->
	    Cnt;
	  _ ->
	    jsg_store:put(object_counter,0),
	    0
	end,
      jsg_store:put({link,Link},Counter),
      jsg_store:put({reverse_link,Counter},Link),
      jsg_store:put(object_counter,Counter+1),
      Counter
  end.

get_link(Counter) ->
  {_,Link} = jsg_store:get({reverse_link,Counter}),
  Link.

add_link(Link,Struct) ->
  Title = jsg_links:link_title(Link),
  LinkCounter = intern_link(Link),
  NewSet =
    case lists:keyfind(Title,1,Struct#dlinks.links) of
      false ->
	ordsets:add_element(LinkCounter,ordsets:new());
      {_,S} ->
	ordsets:add_element(LinkCounter,S)
    end,
  {AdjustedSet,NewSize} =
    begin
      Size = ordsets:size(NewSet),
      case Size>Struct#dlinks.limit of
	true ->
	  List = ordsets:to_list(NewSet),
	  Delete = eqc_gen:pick(eqc_gen:choose(1,Size)),
	  {Before,[_|After]} = lists:split(Delete-1,List),
	  {ordsets:from_list(Before++After),Struct#dlinks.limit};
	false ->
	  {NewSet,Size}
      end
    end,
  Struct#dlinks
    {links=lists:keystore(Title,1,Struct#dlinks.links,{Title,AdjustedSet}),
     size=NewSize}.

is_element(Link,Struct) ->
  Title = jsg_links:link_title(Link),
  Counter = intern_link(Link),
  case lists:keyfind(Title,1,Struct#dlinks.links) of
    false ->
      false;
    {_,S} ->
      ordsets:is_element(Counter,S)
  end.

size(Struct) ->
  Struct#dlinks.size.

titles(Struct) ->
  lists:map(fun ({Title,_}) -> Title end, Struct#dlinks.links).

links(Title,Struct) ->
  {_,Set} = lists:keyfind(Title,1,Struct#dlinks.links),
  lists:map(fun get_link/1, ordsets:to_list(Set)).

links(Struct) ->
  lists:map
    (fun get_link/1,
     lists:flatmap(fun ({_,Links}) -> Links end, Struct#dlinks.links)).
		

      
