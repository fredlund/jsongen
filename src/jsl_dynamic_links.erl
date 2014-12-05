-module(jsl_dynamic_links).

-compile(export_all).

-record(dlinks,{limit,links,size}).

initialize(Limit) ->
  #dlinks{limit=Limit,links=[],size=0}.

is_empty(Struct) ->
  Struct#dlinks.links==[].

add_link(Link,Struct) ->
  Title = jsg_links:link_title(Link),
  NewSet =
    case lists:keyfind(Title,1,Struct#dlinks.links) of
      false ->
	ordsets:add_element(Link,ordsets:new());
      {_,S} ->
	ordsets:add_element(Link,S)
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
  case lists:keyfind(Title,1,Struct#dlinks.links) of
    false ->
      false;
    {_,S} ->
      ordsets:is_element(Link,S)
  end.

size(Struct) ->
  Struct#dlinks.size.

titles(Struct) ->
  lists:map(fun ({Title,_}) -> Title end, Struct#dlinks.links).

links(Title,Struct) ->
  {_,Set} = lists:keyfind(Title,1,Struct#dlinks.links),
  ordsets:to_list(Set).

links(Struct) ->
  lists:flatmap(fun ({_,Links}) -> Links end, Struct#dlinks.links).
		

      
