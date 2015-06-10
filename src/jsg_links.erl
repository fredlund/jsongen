-module(jsg_links).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

-compile(export_all).


%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.

extract_dynamic_links(Link,Term,Object) ->
  _S = link_schema(Link),
  LD = link_def(Link),
  Title = link_title(Link),
  NewHistory = [{Title,Object}|link_history(Link)],
  DynamicLinks =
    case jsg_jsonschema:propertyValue(LD,"targetSchema") of
      undefined ->
	[];
      SchemaDesc ->
	extract_links(Link,SchemaDesc,Term,[],Object,NewHistory)
    end,
  lists:map
    (fun ({link,Props}) -> 
	 {link,
	  [{type,dynamic},{object,Object},{history,NewHistory}|
	   proplists:delete(history,Props)]}
     end, DynamicLinks).

extract_links(FollowedLink,Sch,Term,Pointer,Object,History) ->
  Schema = {struct,Proplist} = get_schema(Sch),
  case proplists:get_value(<<"type">>,Proplist) of
    undefined ->
      %% Could be a union schema; we don't handle this yet
      [];

    <<"object">> ->
      Links = js_links_machine:collect_schema_links(Sch,true),
      ShallowLinks =
	lists:flatmap
	  (fun (Link={link,Props}) ->
	       Href = link_href(Link),
	       Template = uri_template:parse(binary_to_list(Href)),
	       %% We should handle relative URIs here...
	       try uri_template:sub({FollowedLink,Object,lists:reverse(Pointer)},Template) of
		   CHREF ->
		   ComposedCHREF = composed_uri(CHREF,Link,FollowedLink,Object),
		   [{link,[{calculated_href,list_to_binary(ComposedCHREF)}|Props]}]
	       catch _:_ ->
		   io:format
		     ("*** Warning: skipping link ~p due to problems "++
			"resolving href~n",
		      [link_title(Link)]),
		   []
	       end
	   end,
	   Links),
      DeepLinks =
	extract_links_from_subterms
	  (FollowedLink,Schema,Term,Pointer,Object,History),
      ShallowLinks++DeepLinks;

    <<"array">> ->
      case proplists:get_value(<<"items">>,Proplist) of
	ItemSchemaDesc={struct,_} ->
	  {_,Result} =
	    lists:foldl
	      (fun (SubItem,{Index,Acc}) ->
		   SubItemLinks =
		     extract_links
		       (FollowedLink,ItemSchemaDesc,SubItem,[Index|Pointer],
			Object,History),
		   {Index+1,SubItemLinks++Acc}
	       end,
	       {0,[]},
	       Term),
	  Result;
	_ -> []
      end;
    
    _Other -> []
  end.

extract_links_from_subterms(FollowedLink,{struct,Proplist},Term,Pointer,Object,History) ->
  case proplists:get_value(<<"properties">>,Proplist) of
    undefined -> [];
    {struct,Properties} ->
      lists:flatmap
	(fun ({Property,Def}) ->
	     {struct,Props} = Term,
	     case proplists:get_value(Property,Props) of
	       undefined ->
		 [];
	       SubProp ->
		 extract_links
		   (FollowedLink,Def,SubProp,[binary_to_list(Property)|Pointer],Object,History)
	     end
	 end, Properties)
  end.

intern_object(Term) ->
  %% This is far from process safe...
  case jsg_store:get({term,Term}) of
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
      jsg_store:put({term,Term},Counter),
      jsg_store:put({object,Counter},Term),
      jsg_store:put(object_counter,Counter+1),
      Counter
  end.

composed_uri(Ref,_Link,FollowedLink,_Object) ->
  %%io:format("~nRef is ~p~nLink is ~p~nFollowedLink=~p~nObject=~p~n~n",[Ref,Link,FollowedLink,Object]),
  {ok,Node} = jsg_store:get(java_node),
  RefURI = java:new(Node,'java.net.URI',[Ref]),
  Calculated_Ref = binary_to_list(jsg_links:link_calculated_href(FollowedLink)),
  PreviousURI = java:new(Node,'java.net.URI',[Calculated_Ref]),
  case java:call(RefURI,isAbsolute,[]) of
    true ->
      Ref;
    false ->
      Result =
	java:string_to_list
	  (java:call
	     (java:call(PreviousURI,resolve,[RefURI]),
	      toString,
	      [])),
      Result
  end.

get_schema(Value={struct,_Proplist}) ->
  get_schema(Value,{struct,[]});
get_schema([Child,Root]) ->
  get_schema(Child,Root).

get_schema(Value={struct,Proplist},Root) ->
  case proplists:get_value(<<"$ref">>,Proplist) of
    undefined ->
      Value;
    _Ref ->
      %%io:format("Ref is ~p Root is~n~p~n",[_Ref,Root]),
      jsg_jsonref:unref(Value,Root)
  end.

make_schema(Schema,Parent) ->
  case is_parent_relative(Schema) of
    true ->
      [Schema,Parent];
    false ->
      Schema
  end.

is_parent_relative({struct,Proplist}) ->
  case proplists:get_value(<<"$ref">>,Proplist) of
    undefined ->
      false;
    Ref ->
      case binary_to_list(Ref) of
	[$#|_] -> true;
	_ -> false
      end
  end.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

link_def(Link) ->
  Schema = link_schema(Link),
  RootSchema = {struct,[]},
  {struct,SchemaDef} = get_schema(Schema,RootSchema),
  N = link_num(Link),
  Links = proplists:get_value(<<"links">>,SchemaDef),
  %%io:format("Links are ~p~n",[Links]),
  lists:nth(N,Links).

link_title(Link) ->
  {struct,LinkDef} = link_def(Link),
  case proplists:get_value(<<"title">>,LinkDef) of
    L when is_binary(L) ->
      binary_to_list(L);
    Other ->
      Other
  end.

print_link(Link={link,LD}) ->
  {struct,LinkDef} = link_def(Link),
  LinkTitle = 
    case proplists:get_value(<<"title">>,LinkDef) of
      L when is_binary(L) ->
	binary_to_list(L);
      Other ->
	Other
    end,
  Href =
    proplists:get_value(<<"href">>,LinkDef),
  Schema =
    proplists:get_value(schema,LD),
  Object =
    proplists:get_value(object,LD),
  {link,[{title,LinkTitle},{href,Href},{schema,Schema},{object,Object}]}.

link_request_type(Link) ->
  {struct,LinkDef} = link_def(Link),
  case proplists:get_value(<<"method">>,LinkDef) of
    undefined -> get;
    Other -> list_to_atom(string:to_lower(binary_to_list(Other)))
  end.

link_href(Link) ->
  {struct,LinkDef} = link_def(Link),
  proplists:get_value(<<"href">>,LinkDef).

link_targetSchema(Link) ->
  {struct,LinkDef} = link_def(Link),
  proplists:get_value(<<"targetSchema">>,LinkDef).

link_schema(Link) ->
  {link,LD} = Link,
  proplists:get_value(schema,LD).

link_num(Link) ->
  {link,LD} = Link,
  proplists:get_value(link,LD).

link_history(Link) ->
  {link,LD} = Link,
  proplists:get_value(history,LD,[]).

link_calculated_href(Link) ->	     
  {link,LD} = Link,
  proplists:get_value(calculated_href,LD,[]).

link_type(Link) ->	     
  {link,LD} = Link,
  proplists:get_value(type,LD,[]).
      
  
