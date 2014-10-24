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


%% Given a set of file corresponding to JSON schemas,
%% traverse the schemas to find (non-relative) link definitions.

compute_uri(Link={link,LinkData}) ->
  Href = link_href(Link),
  Template = uri_template:parse(binary_to_list(Href)),
  Variables = 
    lists:filter
      (fun ({_,Value}) -> is_integer(Value) orelse is_binary(Value) end,
       proplists:get_value(vars,LinkData)),
  %%io:format("compute_uri: variables are~n~p~n",[Variables]),
  uri_template:sub(Variables,binary_to_list(Href)).

generate_argument(Link) ->
  jsg_store:put(eqc_gen_context,Link),
  S = link_schema(Link),
  Sch = link_def(Link),
  Schema = 
    case jsg_jsonschema:propertyValue(S,"schema") of
      undefined ->
	undefined;
      Sch ->
	get_schema(Sch,S)
    end,
  QuerySchema = 
    case jsg_jsonschema:propertyValue(S,"querySchema") of
      undefined ->
	undefined;
      QSch ->
	get_schema(QSch,S)
    end,
  RequestType = link_request_type(Link),
  Body = 
    case may_have_body(RequestType) of
      true when Schema=/=undefined -> 
	BodyGen = jsongen:json(Schema),
	{ok,eqc_gen:pick(BodyGen)};
      _ -> 
	undefined
    end,
  QueryParameters =
    case may_have_body(RequestType) of
      true when QuerySchema=/=undefined ->
	QGen = jsongen:json(QuerySchema),
	{ok,eqc_gen:pick(QGen)};
      false when QuerySchema=/=undefined ->
	QGen = jsongen:json(QuerySchema),
	{ok,eqc_gen:pick(QGen)};
      false when Schema=/=undefined ->
	QGen = jsongen:json(Schema),
	{ok,eqc_gen:pick(QGen)};
      _ ->
	undefined
    end,
  {Body,QueryParameters}.

may_have_body(get) ->
  false;
may_have_body(delete) ->
  false;
may_have_body(_) ->
  true.

extract_dynamic_links(Link,JSONBody) ->
  S = link_schema(Link),
  LD = link_def(Link),
  V = link_vars(Link),
  Title = link_title(Link),
  NewHistory = [Title|link_history(Link)],
  %%io:format("extract_dynamic_links(~p)~nSch=~p~n",[Link,LD]),
  Result =
    case jsg_jsonschema:propertyValue(LD,"targetSchema") of
      undefined ->
	[];
      SchemaDesc ->
	extract_links
	  (SchemaDesc,JSONBody,V,NewHistory)
    end,
  %%io:format
    %%("extract_links(~p) from~n~p~nyields~n~p~n",
    %%[Link,JSONBody,Result]),
  lists:map
    (fun ({link,Props}) -> 
	 case proplists:get_value(history,Props) of
	   undefined ->
	     {link,[{history,NewHistory}|Props]};
	   OldHistory ->
	     {link,[{history,NewHistory}|proplists:delete(history,Props)]}
	 end
     end, Result).

extract_links(Sch,Term,V,History) ->
  %%io:format("~nSchema is ~p; Term=~n~p~n",[Sch,Term]),
  Schema = {struct,Proplist} = get_schema(Sch),
  case proplists:get_value(<<"type">>,Proplist) of
    undefined ->
      %% Could be a union schema; we don't handle this yet
      [];

    <<"object">> ->
      Links = js_links_machine:collect_schema_links(Sch,true,V),
      NewVars = update_vars(Term,V),
      EnvVars = lists:map(fun ({Key,_}) -> Key end, NewVars),
      ShallowLinks =
	lists:flatmap
	  (fun (Link={link,Props}) ->
	       NewProps = [{object,Term}|Props],
	       NewLink = {link,NewProps},
	       Href = link_href(Link),
	       Template = uri_template:parse(binary_to_list(Href)),
	       TemplateVars = 
		 lists:filter(fun (T) ->
				  case T of 
				    {var,_,_} -> true;
				    _ -> false
				  end
			      end, Template),
	       case check_vars_exists(TemplateVars,EnvVars,NewLink,History) of
		 true ->
		   [{link,[{vars,NewVars}|proplists:delete(vars,NewProps)]}];
		 false ->
		   []
	       end
	   end,
	   Links),
      %%io:format("Shallow links are:~n~p~n",[ShallowLinks]),
      DeepLinks = extract_links_from_subterms(Schema,Term,V,History),
      %%io:format("Deep links are:~n~p~n",[DeepLinks]),
      ShallowLinks++DeepLinks;
	
    <<"array">> ->
      case proplists:get_value(<<"items">>,Proplist) of
	ItemSchemaDesc={struct,_} ->
	  lists:flatmap
	    (fun (SubItem) ->
		 extract_links(ItemSchemaDesc,SubItem,V,History)
	     end,
	     Term);
	_ -> []
      end;
    
    _Other -> []
  end.

extract_links_from_subterms({struct,Proplist},Term,V,History) ->
  case proplists:get_value(<<"properties">>,Proplist) of
    undefined -> [];
    {struct,Properties} ->
      lists:flatmap
	(fun ({Property,Def}) ->
	     %%io:format("Property: ~p~n",[Property]),
	     {struct,Props} = Term,
	     %%io:format("Term props are ~p~n",[Props]),
	     case proplists:get_value(Property,Props) of
	       undefined ->
		 [];
	       SubProp ->
		 %%io:format("SubProp is ~p~n",[SubProp]),
		 extract_links(Def,SubProp,V,History)
	     end
	 end, Properties)
  end.

update_vars(Object,OldVars) ->
  NewVars =
    case Object of
      {struct,Proplist} ->
	lists:foldl
	  (fun ({Key,Value},Acc) ->
	       AtomKey = list_to_atom(binary_to_list(Key)),
	       [{AtomKey,Value}|proplists:delete(AtomKey,Acc)]
	   end, OldVars, Proplist)
    end,
  %%io:format
    %%("new_vars: from ~p and object~n~p~ncomputes ~p~n",
     %%[OldVars,Object,NewVars]),
  NewVars.

check_vars_exists([],_EnvVars,_Link,_History) ->
  true;
check_vars_exists([{var,Name,_}|LinkVars],EnvVars,Link,History) ->
  case not(lists:member(Name,EnvVars)) of
    true ->
      io:format
	("*** Warning: variable ~p not found when generating link~n~p~n"++
	   "Variables=~p~n"++
	   "Link history: ~p~n"++
	   "*** Warning: Not generating link.~n~n",
	 [Name,jsg_links:print_link(Link),EnvVars,lists:reverse(History)]),
      false;
    false ->
      check_vars_exists(LinkVars,EnvVars,Link,History)
  end.

get_schema(Value={struct,Proplist}) ->
  get_schema(Value,{struct,[]});
get_schema([Child,Root]) ->
  get_schema(Child,Root).

get_schema(Value={struct,Proplist},Root) ->
  case proplists:get_value(<<"$ref">>,Proplist) of
    undefined ->
      Value;
    Ref ->
      %%io:format("Ref is ~p Root is~n~p~n",[Ref,Root]),
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

link_schema(Link) ->
  {link,LD} = Link,
  proplists:get_value(schema,LD).

link_num(Link) ->
  {link,LD} = Link,
  proplists:get_value(link,LD).

link_vars(Link) ->
  {link,LD} = Link,
  proplists:get_value(vars,LD).

link_history(Link) ->
  {link,LD} = Link,
  proplists:get_value(history,LD,[]).

	     
      
  
