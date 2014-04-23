-module(jsg_links).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

-compile(export_all).

-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.

%% Given a set of file corresponding to JSON schemas,
%% traverse the schemas to find (non-relative) link definitions.

collect_links(Files) ->
  lists:flatmap(fun collect_links_from_file/1, Files).

collect_links_from_file(File) ->
  try jsg_jsonschema:read_schema(File) of
      {ok,Schema} -> collect_schema_links(Schema,false)
  catch Class:Reason ->
      Stacktrace = erlang:get_stacktrace(),
      io:format
	("*** Error: could not read schema from file ~p~n",
	 [File]),
      erlang:raise(Class,Reason,Stacktrace)
  end.

collect_schema_links(Schema, DependsOnObject) ->
  %% Find all schemas, and retrieve links
  case jsg_jsonschema:links(Schema) of
    undefined ->
      [];
    Links when is_list(Links) ->
      lists:foldl
	(fun (Link,Ls) ->
	     case jsg_jsonschema:propertyValue(Link,"href") of
	       Value when is_binary(Value) ->
		Dependency =
		   depends_on_object_properties(binary_to_list(Value)),
		 if
		   Dependency==DependsOnObject ->
		     LinkData =
		       case jsg_jsonschema:propertyValue(Link,"title") of
			 undefined -> [];
			 Title -> [{title,binary_to_list(Title)}]
		       end,
		     [{link,[{link,Link},{schema,Schema}|LinkData]}|Ls];
		   true ->
		     Ls
		 end
	     end
	 end, [], Links)
  end.

depends_on_object_properties(Href) ->
  Template = uri_template:parse(Href),
  lists:any(fun ({var, _, _}) -> true;
		(_) -> false
	    end, Template).

compute_uri(Link={link,LinkData}) ->
  L = proplists:get_value(link,LinkData),
  Href = jsg_jsonschema:propertyValue(L,"href"),
  Template = uri_template:parse(binary_to_list(Href)),
  Variables = 
    case proplists:get_value(object,LinkData) of
      undefined ->
	[];
      {struct,Proplist} ->
	lists:map
	  (fun ({Key,Value}) ->
	       {list_to_atom(binary_to_list(Key)),Value}
	   end, Proplist)
    end,
  ?LOG("Variables are ~p~n",[Variables]),
  uri_template:sub(Variables,binary_to_list(Href)).

generate_argument(Link={link,LinkData}) ->
  L = proplists:get_value(link,LinkData),
  S = proplists:get_value(schema,LinkData),
  case jsg_jsonschema:propertyValue(L,"schema") of
    undefined ->
      undefined;
    ArgumentSchema ->
      Schema = get_schema(ArgumentSchema,S),
      Gen = jsongen:json(Schema),
      {ok,eqc_gen:pick(Gen)}
  end.

request_type(Link={link,LinkData}) ->
  L = proplists:get_value(link,LinkData),
  RequestType = jsg_jsonschema:propertyValue(L,"method"),
  case RequestType of
    undefined -> get;
    Other -> list_to_atom(string:to_lower(binary_to_list(Other)))
  end.

extract_dynamic_links(Link={link,LinkData},JSONBody) ->
  L = proplists:get_value(link,LinkData),
  S = proplists:get_value(schema,LinkData),
  case jsg_jsonschema:propertyValue(L,"targetSchema") of
    undefined ->
      [];
    SchemaDesc ->
      %%io:format("Schema is ~p~n",[SchemaDesc]),
      Schema = {struct,Proplist} = get_schema(SchemaDesc,S),
      case proplists:get_value(<<"type">>,Proplist) of
	undefined ->
	  %% Could be a union schema; we don't handle this yet
	  throw(bad);
	Type ->
	  case Type of
	    <<"object">> ->
	      Links = collect_schema_links(Schema,true),
	      %%io:format("schema links are:~n~p~n",[Links]),
	      lists:map
		(fun ({link,Props}) -> {link,[{object,JSONBody}|Props]} end,
		 Links);
	    <<"array">> ->
	      case proplists:get_value(<<"additionalItems">>,Proplist) of
		false ->
		  ItemSchemaDesc = proplists:get_value(<<"items">>,Proplist),
		  %%io:format("itemSchema is ~p~n",[ItemSchemaDesc]),
		  ItemSchema = get_schema(ItemSchemaDesc,S),
		  Links = collect_schema_links(ItemSchema,true),
		  %%io:format("schema links are:~n~p~n",[Links]),
		  lists:flatmap
		    (fun ({link,Props}) ->
			 lists:map
			   (fun (Item) ->
				{link,[{object,Item}|Props]}
			    end, JSONBody)
		     end, Links)
	      end
	  end
      end
  end.

get_schema(Value={struct,Proplist},Root) ->
  case proplists:get_value(<<"$ref">>,Proplist) of
    undefined ->
      Value;
    Ref ->
      %%io:format("Ref is ~p Root is~n~p~n",[Ref,Root]),
      jsg_jsonref:unref(Value,Root)
  end.

run_statem(PrivateModule,Files) ->
  case collect_links(Files) of
    [] ->
      io:format
	("*** Error: no independent links could be found among the files ~p~n",
	 [Files]),
      throw(bad);
    Links ->
      js_links_machine:init_table(PrivateModule,Links),
      js_links_machine:test()
  end.

test() ->
  jsg_links:run_statem(test,["question.jsch","answer.jsch","statement.jsch","reset.jsch","answer_reply.jsch"]).

  
  
		 
		 
		 
	     
      
  
