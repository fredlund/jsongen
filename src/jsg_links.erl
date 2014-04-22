-module(jsg_links).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

-compile(export_all).

%% Given a set of file corresponding to JSON schemas,
%% traverse the schemas to find (non-relative) link definitions.

collect_links(Files) ->
  lists:flatmap(fun collect_links_from_file/1, Files).

collect_links_from_file(File) ->
  try jsg_jsonschema:read_schema(File) of
      {ok,Schema} -> collect_schema_links(Schema, File)
  catch Class:Reason ->
      Stacktrace = erlang:get_stacktrace(),
      io:format
	("*** Error: could not read schema from file ~p~n",
	 [File]),
      erlang:raise(Class,Reason,Stacktrace)
  end.

collect_schema_links(Schema, File) ->
  %% Find all schemas, and retrieve links
  case jsg_jsonschema:links(Schema) of
    undefined ->
      [];
    Links when is_list(Links) ->
      lists:foldl
	(fun (Link,Ls) ->
	     case jsg_jsonschema:propertyValue(Link,"href") of
	       Value when is_binary(Value) ->
		 case depends_on_object_properties(binary_to_list(Value)) of
		   true -> Ls;
		   false -> [{Link,Schema,File}|Ls]
		 end
	     end
	 end, [], Links)
  end.

depends_on_object_properties(Href) ->
  Template = uri_template:parse(Href),
  lists:any(fun ({var, _, _}) -> true;
		(_) -> false
	    end, Template).

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
  jsg_links:run_statem(test,["question.jsch","answer.jsch","statement.jsch"]).

  
  
		 
		 
		 
	     
      
  
