-module(jsg_links).

-compile(export_all).

%% Given a set of file corresponding to JSON schemas,
%% traverse the schemas to find (non-relative) link definitions.

collect_links(Files) ->
  lists:flatmap(fun collect_links_from_file/1, Files).

collect_links_from_file(File) ->
  {ok,Schema} = jsg_jsonschema:read_schema(File),
  collect_schema_links(Schema, File).

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
		   false -> [{Value,File}|Ls]
		 end
	     end
	 end, [], Links)
  end.

depends_on_object_properties(Href) ->
  Template = uri_template:parse(Href),
  lists:any(fun ({var, _, _}) -> true;
		(_) -> false
	    end, Template).


		 
		 
		 
	     
      
  
