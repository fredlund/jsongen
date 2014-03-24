-module(jsg_json_validate).

-export([validate/2]).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.

-spec validate(json:json_term(),json:json_term()) -> boolean() | maybe.

validate(Data,Schema) ->
  ?LOG("validate(~p,~n         ~p)~n",[Data,Schema]),
  case jsonschema:hasType(Schema) of
    true ->
      case jsonschema:type(Schema) of
	<<"object">> ->
	  case Data of
	    {struct,DataProperties} ->
	      Properties =
		proplists:get_keys(jsonschema:properties(Schema)),
	      Required =
		jsonschema:keyword(Schema, "required",[]),
	      RequiredSet =
		if
		  Required==undefined -> sets:new();
		  true -> sets:from_list(Required)
		end,
	      PropertiesSet =
		if
		  Properties==undefined -> sets:new();
		  true -> sets:from_list(Properties)
		end,
	      AdditionalProperties =
		case jsonschema:keyword(Schema, "additionalProperties", {}) of
		  false -> false;
		  _ -> true
		end,
	      PatternProperties =
		case jsonschema:keyword(Schema, "patternProperties", {}) of
		  false -> false;
		  _ -> true
		end,
	      DataPropertiesSet =
		sets:from_list(proplists:get_keys(DataProperties)),
	      RequiredPresent =
		sets:is_subset(RequiredSet,DataPropertiesSet),
	      Additionals =
		sets:subtract(DataPropertiesSet,PropertiesSet),
	      AdditionalsSize =
		sets:size(Additionals),
	      if
		not(RequiredPresent) -> false;
                %% TODO: do not ignore patternProperties
		not(AdditionalProperties), AdditionalsSize>0, not(PatternProperties) -> false;
		true -> maybe
	      end;
	    _ -> false
	  end;

	<<"integer">> ->
	  case is_integer(Data) of
	    false ->
	      false;
	    true ->
	      MaxScanned = jsonschema:keyword(Schema,"maximum"),
	      ExcMaxScanned = jsonschema:keyword(Schema,"exclusiveMaximum",false),
	      MinScanned = jsonschema:keyword(Schema,"minimum"),
	      ExcMinScanned = jsonschema:keyword(Schema,"exclusiveMinimum",false),
	      Multiple = jsonschema:keyword(Schema,"multipleOf",1),
	      if
		MaxScanned=/=undefined, Data>MaxScanned ->
		  false;
		MaxScanned=/=undefined, ExcMaxScanned==true, Data==MaxScanned ->
		  false;
		MinScanned=/=undefined, Data<MinScanned ->
		  false;
		MinScanned=/=undefined, ExcMinScanned==true, Data==MinScanned ->
		  false;
		Multiple=/=undefined, (Data rem Multiple)=/=0 ->
		  false;
		true ->
		  true
	      end
	  end;

	<<"string">> ->
	  try binary_to_list(Data) of
	      String ->
	      MaxLength = jsonschema:keyword(Schema,"maxLength"),
	      MinLength = jsonschema:keyword(Schema,"minLength"),
	      _Pattern = jsonschema:keyword(Schema,"pattern"),
	      Length = length(String),
	      if
		MaxLength=/=undefined, Length>MaxLength -> false;
		MinLength=/=undefined, Length<MinLength -> false;
		true -> maybe
                %%% mejorar el validador aqui arriba. Intentar quitar el 'maybe'
	      end
	  catch _:_ -> false end;
	_ -> maybe
      end;
    false -> maybe
  end. 
