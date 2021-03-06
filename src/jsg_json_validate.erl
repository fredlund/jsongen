%% @doc
%% NOT IN USE.
%%
%% This module validates JSON data against JSON Schemas.
%% @author Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund 
%% (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)
%% @copyright 2013 Ángel Herranz, Lars-Ake Fredlund, Sergio Gil

-module(jsg_json_validate).

-export([validate/2]).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.

%% @doc 
%% Validates a JSON value against a JSON Schema.
%% WARNING. The function is not yet finished.
%% Returns true if the schema validates, false if it does not,
%% of maybe (due to unfinished implementation), if the validation 
%% status is unknown.
-spec validate(jsg_json:json_term(),jsg_json:json_term()) -> boolean() | maybe.

validate(Data,Schema) ->
  ?LOG("validate(~p,~n         ~p)~n",[Data,Schema]),
  case jsg_jsonschema:hasType(Schema) of
    true ->
      case jsg_jsonschema:type(Schema) of
	<<"object">> ->
	  case Data of
	    {struct,DataProperties} ->
	      Properties =
		proplists:get_keys(jsg_jsonschema:properties(Schema)),
	      Required =
		jsg_jsonschema:keyword(Schema, "required",[]),
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
		case jsg_jsonschema:keyword(Schema, "additionalProperties", {}) of
		  false -> false;
		  _ -> true
		end,
	      PatternProperties =
		case jsg_jsonschema:keyword(Schema, "patternProperties", {}) of
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
	      MaxScanned = jsg_jsonschema:keyword(Schema,"maximum"),
	      ExcMaxScanned = jsg_jsonschema:keyword(Schema,"exclusiveMaximum",false),
	      MinScanned = jsg_jsonschema:keyword(Schema,"minimum"),
	      ExcMinScanned = jsg_jsonschema:keyword(Schema,"exclusiveMinimum",false),
	      Multiple = jsg_jsonschema:keyword(Schema,"multipleOf",1),
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
	      MaxLength = jsg_jsonschema:keyword(Schema,"maxLength"),
	      MinLength = jsg_jsonschema:keyword(Schema,"minLength"),
	      _Pattern = jsg_jsonschema:keyword(Schema,"pattern"),
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
