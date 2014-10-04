%% @doc This module contains functions for parsing a JSON schema.
%% @author Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund 
%% (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)
%% @copyright 2013 Ángel Herranz, Lars-Ake Fredlund, Sergio Gil

-module(jsg_jsonschema).

-export([read_schema/1]).

-compile(export_all).

%% @doc
%% Reads a JSON schema in textual format, converting it into
%% a mochijson2 Erlang term. 
%% The function argument can either 
%% be on the form "http:...", "file:..." or a filename.
-spec read_schema(string()) -> {ok, jsg_json:json_term()} | {error, any()}.
read_schema(URL) ->
  jsg_json:decode_url(URL).

is_object({struct,_}) ->
  true;
is_object(_) ->
  false.

hasType(_Schema={struct,Def}) ->
  case proplists:lookup(<<"type">>,Def) of
      {_,_Type} ->
          true;
      none ->
          false
  end;
hasType(_Other) ->
  throw(bad).

schemaType(Schema) ->
  case oneOf(Schema) of
    undefined ->
      case anyOf(Schema) of
	undefined ->
	  case allOf(Schema) of
	    undefined ->
	      case notKeyword(Schema) of
		undefined ->
		  case isRef(Schema) of
		    false ->  
		      case hasType(Schema) of
			false ->
			  case hasEnum(Schema) of
			    false ->
			      case hasQuickCheck(Schema) of
				false -> 'error';
				_ -> 'quickcheck'
			      end;
			    _ -> 'enum'
			  end;
			_ -> 'type'
		      end;
		    _ -> 'ref'
		  end;
		_ -> 'not'
	      end;
	    _ -> 'allOf'
	  end;
	_ -> 'anyOf'
      end;
    _ -> 'oneOf'
  end.

hasEnum(_Schema={struct,Def}) ->
  case proplists:lookup(<<"enum">>,Def) of
    {_,_Type} ->
      true;
    none ->
      false
  end.

hasQuickCheck(_Schema={struct,Def}) ->
  case proplists:lookup(<<"quickcheck">>,Def) of
    {_,_Type} ->
      true;
    none ->
      false
  end.

isRef(Schema) ->
    URL = keyword(Schema,"$ref"),
    URL =/= undefined.

anyOf(_Schema = {struct,Def}) ->
  case proplists:lookup(<<"anyOf">>,Def) of
    {_,Schemas} ->
      Schemas;
    none ->
      undefined
  end.

oneOf(_Schema = {struct,Def}) ->
  case proplists:lookup(<<"oneOf">>,Def) of
    {_,Schemas} ->
      Schemas;
    none ->
      undefined
  end.

allOf(_Schema = {struct,Def}) ->
  case proplists:lookup(<<"allOf">>,Def) of
    {_,Schemas} ->
      Schemas;
    none ->
      undefined
  end.

notKeyword(_Schema = {struct,Def}) ->
  case proplists:lookup(<<"not">>,Def) of
    {_,Schemas} ->
      Schemas;
    none ->
      undefined
  end.

type(_Schema={struct, Def}) ->
    {_,Type} = proplists:lookup(<<"type">>,Def),
    Type.

set_type(_Schema={struct, Def},Type) ->
    DefNoType = proplists:delete(<<"type">>,Def),
    {struct, [{<<"type">>,Type} | DefNoType]}.

enumerated(_Schema={struct, Def}) ->
    {_,Enumerated} = proplists:lookup(<<"enum">>,Def),
    Enumerated.

items(_Schema={struct, Def}) ->
    case proplists:lookup(<<"items">>,Def) of
        {_,Items} ->
            case Items of
                {struct, _} -> {itemSchema, Items};
       
                _ -> {error, bad_items_schema}
            end;
        none -> 
            {empty, no_items}
    end.

properties(_Schema = {struct, Def}) ->
    case proplists:lookup(<<"properties">>,Def) of
        {_,{struct, Properties}} ->
            Properties;
        none ->
            []
    end.

minProperties(Schema,Def) ->
    keyword(Schema,"minProperties",Def).


maxProperties(Schema,Def) ->
    keyword(Schema,"maxProperties",Def).

maxProperties(Schema) ->
    keyword(Schema,"maxProperties").

keyword(_Schema = {struct, Def}, KeyWord) ->
    proplists:get_value(list_to_binary(KeyWord),Def).

keyword(_Schema = {struct, Def}, KeyWord, DefaultValue) ->
    proplists:get_value(list_to_binary(KeyWord),Def,DefaultValue).


patternProperties({struct,Schema}) ->

    case proplists:lookup(<<"patternProperties">>, Schema) of
        
       {_, {struct, Properties}} -> 
            Properties;
        
        none -> 
            undefined
    end.

additionalProperties({struct,Schema}) ->
    case proplists:lookup(<<"additionalProperties">>, Schema) of
        
       {_, Properties} -> 
            Properties;

        <<"false">> ->
            false;

        none -> 
            true
    end.

additionalItems({struct,Schema}) ->
    case proplists:lookup(<<"additionalItems">>, Schema) of

       {_,Properties} -> 
            Properties;

        <<"false">> ->
            false;

        none -> 
            true
    end.

links({struct,Schema}) ->
  proplists:get_value(<<"links">>,Schema).

propertyValue({struct,Schema},PropertyName) ->
  proplists:get_value(list_to_binary(PropertyName),Schema).
  
