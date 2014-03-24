-module(jsg_jsonschema).

-compile(export_all).

%% URL can be "http:...", "file:..." or directly a filename
read_schema(URL) ->
  jsg_json:decode_url(URL).

hasType(_Schema={struct,Def}) ->
  case proplists:lookup(<<"type">>,Def) of
      {_,_Type} ->
          true;
      none ->
          false
  end;
hasType(_Other) ->
  throw(bad).

hasEnum(_Schema={struct,Def}) ->
  case proplists:lookup(<<"enum">>,Def) of
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

       {_,{struct, Properties}} -> 
            Properties;

        {_,<<"false">>} ->
            false;

        none -> 
            true
    end.
