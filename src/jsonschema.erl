-module(jsonschema).

-compile(export_all).

-define(JSONLIB, mochijson2).

read_schema(Url) ->    
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, JsonString}} =
        httpc:request(Url),
    {ok, ?JSONLIB:decode(JsonString)}.

read_file(Filename) ->
    Result = file:read_file(Filename),
    case Result of
        {ok, JsonString} ->
            Schema = ?JSONLIB:decode(JsonString),
            {ok, Schema};
        _ ->
            Result
    end.

type(_Schema={struct, Def}) ->
    {_,Type} = proplists:lookup(<<"type">>,Def),
    Type.

set_type(_Schema={struct, Def},Type) ->
    DefNoType = proplists:delete(<<"type">>,Def),
    {struct, [{<<"type">>,Type} | DefNoType]}.

items(_Schema={struct, Def}) ->
    {_, Items} = proplists:lookup(<<"items">>,Def),
    case Items of
        {struct, _} -> {itemSchema, Items};
        _ -> {itemsTemplate, Items}
    end.

properties(_Schema = {struct, Def}) ->
    {_,{struct, Properties}} = proplists:lookup(<<"properties">>,Def),
    Properties.

keyword(_Schema = {struct, Def}, KeyWord) ->
    proplists:get_value(list_to_binary(KeyWord),Def).

keyword(_Schema = {struct, Def}, KeyWord, DefaultValue) ->
    proplists:get_value(list_to_binary(KeyWord),Def,DefaultValue).

%% patternProperties(_Schema = {struct, Def}) ->
%%     {_,{struct, Properties}} = proplists:lookup(<<"patternProperties">>,Def),
%%     Properties.
