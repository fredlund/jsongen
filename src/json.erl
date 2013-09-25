-module(json).

-compile(export_all).

-define(JSONLIB, mochijson2).

-type json_term() :: mochijson2:json_term().

-export_type([json_term/0]).

decode(JsonString) ->
     ?JSONLIB:decode(JsonString).

encode(JsonErlang) ->
     ?JSONLIB:encode(JsonErlang).
