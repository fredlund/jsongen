-module(json).

-compile(export_all).

-define(JSONLIB, mochijson2).

decode(JsonString) ->
     ?JSONLIB:decode(JsonString).

encode(JsonErlang) ->
     ?JSONLIB:encode(JsonErlang).
