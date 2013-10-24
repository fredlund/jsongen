-module(json).

-compile(export_all).

-define(JSONLIB, mochijson2).

-type json_array() :: [json_term()].

-type json_dict() :: [{binary() | atom(), json_term()}].

-type json_term() ::
        null % null
      | true % true
      | false % false
      | binary() | atom() % strings
      | number() % integer or float
      | {struct, json_dict()} % objects
      | json_array() % array
        .

-type json_text() :: iolist().

-export_type([json_term/0,json_text/0]).

-spec decode(json_text()) -> json_term().
decode(JsonString) ->
     ?JSONLIB:decode(JsonString).

-spec encode(json_term()) -> json_text().
encode(JsonErlang) ->
     ?JSONLIB:encode(JsonErlang).
