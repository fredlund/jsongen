-module(jsg_json).

-export([decode/1, decode_url/1,
         encode/1]).

-define(JSONLIB, mochijson2).

-type json_object() :: {struct, json_dict()}.

-type json_dict() :: [{binary() | atom(), json_term()}].

-type json_array() :: [json_term()].

-type json_term() ::
        null % null
      | true % true
      | false % false
      | binary() | atom() % strings
      | number() % integer or float
      | json_object() % object
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

-spec decode_url(string()) -> {ok, json_term()} | {error, any()}.
decode_url(URL = [$h,$t,$t,$p,$:|_]) ->
  Result = httpc:request(URL),
  case Result of
    {ok, {{_Version, 200, _ReasonPhrase},
          _Headers,
          JsonString}} ->
      JsonTerm = ?MODULE:decode(JsonString),
      {ok, JsonTerm};
    _ ->
      Result
  end;
decode_url([$f,$i,$l,$e,$:|Filename]) ->
  Result = file:read_file(Filename),
  case Result of
    {ok, JsonString} ->
      JsonTerm = ?MODULE:decode(JsonString),
      {ok, JsonTerm};
    _ ->
      Result
  end;
decode_url(URL) ->
  decode_url([$f,$i,$l,$e,$:|URL]).
