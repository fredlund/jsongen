%% @doc This module contains functions for transforming
%% JSON data from text to a mochijson2 Erlang term, and back.
%% @author Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund 
%% (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)
%% @copyright 2013 Ángel Herranz, Lars-Ake Fredlund, Sergio Gil

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

%% @doc Translates a JSON value (as text) into a mochijson2 Erlang term
%% representing the value.
-spec decode(json_text()) -> json_term().
decode(JsonString) ->
     ?JSONLIB:decode(JsonString).

%% @doc Translates a mochijson2 Erlang term representing a JSON value
%% into its textual representation.
-spec encode(json_term()) -> json_text().
encode(JsonErlang) ->
     ?JSONLIB:encode(JsonErlang).

%% @doc
%% Reads a JSON schema in textual format, converting it into
%% a mochijson2 Erlang term. 
%% The function argument can either 
%% be on the form "http:...", "file:..." or a filename.
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
