-module(jsg_jsonref).

-compile(export_all).

-export_type([jsonpointer/0]).

-type jsonpointer() :: [string()].
-type url() :: string().
-type uri() :: string().

%% From http://json-schema.org/latest/json-schema-core.html
%% 
%%    JSON Schema uses JSON Reference as a mechanism for schema
%%    addressing. It extends this specification in two ways:
%% 
%%    - JSON Schema offers facilities to alter the base URI against
%%      which a reference must resolve by the means of the "id"
%%      keyword;
%% 
%%    - it defines a specific dereferencing mechanism extending JSON
%%      Reference to accept arbitrary fragment parts.
                             
%% JSON Reference
%% http://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03
%% 
%%    A JSON Reference is a JSON object, which contains a member named
%%    "$ref", which has a JSON string value.  Example:
%% 
%%    { "$ref": "http://example.com/example.json#/foo/bar" }
%% 
%%    If a JSON value does not have these characteristics, then it
%%    SHOULD NOT be interpreted as a JSON Reference.
%% [...]
%%    Resolution of a JSON Reference object SHOULD yield the referenced
%%    JSON value.  Implementations MAY choose to replace the reference with
%%    the referenced value.
%% 
%%    If the URI contained in the JSON Reference value is a relative URI,
%%    then the base URI resolution MUST be calculated according to
%%    [RFC3986], section 5.2.  Resolution is performed relative to the
%%    referring document.
%% 
%%    If a URI contains a fragment identifier, then the fragment should be
%%    resolved per the fragment resolution mechansim of the referrant
%%    document.  If the representation of the referrant document is JSON,
%%    then the fragment identifier SHOULD be interpreted as a
%%    [JSON-Pointer].
               
%% JavaScript Object Notation (JSON) Pointer
%% http://tools.ietf.org/html/rfc6901
%% 
%% For example, given the JSON document
%% 
%%    {
%%       "foo": ["bar", "baz"],
%%       "": 0,
%%       "a/b": 1,
%%       "c%d": 2,
%%       "e^f": 3,
%%       "g|h": 4,
%%       "i\\j": 5,
%%       "k\"l": 6,
%%       " ": 7,
%%       "m~n": 8
%%    }
%% 
%%    Then the following JSON strings evaluate to the accompanying values:
%% 
%%     ""         // the whole document
%%     "/foo"       ["bar", "baz"]
%%     "/foo/0"    "bar"
%%     "/"          0
%%     "/a~1b"      1
%%     "/c%d"       2
%%     "/e^f"       3
%%     "/g|h"       4
%%     "/i\\j"      5
%%     "/k\"l"      6
%%     "/ "         7
%%     "/m~0n"      8

%% Uniform Resource Identifier (URI): Generic Syntax
%% http://tools.ietf.org/html/rfc3986
%% 
%% The following are two example URIs and their component parts:
%% 
%%          foo://example.com:8042/over/there?name=ferret#nose
%%          \_/   \______________/\_________/ \_________/ \__/
%%           |           |            |            |        |
%%        scheme     authority       path        query   fragment
%%           |   _____________________|__
%%          / \ /                        \
%%          urn:example:animal:ferret:nose
%% 
%% [...]
%% 
%% 3.5. Fragment
%%    The fragment identifier component of a URI allows indirect
%%    identification of a secondary resource by reference to a primary
%%    resource and additional identifying information.  The identified
%%    secondary resource may be some portion or subset of the primary
%%    resource, some view on representations of the primary resource, or
%%    some other resource defined or described by those representations.  A
%%    fragment identifier component is indicated by the presence of a
%%    number sign ("#") character and terminated by the end of the URI.
%% 
%%       fragment    = *( pchar / "/" / "?" )
%% 
%%    The semantics of a fragment identifier are defined by the set of
%%    representations that might result from a retrieval action on the
%%    primary resource.  The fragment's format and resolution is therefore
%%    dependent on the media type [RFC2046] of a potentially retrieved

%% @doc Dereferences a JSON Reference (a JSON object, which contains a member named "$ref", which has a JSON string value.  Example:
%% 
%%    { "$ref": "http://example.com/example.json#/foo/bar" }

%% @doc Returns the json value indicated by the JsonRef using
%% RootJsonTerm as root document in case no URL part exists in the
%% URI.
-spec unref(JsonRef :: jsg_json:jsonterm(),
            RootJsonTerm :: jsg_json:jsonterm()) -> jsg_json:json_term().
unref({struct, JsonDict}, RootJsonTerm) ->
  URI_bin = proplists:get_value(<<"$ref">>,JsonDict),
  if is_binary(URI_bin) -> % Otherwise unref breaks
      URI = binary_to_list(URI_bin),
      {URL,Pointer} = url_pointer(URI),
      %%io:format("Split ~p into ~p, ~p~n",[URI,URL,Pointer]),
      case URL of
        undefined -> JsonTerm = RootJsonTerm;
        _ -> {ok, JsonTerm} = jsg_json:decode_url(URL)
      end,
      % io:format("Dereferencing ~p on ~s~n",[Pointer,jsg_json:encode(JsonTerm)]),
      deref(Pointer,JsonTerm)
  end.

%% @doc Evaluates the json pointer Pointer in the json value JsonTerm.
-spec deref(Pointer::jsonpointer(),
            JsonTerm::jsg_json:json_term()) -> jsg_json:json_term().
deref([],JsonTerm) -> JsonTerm;
deref([Key|Pointer],JsonTerm) when is_list(JsonTerm) -> %% Array
  case list_is_integer(Key) of
    true ->
      Index = list_to_integer(Key),
      deref(Pointer,lists:nth(Index + 1,JsonTerm))
  end;
deref([Key|Pointer],{struct, JSON_dict}) -> %% Object
  Key_decoded = decode_escaped(Key),
  Key_bin = list_to_binary(Key_decoded),
  JsonTerm = proplists:get_value(Key_bin,JSON_dict),
  deref(Pointer,JsonTerm).


%% CBE
%% @doc Evaluates the json pointer Pointer in the json value JsonTerm and 
%% substitutes the json value by a new value

-spec subst(Pointer::jsonpointer(),
            JsonTerm::jsg_jsg_json:json_term(),
            NewValue::jsg_jsg_json:json_term()) -> jsg_jsg_json:json_term().
subst([],_JsonTerm,NewValue) -> NewValue;
subst([Key|Pointer],JsonTerm,NewValue) when is_list(JsonTerm) -> %% Array
  case list_is_integer(Key) of
    true ->
      Index = list_to_integer(Key),
      substl(Index,Pointer,JsonTerm,NewValue)
  end;
subst([Key|Pointer],{struct, JSON_dict},NewValue) -> %% Object
  Key_bin = list_to_binary(Key),
  JsonTerm = proplists:get_value(Key_bin,JSON_dict),
  JSON_dict2 = proplists:delete(Key_bin,JSON_dict),
  {struct,[{Key_bin,subst(Pointer,JsonTerm,NewValue)}|JSON_dict2]}.

substl(1,Pointer,[H|T],NewValue) -> 
  [subst(Pointer,H,NewValue)|T];
substl(Pos,Pointer,[H|T],NewValue) -> 
  [H|substl(Pos-1,Pointer,T,NewValue)].


%% CBE

%% @doc Decodes any escaped character.
-spec decode_escaped(string()) -> string().
decode_escaped([]) -> [];
decode_escaped([$~,$1|S]) -> [$/|decode_escaped(S)];
decode_escaped([$~,$0|S]) -> [$~|decode_escaped(S)];
decode_escaped([C|S]) when C =/= $~ -> [C|decode_escaped(S)].

%% @doc Decides if a list represent an integer.
-spec list_is_integer(string()) -> boolean().
list_is_integer(S) ->
    lists:all(fun(C) -> (C >= $0) and (C =< $9) end, S).

%% @doc Splits a URI in a URL + a fragment.
-spec url_pointer(uri()) -> {undefined | url(),
                             undefined | jsonpointer()}.
url_pointer("") ->
  {undefined, undefined};
url_pointer([$#|Fragment]) ->
  {undefined, string:tokens(Fragment,"/")};
url_pointer(URI) ->
  [URL|Fragments] = string:tokens(URI,"#"),
  Pointer = case Fragments of
              [] -> [];
              [Fragment] -> string:tokens(Fragment,"/")
            end,
  {URL,Pointer}.



%% @doc Generates all valid pairs {P,V} where P is a JSON Pointer and
%% V is its derreferenced value in a given JSON value.
%% angel@angel-laptop:~/projects/sci/prowess/code/jsongen$ erl -pa ebin
%% Eshell V5.10.3  (abort with ^G)
%% 1>  {ok,JT} = jsg_jsg_json:decode_url("test/in/users.json").
%% {ok,{struct,[{<<"users">>,
%%               [{struct,[{<<"id">>,1},
%%                         {<<"username">>,<<"davidwalsh">>},
%%                         {<<"numPosts">>,404},
%%                         {<<"realName">>,<<"David Walsh">>}]},
%%                {struct,[{<<"id">>,2},
%%                         {<<"username">>,<<"russianprince">>},
%%                         {<<"numPosts">>,12},
%%                         {<<"realName">>,<<"Andrei Arshavin">>}]}]}]}}
%% 2>  jsonref:gen(JT).
%% [{[],
%%   {struct,[{<<"users">>,
%%             [{struct,[{<<"id">>,1},
%%                       {<<"username">>,<<"davidwalsh">>},
%%                       {<<"numPosts">>,404},
%%                       {<<"realName">>,<<"David Walsh">>}]},
%%              {struct,[{<<"id">>,2},
%%                       {<<"username">>,<<"russianprince">>},
%%                       {<<"numPosts">>,12},
%%                       {<<"realName">>,<<"Andrei Arshavin">>}]}]}]}},
%%  {[<<"users">>],
%%   [{struct,[{<<"id">>,1},
%%             {<<"username">>,<<"davidwalsh">>},
%%             {<<"numPosts">>,404},
%%             {<<"realName">>,<<"David Walsh">>}]},
%%    {struct,[{<<"id">>,2},
%%             {<<"username">>,<<"russianprince">>},
%%             {<<"numPosts">>,12},
%%             {<<"realName">>,<<"Andrei Arshavin">>}]}]},
%%  {[<<"users">>,"0"],
%%   {struct,[{<<"id">>,1},
%%            {<<"username">>,<<"davidwalsh">>},
%%            {<<"numPosts">>,404},
%%            {<<"realName">>,<<"David Walsh">>}]}},
%%  {[<<"users">>,"0",<<"id">>],1},
%%  {[<<"users">>,"0",<<"username">>],<<"davidwalsh">>},
%%  {[<<"users">>,"0",<<"numPosts">>],404},
%%  {[<<"users">>,"0",<<"realName">>],<<"David Walsh">>},
%%  {[<<"users">>,"1"],
%%   {struct,[{<<"id">>,2},
%%            {<<"username">>,<<"russianprince">>},
%%            {<<"numPosts">>,12},
%%            {<<"realName">>,<<"Andrei Arshavin">>}]}},
%%  {[<<"users">>,"1",<<"id">>],2},
%%  {[<<"users">>,"1",<<"username">>],<<"russianprince">>},
%%  {[<<"users">>,"1",<<"numPosts">>],12},
%%  {[<<"users">>,"1",<<"realName">>],<<"Andrei Arshavin">>}]
%% 3>  [ {P,V} || {P,V} <- jsonref:gen(JT), V == <<"davidwalsh">> ].
%% [{[<<"users">>,"0",<<"username">>],<<"davidwalsh">>}]
-spec gen(jsg_jsg_json:json_term()) -> [{jsonpointer(),jsg_jsg_json:json_term()}].
gen(JsonTerm) when is_list(JsonTerm) -> % Array
  Indices = lists:map(fun erlang:integer_to_list/1,
                      lists:seq(0,length(JsonTerm)-1)),
  PVss = lists:map(fun gen/1,JsonTerm),
  % PVss = [[{P11,V11},...],
  %         [{P21,V21},...]...]
  [ {[], JsonTerm}
    | lists:concat(
        lists:zipwith(
          fun (I,PVs) -> [{[I|P],V} || {P,V} <- PVs] end,
          Indices,
          PVss)) ];
gen(JsonTerm = {struct, JsonDict}) -> % Object
  [ {[], JsonTerm}
    | lists:concat(
        lists:map(
          fun ({Key,Value}) -> [ {[Key|P],V} || {P,V} <- gen(Value) ] end,
          JsonDict)) ];
gen(JsonTerm) ->
  [ {[], JsonTerm} ].
                                    
