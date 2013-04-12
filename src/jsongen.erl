%% Copyright (c) 2013, Ángel Herranz, Lars-Ake Fredlund
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @doc This module implements translates an JSON Schema into
%% an Erlang QuickCheck generator.
%% @author Ángel Herranz (aherranz@fi.upm.es, Lars-Ake Fredlund (lfredlund@fi.upm.es)
%% @copyright 2013 Ángel Herranz, Lars-Ake Fredlund
%%

-module(jsongen).

-export([schema/1]).

-compile(export_all).

-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.



%% Mochijson2 should export a better type...
-opaque json_term() :: any().

% Use example: io:format("~s~n",[mochijson2:encode(eqc_gen:pick(jsongen:schema(S)))]).

-include_lib("eqc/include/eqc.hrl").

%% @doc
%% Translates a JSON schema into an Erlang QuickCheck generator.
-spec schema(json_term()) -> eqc_gen:gen(json_term()).


% Use example
write_instance_of(File) ->
    {ok, S} = jsonschema:read_file(File),
    JsonGenerator = jsongen:schema(S),
    JsonInstance = eqc_gen:pick(JsonGenerator),
    JsonString = mochijson2:encode(JsonInstance),
    io:format("~s~n", [JsonString]).

schema(Schema) ->
    ?LOG("schema(~p)~n",[Schema]),
    case jsonschema:type(Schema) of
        %% array
        %%     A JSON array. 
        <<"array">> ->
            case jsonschema:items(Schema) of
                {itemSchema, ItemSchema} ->
                    array(ItemSchema);
                {itemsTemplate, ItemsTemplate} ->
                    template(ItemsTemplate)
            end;
        %% boolean
        %%     A JSON boolean. 
        <<"boolean">> ->
            boolean();
        %% integer
        %%     A JSON number without a fraction or exponent part. 
        <<"integer">> ->
            integer();
        %% number
        %%     Any JSON number. Number includes integer.
        <<"number">> ->
            number();
        %% null
        %%     The JSON null value. 
        <<"null">> ->
            null();
        %% object
        %%     A JSON object.
        <<"object">> ->
            P = jsonschema:properties(Schema),
            % TODO: regular expressions for generating properties
            % _PP = jsonschema:patternProperties(Schema),
            {struct, lists:map (fun ({M,S}) ->
                                        {M,schema(S)}
                                end,
                                P)};
        %% string
        %%     A JSON string.
        <<"string">> ->
	    string();
        %% any
        %%     Any JSON data, including "null".
        <<"any">> ->
	    any();
        %% Union types
        %%     An array of two or more *simple type definitions*.
        Types when is_list(Types) ->
	    eqc_gen:oneof
              (lists:map
                 (fun (Type) ->
                          ConcreteSchema = jsonschema:set_type(Schema,Type),
                          schema(ConcreteSchema)
                  end,
                  Types))
    end.

array(Schema) ->
    eqc_gen:list(schema(Schema)).

template(_Template) ->
    %% TODO: generator for template
    null().

null() ->
    null.

integer() ->
    eqc_gen:int().

number() ->
    eqc_gen:oneof([eqc_gen:int(),eqc_gen:real()]).

boolean() ->
    eqc_gen:bool().

string() ->
    % TODO: generator of valid JSON strings
    % for the moment...
    ?LET(Name,name(),list_to_binary(Name)).

propname() ->
    name().

name() ->
    eqc_gen:non_empty(eqc_gen:list(eqc_gen:choose($a,$z))).

any() ->
    eqc_gen:oneof
      ([string(),number(),integer(),boolean(),object(),array(),null()]).

object() ->
    %% TODO: generator for object
    %% (could it be integrated in... schema/1?)
    null().

array() ->
    %% TODO: generator for array (no items)
    %% (it could be integrated in array/1)
    null().
