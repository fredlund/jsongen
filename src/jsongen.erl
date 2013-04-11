-module(jsongen).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

% Use example
write_instance(File) ->
    {ok, S} = jsonschema:read_file(File),
    JsonGenerator = jsongen:schema(S),
    JsonInstance = eqc_gen:pick(JsonGenerator),
    JsonString = mochijson2:encode(JsonInstance),
    io:format("~s~n",[JsonString]).

schema(Schema) ->
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
            string()
    end.

array(Schema) ->
    eqc_gen:list(schema(Schema)).

template(_Template) ->
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
    % eqc_gen:list(eqc_gen:char()).
    name().

propname() ->
    name().

name() ->
    eqc_gen:non_empty(eqc_gen:list(eqc_gen:choose($a,$z))).
