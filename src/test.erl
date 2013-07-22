-module(test).

-compile(export_all).

%% Run it using, for example,
%% erl -pa ebin -noshell -run test write_instance_of test/in/combined.jsch 

write_instance_of(File) ->
    {ok, Schema} = jsonschema:read_file(File),
    JsonGenerator = jsongen:json(Schema),
    JsonInstance = eqc_gen:pick(JsonGenerator),
    JsonString = json:encode(JsonInstance),
    io:format("~s~n", [JsonString]),
    halt().

