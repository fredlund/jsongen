-module(test).

-compile(export_all).

%% Run it using, for example,
%% erl -pa ebin -noshell -run test write_instance_of test/in/string.jsch 

write_instance_of(File) ->
    {ok, Schema} = jsonschema:read_file(File),
    JsonGenerator = jsongen:json(Schema),
    JsonInstance = eqc_gen:pick(JsonGenerator),
    JsonString = json:encode(JsonInstance),
    io:format("~s~n", [JsonString]),
    halt().


gen_instance(Schema,N) when N > 0 ->
	Generator = jsongen:json(Schema),
	JsonInstance = eqc_gen:pick(Generator),
	JsonString = json:encode(JsonInstance),
    io:format("~s~n", [JsonString]),
    gen_instance(Schema,N-1);

gen_instance(_,0) ->
	halt().


write_X_instances_of([File,N])  ->
    {ok, Schema} = jsonschema:read_file(File),
    io:format("RUNNING ~B INSTANCES...~n",[list_to_integer(N)]),
    gen_instance(Schema,list_to_integer(N)).


