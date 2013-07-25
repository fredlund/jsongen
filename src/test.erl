-module(test).

-compile(export_all).

%% Run it using, for example,
%% erl -pa ebin -noshell -run test write_instance_of test/in/combined.jsch 

write_instance_of(File) ->
    {ok, Schema} = jsonschema:read_file(File),
    JsonGenerator = jsongen:json(Schema),
    JsonInstance = eqc_gen:pick(JsonGenerator),
    JsonString = json:encode(JsonInstance),
    io:format("~s~n", [JsonString]).
    %halt().

gen_list(L,_,0) ->
    L;

gen_list(L, JsonGenerator, N) when N > 0 ->
    L2 = lists:append(L,[json:encode(eqc_gen:pick(JsonGenerator))]),
    gen_list(L2,JsonGenerator,N-1).
		  

%instances_loop(_, 0) ->
 %   io:format("Done~n"),
  %  halt();

instances_10_times(File) ->
    {ok, Schema} = jsonschema:read_file(File),
    JsonGenerator = jsongen:json(Schema),
    io:format("RUNNING 10 INSTANCES...~n"),
    L = gen_list([json:encode(eqc_gen:pick(JsonGenerator))],JsonGenerator,9),
    [io:format("~s~n", [X]) || X <- L],
    io:format("Done~n"),
    halt().

