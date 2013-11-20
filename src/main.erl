-module(main).

-compile(export_all).

%% Run it using, for example,
%% erl -pa ebin -noshell -run test write_instance_of test/in/string.jsch 

write_instance_of(File) ->
    {ok, Schema} = jsonschema:read_schema(File),
    io:format("RUNNING 1 INSTANCE...~n"),

    try jsongen:json(Schema) of
    
        JsonGenerator ->
            JsonInstance = eqc_gen:pick(JsonGenerator),
            JsonString = json:encode(JsonInstance),
            io:format("~n~n~s~n", [JsonString])

    catch
        Throw ->
            io:format("~n****EXCEPTION. REASON: ~p~n",[Throw]);
            %{throw,caught};
        error:Error ->
            io:format("~n****ERROR. REASON: ~p~n",[Error]);
            %{error,Error};
        exit:Exit ->
            io:format("~n*****EXIT. REASON: ~p~n",[Exit])
            %{exit, Exit}

    after
        halt()
    end.



gen_instance(Generator,N) when N > 0 ->
    JsonInstance = eqc_gen:pick(Generator),
    JsonString = json:encode(JsonInstance),
    io:format("~n~n~s~n", [JsonString]),
    gen_instance(Generator,N-1);

gen_instance(_,0) ->
	halt().

write_X_instances_of([File,N])  ->
    {ok, Schema} = jsonschema:read_schema(File),
    JsonGenerator = jsongen:json(Schema),
    io:format("RUNNING ~B INSTANCES...~n",[list_to_integer(N)]),
    gen_instance(JsonGenerator,list_to_integer(N)).
