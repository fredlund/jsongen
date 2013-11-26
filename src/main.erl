-module(main).

-compile(export_all).

%% Run it using, for example, to generate 10 samples of string_simple.jsch:
%% erl -pa ebin -noshell -run main write_X_instances_of 10 tests/string_simple.jsch

write_instance_of(File) ->

    %% This is for testing. 'write_X_instances_of' should be used from now on
    try
        io:format("~n+OPENING FILE ~p~n~n",[File]),
        {ok, Schema} = jsonschema:read_schema(File),
        io:format("RUNNING 1 INSTANCE...~n"),
        JsonGenerator = jsongen:json(Schema),
        JsonInstance = eqc_gen:pick(JsonGenerator),
        JsonString = json:encode(JsonInstance),
        io:format("~n~n~s~n", [JsonString])

    of 
        _ -> 
            io:format("~n+JSON INSTANCE GENERATED SUCESFULLY~n~n"),
            halt()
    catch
        exception:Throw ->
            
            case Throw of
                regExp_with_length ->
                    io:format("~n****EXCEPTION: 'pattern' keyword and 'minLenght' or 'maxLength' can't be used together~n",[]);
                _ ->
                    io:format("Exception thrown: ~p~n",[Throw]),
                    io:format("~n****EXCEPTION. REASON: ~p~n",[Throw])
            end;
        error:Error ->
            case Error of
                {badmatch,{error,enoent}} ->
                    io:format("~n****ERROR. FILE ~p COULDN'T BE OPENED~n",[File]);
                _ ->
                    io:format("~n****ERROR. REASON: ~p~n",[Error])
            end;
        exit:Exit ->
            io:format("~n*****EXIT. REASON: ~p~n",[Exit])

    after
        halt()
    end.



gen_instance(Generator,N) when N > 0 ->
    JsonInstance = eqc_gen:pick(Generator),
    JsonString = json:encode(JsonInstance),
    io:format("~n~n~s~n", [JsonString]),
    gen_instance(Generator,N-1);

gen_instance(_,0) ->
	ok.

write_X_instances_of([N,File])  ->
    try
        io:format("~n+OPENING FILE ~p~n~n",[File]),
        {ok, Schema} = jsonschema:read_schema(File),
        JsonGenerator = jsongen:json(Schema),
        io:format("RUNNING ~B INSTANCES...~n",[list_to_integer(N)]),
        gen_instance(JsonGenerator,list_to_integer(N))
    of
        ok ->
            io:format("~n~n+PROCESS FINISHED SUCESFULLY~n~n",[]),
            halt()
    catch
        Throw ->     
            case Throw of
                regExp_with_length ->
                    io:format("~n****EXCEPTION: 'pattern' keyword and either 'minLenght' or 'maxLength' keywords can't be used together~n",[]);
                _ ->
                    io:format("Exception thrown: ~p~n",[Throw]),
                    io:format("~n****EXCEPTION. REASON: ~p~n",[Throw])
            end;
        exit:Exit ->
            io:format("~n*****EXIT. REASON: ~p~n",[Exit])     
    after
        halt()           
    end.
