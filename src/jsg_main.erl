-module(jsg_main).

-compile(export_all).

%% Run it using, for example,
%% erl -pa ebin -noshell -run main write_X_instances_of 10 tests/string_simple.jsch

write_instance_of(File) ->
write_X_instances_of([1,File]).

gen_instance(Generator,N,Count) when N > 0 ->
    JsonInstance = eqc_gen:pick(Generator),
    JsonString = json:encode(JsonInstance),
    io:format("~n+INSTANCE ~B GENERATED:~n~s~n", [Count,JsonString]),
    gen_instance(Generator,N-1,Count + 1);

gen_instance(_,0,_) ->
	ok.

write_X_instances_of([N,File])  ->
    try
        io:format("~n+OPENING FILE ~p~n~n",[File]),
        {ok, Schema} = jsonschema:read_schema(File),
        JsonGenerator = jsongen:json(Schema),
        io:format("GENERATING ~B INSTANCES...~n",[list_to_integer(N)]),
        gen_instance(JsonGenerator,list_to_integer(N),1)
    of
        ok ->
            io:format("~n~n+PROCESS FINISHED SUCESFULLY~n~n",[]),
            halt()
    catch
        Throw ->     
            case Throw of
                regExp_with_length ->
                    io:format("~n****EXCEPTION: 'pattern' keyword and either 'minLenght'",[]),
                    io:format(" or 'maxLength' keywords can't be used together~n~n",[]);
                _ ->
                    io:format("Exception thrown: ~p~n",[Throw]),
                    io:format("~n****EXCEPTION. REASON: ~p~n",[Throw])
            end;
        error:Error ->
            case Error of
                {badmatch,{error,enoent}} ->
                    io:format("~n****ERROR. FILE ~p COULDN'T BE OPENED~n",[File]);
                _ ->
                    io:format("~n****ERROR. REASON: ~p ~n",[Error])
            end;
        exit:Exit ->
            io:format("~n*****EXIT. REASON: ~p ~n",[Exit])     
    after
        io:format("~n~n+PROCESS ABORTED~n~n",[]),
        halt()           
    end.
