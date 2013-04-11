-module(test).

-compile(export_all).

%% Run it using, for example,
%% erl -pa ebin -noshell -run test gen test/in/combined.jsch 

gen([Filename]) ->
    {ok,Schema} = jsonschema:read_file(Filename),
    eqc_gen:sample(jsongen:schema(Schema)),
    halt().


