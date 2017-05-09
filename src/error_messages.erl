-module(error_messages).

-compile(export_all).

wrong_body_message(JSONText, RawSchema) ->
  io:format
    ("~n***************************************************~n" ++
       "ERROR [postcondition error] [WRONG BODY]~n " ++
       "the JSON value~n~s~n"++
       "did not validate against the schema~n~s~n"++
       "***************************************************~n",
     [jsg_json:pretty_json(JSONText),
      jsg_json:pretty_json(mochijson2:encode(RawSchema))]).

wrong_body_message(Args, Body, RealTargetSchema, Reason) ->
  io:format
    ("~n***************************************************~n" ++
       "ERROR [postcondition error] [WRONG BODY]~n " ++
       "for http call~n~s~n"++
       "the JSON value~n~s~n"++
       "did not validate against the schema~n~s~n"++
       "due to error~n~p~n"++
       "***************************************************~n",
     [js_links_machine:format_http_call(Args),
      Body,
      mochijson2:encode(RealTargetSchema),
      Reason]).

wrong_status_code(Args, Result, SchemaStatusCode) ->
  io:format
    ("~n***************************************************~n" ++
       "ERROR [postcondition error] [WRONG HTTP STATUS CODE]~n"++
       "for http call~n~s~n"++
       "the HTTP response code was: ~p~n"++
       "but expected: ~p~n" ++
       "***************************************************~n",
     [js_links_machine:format_http_call(Args),
      js_links_machine:http_result_code(Result),
      SchemaStatusCode]).

wrong_http_call(Args, Result) ->
  io:format
    ("~n***************************************************~n" ++
       "ERROR [postcondition error] [WRONG HTTP CALL]~n"++
       "for http call~n~s~n"++
       "responded with error: ~p~n" ++
       "***************************************************~n",
     [js_links_machine:format_http_call(Args),
      js_links_machine:http_error(Result)]).

erlang_exception(Args, Error) ->
  io:format
    ("~n***************************************************~n" ++
       "ERROR [exception]~n"++
       "when calling generating/issuing the http call ~n~s~n" ++
       "Erlang raised the exception~n~p~n" ++
       "***************************************************~n",
     [js_links_machine:format_http_call(Args),Error]).

wrong_body_message(StatusCode, Header, JSONText) ->
  io:format
    ("~n***************************************************~n" ++
       "ERROR [postcondition error] [WRONG HTTP]~n" ++
       "http status header: ~p~n"++
       "expected: ~p~n"++
       "for the JSON value:~n~s~n"++
       "***************************************************~n",
     [StatusCode, Header, jsg_json:pretty_json(JSONText)]).

unknown_status(Args, StatusCode) ->
  io:format
    ("~n***************************************************~n" ++
       "WARNING [WRONG HTTP]~n" ++
       "unexpected http header: ~p~n" ++
       "for http call~n~s~n" ++
       "***************************************************~n",
     [StatusCode, js_links_machine:format_http_call(Args)]).
