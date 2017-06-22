-module(jesse_validator).

-export([ start_validator/0
	, validate/2]).

start_validator() ->
  case code:which(jesse_schema_validator) of
    non_existing ->
      io:format
	("*** Error: the Jesse library is not accessible.~n"),
      throw(bad);
    _ ->
      ok
  end.

validate(Schema,Body) ->
  JSON = mochijson2:decode(Body),
  case jesse_schema_validator:validate(Schema,JSON,[]) of
    {ok,_} ->
      true;
    _ ->
      false
  end.

  
    
