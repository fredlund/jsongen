-module(jsongen_validator).
-export([ start_validator/0
	, validate/2
	]).

start_validator() ->
  ok.

validate(Schema,Body) ->
  JSON = mochijson2:decode(Body),
  jsg_json_validate:validate(JSON,Schema).

  
    
