-module(jsongen_validator).
-compile(export_all).

start_validator() ->
  ok.

validate(Schema,Body) ->
  JSON = mochijson2:decode(Body),
  jsg_json_validate:validate(JSON,Schema).

  
    
