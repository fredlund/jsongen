-module(json_validate).

-export([validate/2]).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.

-spec validate(json:json_term(),json:json_term()) -> boolean() | maybe.

validate(Data,Schema) ->
  ?LOG("validate(~p,~n         ~p)~n",[Data,Schema]),
  case jsonschema:hasType(Schema) of
    true ->
      case jsonschema:type(Schema) of
	<<"integer">> ->
	  case is_integer(Data) of
	    false ->
	      false;
	    true ->
	      MaxScanned = jsonschema:keyword(Schema,"maximum"),
	      ExcMaxScanned = jsonschema:keyword(Schema,"exclusiveMaximum",false),
	      MinScanned = jsonschema:keyword(Schema,"minimum"),
	      ExcMinScanned = jsonschema:keyword(Schema,"exclusiveMinimum",false),
	      Multiple = jsonschema:keyword(Schema,"multipleOf",1),
	      if
		MaxScanned=/=undefined, Data>MaxScanned ->
		  false;
		MaxScanned=/=undefined, ExcMaxScanned==true, Data==MaxScanned ->
		  false;
		MinScanned=/=undefined, Data<MinScanned ->
		  false;
		MinScanned=/=undefined, ExcMinScanned==true, Data==MinScanned ->
		  false;
		Multiple=/=undefined, (Data rem Multiple)=/=0 ->
		  false;
		true ->
		  true
	      end
	  end;
	_ -> maybe
      end;
    false -> maybe
  end. 
