-module(jsg_utils).

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_schema(URL) ->
  io:format("looking up ~s~n",[URL]),
  ensure_open(ets_schemas),
  case ets:lookup(ets_schemas,URL) of
    [{_,Schema}] ->
      {ok,Schema};
    _ ->
      no
  end.

store_schema(URL,Schema) ->
  io:format("storing ~s~n",[URL]),
  ensure_open(ets_schemas),
  true = ets:insert(ets_schemas,{URL,Schema}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ensure_open(EtsName) ->
  case ets:info(EtsName) of
    undefined ->
      open_ets_table(EtsName);
    _ ->
      ok
  end.

open_ets_table(EtsName) ->
  open_ets_table(EtsName,[named_table,public]).

open_ets_table(EtsName,Options) ->
  spawn(fun () ->
	    case ets:info(EtsName) of
	      undefined ->
		ets:new(EtsName,Options),
		wait_forever();
	      _ ->
		ets:delete_all_objects(EtsName)
	    end
	end),
  wait_until_stable(EtsName).

wait_until_stable(Name) ->
  case ets:info(Name) of
    L when is_list(L) ->
      ok;
    _ ->
      wait_until_stable(Name)
  end.

wait_forever() ->
  receive 
    terminate ->
      ok;
    _ -> wait_forever()
  end.

