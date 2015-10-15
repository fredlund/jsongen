%%%-------------------------------------------------------------------
%% @doc java_erlang public API
%% @end
%%%-------------------------------------------------------------------

-module(jsongen_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    case code:priv_dir(jsongen) of
	{error,_} ->
	    ok;
	Name when is_list(Name) ->
	    JSDir = filename:dirname(Name),
	    LibDir = filename:dirname(JSDir),
	    code:add_patha(LibDir++"/java_erlang/ebin"),
	    code:add_patha(LibDir++"/jesse/ebin"),
	    code:add_patha(LibDir++"/mochijson2/ebin"),
	    code:add_patha(JSDir++"/priv/java_erlang/ebin"),
	    code:add_patha(JSDir++"/priv/jesse/ebin"),
	    code:add_patha(JSDir++"/priv/mochijson2/ebin"),
	    ok = application:ensure_started(java_erlang),
	    ok = application:ensure_started(jesse),
	    ok = application:ensure_started(mochijson2)
    end,
    jsongen_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
