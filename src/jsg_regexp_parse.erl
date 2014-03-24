-module(regexp_parse).
-export([string/1]).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-define(DEBUGVAL(),true).
-else.
-define(LOG(X,Y), ok).
-define(DEBUGVAL(),false).
-endif.

string(S) ->
  Tokens = regexp_scan:scan(S),
  ?LOG("tokens are~n~p~n",[Tokens]),
  case regexp_parser:parse(Tokens) of
    {ok,Result} ->
      Canonical = canonical(Result),
      ?LOG("Canonical result:~n~p~n",[Canonical]),
      Canonical;
    {error,_} ->
      io:format("*** parse error at expression ~p~n",[S]),
      io:format("tokens are~n~p~n",[Tokens]),
      throw(parse)
  end.

canonical(Expr) ->
  ?LOG("before canonical:~n~p~n",[Expr]),
  tail_anchor(head_anchor(Expr)).

head_anchor({'or',L}) ->
  {'or',lists:map(fun head_anchor/1,L)};
head_anchor({'concat',head,R}) ->
  R;
head_anchor({'concat',R1,R2}) ->
  {'concat',head_anchor(R1),R2};
head_anchor(Other) ->
  hd_anchor(Other).

tail_anchor({'or',L}) ->
  {'or',lists:map(fun tail_anchor/1,L)};
tail_anchor({'concat',R,tail}) ->
  R;
tail_anchor({'concat',R1,R2}) ->
  {'concat',R1,tail_anchor(R2)};
tail_anchor(Other) ->
  tl_anchor(Other).

hd_anchor(R) ->
  {'concat',{'quantify',{'characterClass',[{range,pos,{symbol,0},{symbol,127}}]},star},R}.
tl_anchor(R) ->
  {'concat',R,{'quantify',{'characterClass',[{range,pos,{symbol,0},{symbol,127}}]},star}}.












  
  
  
