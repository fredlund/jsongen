-module(uri_template).

%% -compile(export_all).

-export([ sub/2
	, encode/1
	, encode_var/1
	, parse/1
	, percent_encode/1
	, percent_encode/2
	, hexchr/1
	]).



-import(lists, [foldl/3, reverse/1, reverse/2]).

-define(is_empty_list(Value), is_tuple(Value) andalso element(2, Value) =:= {list, []}).


sub(Vars, Template) ->
%%  sub(parse(Template), encode(Vars), []).
  sub(parse(Template), Vars, []).

sub([], _Vars, URI) ->
  reverse(URI);
sub([Segment|Segments], Vars, URI) when is_integer(Segment) ->
  sub(Segments, Vars, [Segment|URI]);
sub([Segment|Segments], Vars, URI) ->
  sub(Segments, Vars, reverse(expand(Segment, Vars), URI)).

parse(Template) ->
  parse(Template, []).

parse([], Segments) ->
  lists:reverse(Segments);
parse([${|Etc], Segments) ->
  {String, Rem} = break($}, Etc),
  parse(Rem, [parse_expansion(String) | Segments]);
parse([C|Etc], Segments) ->
  parse(Etc, [C|Segments]).

parse_expansion(String) ->
  case string:tokens(String, "|") of
    [String] ->
      parse_var(String);
    [[$-|Op], Args, Vars] ->
      {list_to_atom(Op), Args, [parse_var(V) || V <- string:tokens(Vars, ",")]}
  end.

parse_var(String) ->
  case string:tokens(String, "=") of
    [String] ->
      {var, list_to_atom(String), void};
    [Var, Default] ->
      {var, list_to_atom(Var), Default}
  end.

break(Sep, List) ->
  case lists:splitwith(fun(C) -> C =/= Sep end, List) of
    {Taken, []} ->
      {Taken, []};
    {Taken, [Sep|Etc]} ->
      {Taken, Etc}
  end.

encode(Vars) ->
  [encode_var(V) || V <- Vars].

encode_var({Key, {list, List}}) ->
  {Key, {list, [percent_encode(Value) || Value <- List]}};
encode_var({Key, Value}) ->
  {Key, percent_encode(Value)}.

expand({var, Var, _Default}, Values) ->
 var_lookup(Var, Values);
expand({opt, Arg, Vars}, Values) ->
  expand_list(Vars, Values, fun(Value, Acc) -> expand_opt(Arg, Value, Acc) end);
expand({neg, Arg, Vars}, Values) ->
  expand_list(Vars, Values, fun(Value, Acc) -> expand_neg(Arg, Value, Acc) end);
% expand({prefix, _Arg, Vars}, _Values) when length(Vars) =/= 1 ->
% todo; % this is an error
expand({prefix, Arg, [{var, Var, []}]}, Values) ->
  case proplists:lookup(Var, Values) of
    none -> [];
    {Var, []} -> [];
    {Var, {list, List}} -> expand_prefix(Arg, List);
    {Var, Value} -> expand_prefix(Arg, [Value])
  end;
% expand({suffix, _Arg, Vars}, _Values) when length(Vars) =/= 1 ->
% todo; % this is an error
expand({suffix, Arg, [{var, Var, []}]}, Values) ->
  case proplists:lookup(Var, Values) of
    none -> [];
    {Var, []} -> [];
    {Var, {list, List}} -> expand_suffix(Arg, List);
    {Var, Value} -> expand_suffix(Arg, [Value])
  end;
expand({join, Arg, Vars}, Values) ->
  expand_list(Vars, Values, fun(Value, Acc) -> expand_join(Arg, Value, Acc) end);
% expand({list, _Arg, Vars}, _Values) when length(Vars) =/= 1 ->
% todo; % this is an error
expand({list, Arg, [{var, Var, []}]}, Values) ->
  case proplists:lookup(Var, Values) of
    none -> [];
    {Var, []} -> [];
    {Var, {list, List}} -> string:join(List, Arg)
  end.

expand_opt(_Arg, Value, Acc) when Value =:= none; ?is_empty_list(Value) ->
  Acc;
expand_opt(Arg, _Value, Acc) ->
  reverse(Arg, Acc).

expand_neg(Arg, Value, Acc) when Value =:= none; ?is_empty_list(Value) ->
  reverse(Arg, Acc);
expand_neg(_Arg, _Value, Acc) ->
  Acc.

expand_prefix(Arg, List) ->
  reverse(foldl(fun(Value, Acc) -> reverse(Value, reverse(Arg, Acc)) end, [], List)).

expand_suffix(Arg, List) ->
  reverse(foldl(fun(Value, Acc) -> reverse(Arg, reverse(Value, Acc)) end, [], List)).

expand_join(_Arg, none, Acc) ->
  Acc;
expand_join(Arg, {Var, Value}, Acc) ->
  reverse([$=|Value], reverse(atom_to_list(Var), case Acc of [] -> []; _ -> reverse(Arg, Acc) end)).

expand_list(Vars, Values, Expand) ->
  reverse(foldl(fun({var, Var, []}, Acc) -> Expand(proplists:lookup(Var, Values), Acc) end, [], Vars)).

-define(is_alphanum(C), C >= $A, C =< $Z; C >= $a, C =< $z; C >= $0, C =< $9).

percent_encode(Term) when is_integer(Term) ->
  integer_to_list(Term);
percent_encode(Term) when is_float(Term) ->
  float_to_list(Term);
percent_encode(Term) when is_atom(Term) ->
  percent_encode(atom_to_list(Term));
percent_encode(Term) when is_binary(Term) ->
  percent_encode(lists:reverse(binary_to_list(Term), []), []);
percent_encode(Term) when is_list(Term) ->
  percent_encode(lists:reverse(Term, []), []).

percent_encode([X | T], Acc) when ?is_alphanum(X); X =:= $-; X =:= $_; X =:= $.; X =:= $~ ->
  percent_encode(T, [X | Acc]);
percent_encode([X | T], Acc) ->
  NewAcc = [$%, hexchr(X bsr 4), hexchr(X band 16#0f) | Acc],
  percent_encode(T, NewAcc);
percent_encode([], Acc) ->
  Acc.

-compile({inline, [{hexchr, 1}]}).

hexchr(N) when N >= 10 ->
  N + $A - 10;
hexchr(N) when N < 10 ->
  N + $0.

parse_number(String) ->
  parse_number(String,[]).
parse_number([Ch|Rest],Acc) ->
  if
    Ch>=$0, Ch=<$9 ->
      parse_number(Rest,[Ch|Acc]);
    Acc==[] ->
      [Ch|Rest];
    true ->
      {lists:reverse(Acc),[Ch|Rest]}
  end;
parse_number([],Acc) when Acc==[] ->
  [];
parse_number([],Acc) ->
  {lists:reverse(Acc),[]}.

var_lookup(Name, _Context={Link,Object,Location}) ->
  NameList = atom_to_list(Name),
  case string:tokens(atom_to_list(Name), ".") of
    [Title,JPointer] ->
      %%io:format("an absolute link ~p:~p~n",[Title,JPointer]),
      History = jsg_links:link_history(Link),
      %%io:format("history=~n~p~n",[History]),
      {_,Num} = lists:keyfind(Title,1,History),
      {ok,HistoryObject} = jsg_store:get({object,Num}),
      PointerList = string:tokens(JPointer,"/"),
      try_deref(Link,PointerList,HistoryObject,Name);
    [_NameString] ->
      {ok,RealObject} = jsg_store:get({object,Object}),
      case parse_number(NameList) of
	{Number,Rest} -> 
	  PointerList = string:tokens(Rest,"/"),
	  try_deref_relative(Link,Number,PointerList,RealObject,Location,Name);
	[$*|Rest] ->
	  PointerList = string:tokens(Rest,"/"),
	  try_deref_relative(Link,any,PointerList,RealObject,Location,Name);
	Rest ->
	  PointerList = string:tokens(Rest,"/"),
	  try_deref_relative(Link,0,PointerList,RealObject,Location,Name)
      end
  end.
 
try_deref(Link,Pointer,Object,Name) ->
  case jsg_jsonref:deref(Pointer,Object) of
    false ->
      io:format
	("*** Error: for link~n~p~ncould not lookup ~p (~p) in ~p~n",
	 [Link,Name,Pointer,Object]),
      throw(bad);
    {ok,Result} ->
      binary_to_list(Result)
  end.

try_deref_relative(Link,LevelsUp,AbsolutePointer,Object,CurrentPointer,Name) ->
  case jsg_jsonref:deref_relative_pointer(LevelsUp,AbsolutePointer,Object,CurrentPointer) of
    false ->
      io:format
	("*** Error: for link~n~p~ncould not do relative lookup ~p (~p:~p)~nin ~p~n(at ~p)~n",
	 [Link,Name,LevelsUp,AbsolutePointer,Object,CurrentPointer]),
      throw(bad);
    {ok,Result} ->
      binary_to_list(Result)
  end.

     
  
  
