-module(uri_template).

-compile(export_all).

-export([sub/2]).

-import(lists, [foldl/3, reverse/1, reverse/2]).

-define(is_empty_list(Value), is_tuple(Value) andalso element(2, Value) =:= {list, []}).


sub(Vars, Template) ->
  sub(parse(Template), encode(Vars), []).

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
      {var, list_to_atom(String), []};
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

expand({var, Var, Default}, Values) ->
  case proplists:lookup(Var, Values) of
    none -> Default;
    {Var, Value} -> Value
  end;
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
