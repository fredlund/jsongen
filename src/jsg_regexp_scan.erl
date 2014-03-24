-module(jsg_regexp_scan).
-export([scan/1]).

scan([]) ->
  [{'$end',1}];
scan([First|Rest]) ->
  case has_continuation(First) of
    true when Rest=/=[] ->
      case is_continuation(First,hd(Rest)) of
	true -> [{list_to_atom([First,hd(Rest)]),1}|scan(tl(Rest))];
	false -> [{list_to_atom([First]),1}|scan(Rest)]
      end;
    true ->
      [{list_to_atom([First]),1}|scan(Rest)];
    false ->
      case is_special(First) of
	true ->
	  [{list_to_atom([First]),1}|scan(Rest)];
	false when First>=$0, First=<$9 ->
	  [{digit,1,First-$0}|scan(Rest)];
	false ->
	  [{basic_char,1,First}|scan(Rest)]
      end
  end.

has_continuation(Char) ->
  lists:member(Char,[$+,$*,$?,$\\]).

is_special(Char) ->
  lists:member(Char,[$+,$*,$?,$(,$),${,$},$[,$],$-,$\\,$:,$,,$^,$$,$.,$|]).

is_continuation(Char,Cont) ->
  lists:member
    ({Char,Cont},
     [{$+,$?},{$*,$?},{$?,$?},{$\\,$x}]).


  
  
      
