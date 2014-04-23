-module(test).

-compile(export_all).

-record(state,{static_links,links,private_state=void}).

initial_state() -> void.

next_state(State,Result,Call,Super) ->
  case Call of
    {_, _, follow_link, [{link,LD}], _} ->
      case proplists:get_value(title,LD) of
	"reset" -> State#state{links=sets:new()};
	_ -> Super(State,Result,Call)
      end
  end.

