-module(gen_string_from_regexp).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

gen({'or',L}) ->
  eqc_gen:oneof(lists:map(fun gen/1, L));
gen({'concat',R1,R2}) ->
  ?LET
     ({X,Y},
      {gen(R1),gen(R2)},
      X++Y);
gen({'symbol',S}) ->
  [S];
gen({'characterClass',CSpec}) ->
  gen_characterClass_element(CSpec);
gen({quantify,R,N}) ->
  ?LET(FA,calculate_arity(N),
       gen({fixed,FA,R}));
gen({fixed,0,_R}) ->
  "";
gen({fixed,N,R}) ->
  gen({concat,R,{fixed,N-1,R}}).

gen_characterClass_element(CSpec) ->
  eqc_gen:oneof(lists:map(fun gen_spec_element/1, CSpec)).

gen_spec_element({'range',pos,{'symbol',S1},{'symbol',S2}}) ->
  [eqc_gen:choose(S1,S2)];
gen_spec_element({'range',neg,{'symbol',S1},{'symbol',S2}}) ->
  [eqc_gen:oneof
     ([eqc_gen:choose(0,S1-1),
       eqc_gen:choose(S2+1,127)])];
gen_spec_element({'range',pos,L}) ->
  [eqc_gen:oneof(lists:map(fun ({symbol,X}) -> X end,L))];
gen_spec_element({'range',neg,L}) ->
  NegList =
    sets:to_list
      (sets:subtract(sets:from_list(lists:seq(0,127)),
		     lists:map(fun ({symbol,X}) -> X end))),
  [eqc_gen:oneof(NegList)].

calculate_arity(star) ->
  nat();
calculate_arity(plus) ->
  ?LET(N,nat(),N+1);
calculate_arity('query') ->
  eqc_gen:choose([0,1]);
calculate_arity({number,N}) ->
  N;
calculate_arity({number_comma,N}) ->
  ?LET(M,nat(),N+M);
calculate_arity({number_number,N,M}) ->
  eqc_gen:choose(N,M).


  
  
