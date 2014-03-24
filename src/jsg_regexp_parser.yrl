Rootsymbol pattern.
Endsymbol '$end'.
Nonterminals pattern assertion atom quantifier quantifier_prefix escape alternative characterClass characterClassSpecs characterClassSpec number character term characters hexnumber digit_or_char. 
Terminals '(' ')' '{' '}' '[' ']' '-' '\\' ':' ',' '^' '$' '*' '+' '?' '+?' '*?' '??' '.' '|' basic_char digit '\\x'.

pattern -> pattern '|' pattern : merge_ors('$1','$3').
pattern -> alternative : '$1'.

alternative -> term : '$1'. 
alternative -> term alternative : {concat,'$1','$2'}.

term -> assertion : '$1'.
term -> atom : '$1'.
term -> atom quantifier : {quantify,'$1','$2'}.

assertion -> '^' : head.
assertion -> '$' : tail.

atom -> character : '$1'.
atom -> '.' : dot.
atom -> escape : '$1'.
atom -> characterClass : {'characterClass','$1'}.
atom -> '(' pattern ')' : '$2'.

quantifier -> quantifier_prefix : '$1'.

quantifier_prefix -> '*' : star.
quantifier_prefix -> '+' : plus.
quantifier_prefix -> '?' : query.
quantifier_prefix -> '{' number '}' : {number,'$2'}.
quantifier_prefix -> '{' number ',' '}' : {number_comma,'$2'}.
quantifier_prefix -> '{' number ',' number '}' : {number_number,'$2','$4'}.

characterClass -> '[' characterClassSpecs ']' : '$2'.

characterClassSpecs -> characterClassSpec : ['$1'].
characterClassSpecs -> characterClassSpec characterClassSpecs : ['$1'|'$2'].

characterClassSpec -> character '-' character : {'range',pos,'$1','$3'}.
characterClassSpec -> '^' character '-' character : {'range',neg,'$2','$4'}.
characterClassSpec -> character characters : {'range',pos,['$1'|'$2']}.
characterClassSpec -> '^' character characters : {'range',neg,['$2'|'$3']}.

characters -> '$empty' : [].
characters -> character characters : ['$1'|'$2'].

character -> basic_char : {symbol,element(3,'$1')}.
character -> digit : {symbol,element(3,'$1')+$0}.
		   
escape -> '\\x' hexnumber : {symbol,'$2'}.
escape -> '\\' character : '$2'.
escape -> '\\' '.' : {symbol,$.}.

number -> digit : element(3,'$1').
number -> number digit : '$1'*10+element(3,'$2').
		
hexnumber -> digit_or_char digit_or_char : from_hex('$1','$2').

digit_or_char -> basic_char : element(3,'$1').
digit_or_char -> digit : element(3,'$1').

Erlang code.

merge_ors({'or',L1},{'or',L2}) -> {'or',L1++L2};
merge_ors({'or',L1},R2) -> {'or',[R2|L1]};
merge_ors(R1,{'or',L2}) -> {'or',[R1|L2]};
merge_ors(R1,R2) -> {'or',[R1,R2]}.

from_hex(D1,D2) ->
  from_hex(D1)*16+from_hex(D2).

from_hex(Ch) ->
  if
    Ch>=0, Ch=<9 -> Ch;
    Ch>=$A, Ch=<$E -> Ch-$A+10;
    Ch>=$a, Ch=<$e -> Ch-$a+10;
    true ->
      io:format("Strange hex character ~c~n",[Ch]),
      throw(bad)
  end.


