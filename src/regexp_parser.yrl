Rootsymbol pattern.
Endsymbol '$end'.
Nonterminals pattern assertion atom quantifier quantifier_prefix escape alternative characterClass characterClassSpecs characterClassSpec number character term characters. 
Terminals '(' ')' '{' '}' '[' ']' '-' '\\' ':' ',' '^' '$' '*' '+' '?' '+?' '*?' '??' '.' '|' basic_char digit .

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
atom -> '\\' escape : '$2'.
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
		   
escape -> number : {symbol,'$1'}.

number -> digit : element(3,'$1').
number -> number digit : '$1'*10+element(3,'$2').
			 

Erlang code.

merge_ors({'or',L1},{'or',L2}) -> {'or',L1++L2};
merge_ors({'or',L1},R2) -> {'or',[R2|L1]};
merge_ors(R1,{'or',L2}) -> {'or',[R1|L2]};
merge_ors(R1,R2) -> {'or',[R1,R2]}.

			       

