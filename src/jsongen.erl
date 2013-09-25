%% Copyright (c) 2013, Ángel Herranz, Lars-Ake Fredlund, Sergio Gil
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @doc This module translates a JSON Schema into
%% an Erlang QuickCheck generator.
%% @author Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)
%% @copyright 2013 Ángel Herranz, Lars-Ake Fredlund, Sergio Gil 
%%

-module(jsongen).

-export([json/1]).

-compile(export_all).

% -define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.

-define(MAX_INT_VALUE,100000).
-define(MAX_STR_LENGTH,1000).

-include_lib("eqc/include/eqc.hrl").

%% @doc
%% Translates a JSON schema into an Erlang QuickCheck generator.
-spec json(json:json_term()) -> eqc_gen:gen(json:json_term()).

json(Schema) ->
    ?LOG("json(~p)~n",[Schema]),
    case jsonschema:type(Schema) of
        %% array
        %%     A JSON array. 
        <<"array">> ->
	    MaxItems = jsonschema:keyword(Schema,"maxItems"),
	    MinItems = jsonschema:keyword(Schema,"minItems"),
	    _UniqueItems = 0,
            case jsonschema:items(Schema) of
                {itemSchema, ItemSchema} ->
                    array(ItemSchema);
                {itemsTemplate, ItemsTemplate} ->
                    template(ItemsTemplate)
            end;

        %% boolean
        %%     A JSON boolean. 
        <<"boolean">> ->
            boolean();




        %% integer
        %%     A JSON number without a fraction or exponent part. 
        <<"integer">> ->
			
	    MaxScanned = jsonschema:keyword(Schema,"maximum"),
	    ExcMaxScanned = jsonschema:keyword(Schema,"exclusiveMaximum",false),
	    MinScanned = jsonschema:keyword(Schema,"minimum"),
	    ExcMinScanned = jsonschema:keyword(Schema,"exclusiveMinimum",false),
	    Multiple = jsonschema:keyword(Schema,"multipleOf",1),
	    
	    % Setting up keywords

	    case MaxScanned of
		undefined -> Max = undefined;
		
		_ ->
		    case ExcMaxScanned of
			true ->
			    Max = MaxScanned -1;
			false ->
			    Max = MaxScanned
		    end
	    end,
	    


	    case MinScanned of
		undefined -> Min = undefined;
		
		_ ->
		    case ExcMinScanned of
			true ->
			    Min = MinScanned +1;
			false ->
			    Min = MinScanned
		    end
	    end,

	    Gen = case {Min, Max} of
		{undefined, undefined} ->
		    multiple_of(Multiple);
		    
		{Min, undefined} ->
		    multiple_of_min(Multiple, Min);
			
		{undefined, Max} ->
		    multiple_of_max(Multiple, Max);

		{Min, Max} ->
		    multiple_of_min_max(Multiple, Min,Max)
		  end,
            Gen;
 
			%Creating the generator

			%?SUCHTHAT(Int,randInt(Min,Max), 
					  %sMultiple(Int,MultipleOf));


        %% Number
        %%     Any JSON number. Number includes integer.
        <<"number">> ->
			MaxScanned = jsonschema:keyword(Schema,"maximum"),
			ExcMaxScanned = jsonschema:keyword(Schema,"exclusiveMaximum"),
			MinScanned = jsonschema:keyword(Schema,"minimum"),
			ExcMinScanned = jsonschema:keyword(Schema,"exlusiveMinimum"),
		   	_MultipleOf = jsonschema:keyword(Schema,"multipleOf",1),

			% Setting up keywords
			%case {MaxScanned,ExcMaxScanned} of
			case {MaxScanned} of	
				{undefined} ->
					Max = ?MAX_INT_VALUE;
				
				%% There should be an error case here. Json-schema doc says that if exclusiveMaximum is present, maximum MUST be present as well

				%{undefined, true} ->
				%	Max = ?MAX_INT_VALUE - 0.1;

			    %% Exclusive maximum for floats???
				%{MaxScanned, true} ->
					%Max = MaxScanned - 0.1;

				{MaxScanned} ->
					Max = MaxScanned
			
			end,
			

			case {MinScanned} of
				
				{undefined} ->
					Min = 0;
				

				%% There should be an error case here. Json-schema doc says that if exclusiveMinimum is present, minimum MUST be present as well

				%{undefined, _} ->
					%Min = 1;

		%		{MinScanned} ->
		%			Min = MinScanned + 0.1;

				{MinScanned} ->
					Min = MinScanned
			
			end,


	    case {ExcMinScanned, ExcMaxScanned} of

		{undefined, undefined} ->

		    ?SUCHTHAT(Float,randFlt(Min, Max),
			      (Float >= Min) and (Float =< Max)) ;

		{true, undefined} ->
		    ?SUCHTHAT(Float,randFlt(Min, Max),
			      (Float > Min) and (Float =< Max)) ;

		{undefined,true} ->

		    ?SUCHTHAT(Float,randFlt(Min, Max),
			      (Float >= Min) and (Float < Max)) ;

		{true,true} ->

		    ?SUCHTHAT(Float,randFlt(Min, Max),
			      (Float > Min) and (Float < Max)) % and isMultipleFloat(Float,MultipleOf));
	    end;

        %% null
        %%     The JSON null value. 
        <<"null">> ->
            null();

        %% object
        %%     A JSON object.
        <<"object">> ->
            P = jsonschema:properties(Schema),
	    MaxProp = jsonschema:keyword(Schema, "maxProperties"),
	    MinProp = jsonschema:keyword(Schema, "minProperties", 0),
	    Required = jsonschema:keyword(Schema, "required"), %% values from properties
	    %_Req_prop = filter
	    io:format("Required is: ~p~n",[Required]),
            
	 io:format("Required is: ~p~n",[Required]),
            
	    case {Required} of
		{undefined} ->
		    ReqList = [];
		{_} ->
		    ReqList = Required
		    %ReqList = lists:map(fun (X) -> io:format("binary is: ~p~n",[X]),
		%				   binary_to_list(X)
					%end, Required)
	    end,
	    io:format("Final list: ~p~n",[ReqList]),
            
	    % TODO: regular expressions for generating properties
            % _PP = jsonschema:patternProperties(Schema),
	    %lists:map ( fun ({M,S}) -> processProperties({M,S},Required)
	%		end,
	%		P),
            {struct, lists:map (fun ({M,S}) ->

					%case processProperties({M,S},ReqList) of

					 %   true ->
                                        {M,json(S)}

					    %false ->
					%	case eqc_gen:pick(boolean()) of
					%	    true ->
					%		{M,json(S)};
					%	    false -> 
					%		{M,null}
					%	end
						    %eqc_gen:oneof([ [], [{M,json(S)}]])
				%	end
						%eqc_gen:oneof([ [] , {M,json(S)} ])
                                end,
                                P)};
	    %io:format("Object is: ~p",[P]);

        %% string
        %%     A JSON string.
        <<"string">> ->
	    MinLength = jsonschema:keyword(Schema,"minLength"),
	    MaxLength = jsonschema:keyword(Schema,"maxLength"),
	    _Pattern =  jsonschema:keyword(Schema,"pattern"),  %% to be added later

	    case MinLength of 

			undefined -> 
				Min = 0;

			_ -> 
				Min = MinLength
	    end,

	    case MaxLength of

			undefined ->
				Max = ?MAX_STR_LENGTH; 

			_ ->  
				Max = MaxLength
		end,			
	    

		?LET(Rand,randInt(Min,Max), 
			?LET(S, stringGen(Rand), list_to_binary(S)));
		


        %% any
        %%     Any JSON data, including "null".
        <<"any">> ->
	    any();
        %% Union types
        %%     An array of two or more *simple type definitions*.
        Types when is_list(Types) ->
	    eqc_gen:oneof
              (lists:map
                 (fun (Type) ->
                          ConcreteSchema = jsonschema:set_type(Schema,Type),
                          json(ConcreteSchema)
                  end,
                  Types))
    end.

array(Schema) ->
    eqc_gen:list(json(Schema)).

template(_Template) ->
    %% TODO: generator for template
    null().

null() ->
    null.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Integer generators
integer() ->
    eqc_gen:int().

multiple_of(M) ->
    ?LET(N, integer(), M * N).

multiple_of_min(Mul,Min) ->
    MinMul = Mul * (1 + (Min-1) div Mul),
    ?LET(N, nat(), MinMul + Mul * N).

multiple_of_max(Mul,Max) ->
    MaxMul = Mul * (Max div Mul),
    ?LET(N, nat(), MaxMul - Mul * N).

multiple_of_min_max(Mul,Min,Max) ->
    MinMul = (1 + (Min-1) div Mul),
    MaxMul = (Max div Mul),
    ?LET(N, eqc_gen:choose(MinMul,MaxMul), Mul * N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
number() ->
    eqc_gen:oneof([eqc_gen:int(),eqc_gen:real()]).

boolean() ->
    eqc_gen:bool().

string() ->
    % TODO: generator of valid JSON strings
    % Its not a good generator. Implementation has to be changed
    ?LET(Name,name(),list_to_binary(Name)).
	%?SIZED(Size,list_to_binary(name())).
stringGen(0) ->
	[];

stringGen(N) ->
	?LET({S,G},{eqc_gen:choose($a,$z), stringGen(N-1)}, [S|G]).


%random integer generator between Min and Max values
randInt (Min, Max) ->
	eqc_gen:choose(Min,Max).

%maybe its not very efficient
randFlt (Min, _) ->
    ?LET(Flt, eqc_gen:real(), Min + Flt).


propname() ->
    name().

name() ->
    eqc_gen:non_empty(eqc_gen:list(eqc_gen:choose($a,$z))).

any() ->
    eqc_gen:oneof
      ([string(),number(),integer(),boolean(),object(),array(),null()]).

object() ->
    %% TODO: generator for object
    %% (could it be integrated in... json/1?)
    null().

array() ->
    %% TODO: generator for array (no items)
    %% (it could be integrated in array/1)
    null().

isMultiple(N,Mul) when Mul > 0 ->
	N rem Mul == 0.

%test, not implemented yet
isMultipleFloat(F,Mul) when Mul > 0 ->
    is_integer(F/Mul).
