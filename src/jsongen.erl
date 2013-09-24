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

-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.

-define(MAX_INT_VALUE,100000).
-define(MAX_STR_LENGTH,1000).

%% Mochijson2 should export a better type...
-opaque json_term() :: any().

-include_lib("eqc/include/eqc.hrl").

%% @doc
%% Translates a JSON schema into an Erlang QuickCheck generator.
-spec json(json_term()) -> eqc_gen:gen(json_term()).

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
			ExcMaxScanned = jsonschema:keyword(Schema,"exclusiveMaximum"),
			MinScanned = jsonschema:keyword(Schema,"minimum"),
			ExcMinScanned = jsonschema:keyword(Schema,"exlusiveMinimum"),
		   	MultipleOf = jsonschema:keyword(Schema,"multipleOf",1),

			% Setting up keywords
			case {MaxScanned,ExcMaxScanned} of
				
				{undefined,undefined} ->
					Max = ?MAX_INT_VALUE;
				

				%% There should be an error case here. Json-schema doc says that if exclusiveMaximum is present, maximum MUST be present as well

				{undefined, true} ->
					Max = ?MAX_INT_VALUE - 1;

				{MaxScanned, true} ->
					Max = MaxScanned -1;

				{MaxScanned, _} ->
					Max = MaxScanned
			
			end,
			

			case {MinScanned,ExcMinScanned} of
				
				{undefined,undefined} ->
					Min = 0;
				

				%% There should be an error case here. Json-schema doc says that if exclusiveMinimum is present, minimum MUST be present as well

				{undefined, _} ->
					Min = 1;

				{MinScanned, true} ->
					Min = MinScanned + 1;

				{MinScanned, _} ->
					Min = MinScanned
			
			end,

 
			%Creating the generator

			?SUCHTHAT(Int,randInt(Min,Max), 
					  isMultiple(Int,MultipleOf));


        %% Number
        %%     Any JSON number. Number includes integer.
        <<"number">> ->
			MaxScanned = jsonschema:keyword(Schema,"maximum"),
			ExcMaxScanned = jsonschema:keyword(Schema,"exclusiveMaximum"),
			MinScanned = jsonschema:keyword(Schema,"minimum"),
			ExcMinScanned = jsonschema:keyword(Schema,"exlusiveMinimum"),
		   	MultipleOf = jsonschema:keyword(Schema,"multipleOf",1),

			% Setting up keywords
			case {MaxScanned,ExcMaxScanned} of
				
				{undefined,undefined} ->
					Max = ?MAX_INT_VALUE;
				
				%% There should be an error case here. Json-schema doc says that if exclusiveMaximum is present, maximum MUST be present as well

				{undefined, true} ->
					Max = ?MAX_INT_VALUE - 0.1;

			    %% Exclusive maximum for floats???
				{MaxScanned, true} ->
					Max = MaxScanned - 0.1;

				{MaxScanned, _} ->
					Max = MaxScanned
			
			end,
			

			case {MinScanned,ExcMinScanned} of
				
				{undefined,undefined} ->
					Min = 0;
				

				%% There should be an error case here. Json-schema doc says that if exclusiveMinimum is present, minimum MUST be present as well

				{undefined, _} ->
					Min = 1;

				{MinScanned, true} ->
					Min = MinScanned + 0.1;

				{MinScanned, _} ->
					Min = MinScanned
			
			end,



	    ?SUCHTHAT(Float,randFlt(Min, Max),
		      (Float > Min) and (Float < Max)); % and isMultipleFloat(Float,MultipleOf));


        %% null
        %%     The JSON null value. 
        <<"null">> ->
            null();

        %% object
        %%     A JSON object.
        <<"object">> ->
            P = jsonschema:properties(Schema),
	    MaxProp = jsonschema:keyword(Schema, "maxProperties"),
	    MinProp = jsonschema:keyword(Schema, "minProperties"),
	    Required = jsonschema:keyword(Schema, "required"), %% values from properties
	    
            % TODO: regular expressions for generating properties
            % _PP = jsonschema:patternProperties(Schema),
            {struct, lists:map (fun ({M,S}) ->
                                        {M,json(S)}
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

integer() ->
    eqc_gen:int().

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

isMultipleFloat(F,Mul) when Mul > 0 ->
    is_integer(F/Mul).
