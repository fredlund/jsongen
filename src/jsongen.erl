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
%% @author Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund 
%% (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)
%% @copyright 2013 Ángel Herranz, Lars-Ake Fredlund, Sergio Gil 
%%

-module(jsongen).

-export([json/1]).

%%-compile(export_all).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.

-define(MAX_ARRAY_SIZE,100).
-define(MAX_STR_LENGTH,1000).
-define(MAX_PROPERTIES,100).

-include_lib("eqc/include/eqc.hrl").

%% @doc
%% Translates a JSON schema into an Erlang QuickCheck generator.
-spec json(json:json_term()) -> eqc_gen:gen(json:json_term()).
json(Schema) ->
  json(Schema,[]).

json(Schema,Options) ->
  ?LOG("json(~p)~n",[Schema]),
  case jsonschema:anyOf(Schema) of
    undefined ->
      case jsonschema:ref(Schema) of
        undefined ->                        
          case jsonschema:hasType(Schema) of
            true ->
              gen_typed_schema(Schema,Options);
            false ->
              case jsonschema:hasEnum(Schema) of
                true -> 
                  eqc_gen:oneof(jsonschema:enumerated(Schema));
                false ->
                  throw(bad)
              end
          end;
        Ref ->
          {ok, RefSch} = jsonschema:read_file(Ref),
          ?LAZY(jsongen:json(RefSch))
      end;
    Schemas ->
      eqc_gen:oneof([json(S) || S <- Schemas])
  end.

gen_typed_schema(Schema,Options) ->
  case jsonschema:type(Schema) of
    
    %% array
    %%     A JSON array. 
    <<"array">> ->
      MaxItems = jsonschema:keyword(Schema,"maxItems"),
      MinItems = jsonschema:keyword(Schema,"minItems",0),
      _UniqueItems = 0,
      case jsonschema:items(Schema) of
	{itemSchema, ItemSchema} ->
	  array(ItemSchema, {MinItems,MaxItems});
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
	undefined ->
	  Max = undefined;
	_ ->
	  case ExcMaxScanned of
	    true -> Max = MaxScanned -1;
	    false -> Max = MaxScanned
	  end
      end,	    
      
      case MinScanned of
	undefined ->
	  Min = undefined;
	_ ->
	  case ExcMinScanned of
	    true -> Min = MinScanned +1;
	    false -> Min = MinScanned
	  end
      end,
      
      case {Min, Max} of
	{undefined, undefined} -> multiple_of(Multiple);
	{Min, undefined} -> multiple_of_min(Multiple, Min);
	{undefined, Max} -> multiple_of_max(Multiple, Max);
	{Min, Max} -> multiple_of_min_max(Multiple, Min,Max)
      end;
    
    %% Number
    %%     Any JSON number. Number includes integer.
    <<"number">> ->
      Max = jsonschema:keyword(Schema,"maximum"),
      ExcMax = jsonschema:keyword(Schema,"exclusiveMaximum",false),
      Min = jsonschema:keyword(Schema,"minimum"),
      ExcMin = jsonschema:keyword(Schema,"exclusiveMinimum",false),
      Mul = jsonschema:keyword(Schema,"multipleOf",1),
      
      
      case {Min, Max} of
	
	{undefined, undefined} ->
	  number();
	
	{Min, undefined} ->
	  number_mul_min(Mul,Min,ExcMin);
	
	{undefined, Max} ->
	  number_mul_max(Mul,Max, ExcMax);
	
	{Min,Max} ->
	  ?LOG("number_mul_min_max(Mul,Min,Max,{ExcMin,ExcMax}):  number_mul_min_max(~p,~p,~p,{~p,~p})",[Mul,Min,Max,ExcMin,ExcMax]),
	  number_mul_min_max(Mul,Min,Max,{ExcMin,ExcMax})
      end;
    
    
    
    %% null
    %%     The JSON null value. 
    <<"null">> ->
      null();
    
    
    %% object
    %%     A JSON object.
    <<"object">> ->
      Properties = jsonschema:properties(Schema),
      MinProperties = jsonschema:minProperties(Schema, 0),
      MaxProperties = jsonschema:maxProperties(Schema, ?MAX_PROPERTIES),
      Required = jsonschema:keyword(Schema, "required",[]),
      PatternProperties = jsonschema:patternProperties(Schema),
      
      _AdditionalProperties = jsonschema:keyword(Schema, "additionalProperties", {}),


            ReqProps = [{P,S} || {P,S} <- Properties, lists:member(P, Required)],
            OptProps = [{P,S} || {P,S} <- Properties, not lists:member(P, Required)],

            %% case AdditionalProperties of 
            %%     false -> 
            %%         AddP = [];

            %%     true -> 
            %%         AddP = {};

            %%     {} ->
            %%         AddP = {};

            %%   AddSchema ->
            %%         AddP = AddSchema
            %% end,

            ?LOG("Max Properties: ~p~n",[MaxProperties]),
	    ?LOG("Required is: ~p~n",[ReqProps]),
	    ?LOG("Not Required is: ~p~n",[OptProps]),
            %?LOG("Additional Prop are: ~p~n", [AddP]),
            ?LOG("PatternProperties are: ~p~n",[PatternProperties]),

            case length(ReqProps) > MinProperties of
                true -> Min = 0;
                false -> Min = MinProperties - length(ReqProps)
            end,
            Max = MaxProperties - length(ReqProps),
            
            ?LET({PatternGen,AdditionalGen}, {create_patterns(PatternProperties),[]},
                 ?LET(Optionals, 
                      choose_properties(OptProps ++ lists:concat(PatternGen) ++ AdditionalGen , Min, Max),    

                      {
                        struct,
                        [{P,json(S,Options)} || {P,S} <- ReqProps
                                            ++ 
                                            Optionals
                        ]
                      }));
        
        %% string
        %%     A JSON string.
        <<"string">> ->
	    MinLength = jsonschema:keyword(Schema,"minLength"),
	    MaxLength = jsonschema:keyword(Schema,"maxLength"),
	    Pattern =  jsonschema:keyword(Schema,"pattern"),  %% to be added later
            
	    %% Currently we do not like length specifications 
	    %% combined with regular expressions. Will this change? 
	    %% Maybe, it is not easy to do.
	    if
                ((MinLength=/=undefined) orelse (MaxLength=/=undefined)) andalso
                (Pattern=/=undefined) ->
                    io:format
                      ("Specifying both minimum or maximum string lengths "++
                       "and a regular expression is not currently supported"),
                    throw(nyi);
                true -> ok
	    end,
            

           if (Pattern == undefined) ->
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
   
                
                true ->
                    ?LOG("Pattern is: ~p~n",[Pattern]),
                    property_name(Pattern)
            end;

      %% VVV This code wasn't working if there is no keywords for a string type schema VVV
            %% if 
            %%     (MinLength=/=undefined) orelse (MaxLength=/=undefined) ->
            %%         case MinLength of 
            %%             undefined -> 
            %%                 Min = 0;
            %%             _ -> 
            %%                 Min = MinLength
            %%         end,
                    
            %%         case MaxLength of
            %%             undefined ->
            %%                 Max = ?MAX_STR_LENGTH; 
            %%             _ ->  
            %%                 Max = MaxLength
            %%         end,			
                    
            %%         ?LET(Rand,randInt(Min,Max), 
            %%              ?LET(S, stringGen(Rand), list_to_binary(S)));
   
                
            %%     true ->
            %%         ?LOG("Pattern is: ~p~n",[Pattern]),
            %%         property_name(Pattern)
            %% end;
        
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
                          json(ConcreteSchema,Options)
                  end,
                  Types))
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Array gen

-spec array(json:json_term(),{integer(),integer()}) -> eqc_gen:gen(json:json_term()).
array(Schema,{MinItems,MaxItems}) ->

    case MaxItems of
	undefined ->
	    ?LET(N, eqc_gen:choose(MinItems, ?MAX_ARRAY_SIZE), arrayGen(Schema,N));

	MaxItems ->
	    ?LET(N, eqc_gen:choose(MinItems,MaxItems), arrayGen(Schema,N))
    end.		   


-spec arrayGen (json:json_term(), integer()) -> eqc_gen:gen(json:json_term()).
arrayGen(_Schema,0) ->
    [];

arrayGen(Schema,N) when N > 0->
    [json(Schema) | arrayGen(Schema,N-1)].


template(_Template) ->
    %% TODO: generator for template
    null().

null() ->
    null.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Integer generators

-spec integer () -> eqc_gen:gen(integer()).
integer() ->
    eqc_gen:int().

-spec multiple_of (integer()) -> eqc_gen:gen(integer()).
multiple_of(M) ->
    ?LET(N, integer(), M * N).

-spec multiple_of_min(integer(), integer()) -> eqc_gen:gen(integer()).
multiple_of_min(Mul,Min) ->
    MinMul = Mul * (1 + (Min-1) div Mul),
    ?LET(N, nat(), MinMul + Mul * N).

-spec multiple_of_max(integer(), integer()) -> eqc_gen:gen(integer()).
multiple_of_max(Mul,Max) ->
    MaxMul = Mul * (Max div Mul),
    ?LET(N, nat(), MaxMul - Mul * N).

-spec multiple_of_min_max(integer(), integer(), integer()) -> eqc_gen:gen(integer()).
multiple_of_min_max(Mul,Min,Max) ->
    MinMul = (1 + (Min-1) div Mul),
    MaxMul = (Max div Mul),
    ?LET(N, eqc_gen:choose(MinMul,MaxMul), Mul * N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Natural numbers

-spec natural() -> eqc_gen:gen(integer()).
natural() ->
    eqc_gen:nat().

-spec positive() -> eqc_gen:gen(integer()).
positive() ->
    natural_gte(1).

-spec natural_gte(integer()) -> eqc_gen:gen(integer()).
natural_gte(K) ->
    ?LET(N,natural(),N+K).
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Number generators

-spec number() -> eqc_gen:gen(integer()) | eqc_gen:gen(float()).
number() ->
    eqc_gen:oneof([eqc_gen:int(),eqc_gen:real()]).

-spec number_positive() -> eqc_gen:gen(integer()) | eqc_gen:gen(float()).
number_positive() ->
    ?SUCHTHAT(N, eqc_gen:oneof([eqc_gen:int(),eqc_gen:real()]), N>=0).

-spec number_mul(integer() | float()) -> eqc:gen_gen(integer()) | eqc:gen_gen(float()).
number_mul(Mul) ->
    ?LET(N, integer(), Mul * N).

-spec number_mul_min(integer() | float(), integer() | float(), boolean()) ->  eqc:gen_gen(integer()) | eqc:gen_gen(float()).
number_mul_min(Mul,Min,MinExc) ->
    MinMul = Mul * (1 + floor( (Min-1) / Mul)),
    case MinExc of
	true ->
	    ?SUCHTHAT(N, MinMul + Mul * nat(), N /= Min);
        
	false ->
	    ?LET(N,nat(),MinMul + Mul * N)
    end.

-spec number_mul_max(integer() | float(), integer() | float(), boolean()) ->  eqc:gen_gen(integer()) | eqc:gen_gen(float()).
number_mul_max(Mul,Max,MaxExc) ->
    MaxMul = Mul * (Max div Mul),
    
    case MaxExc of
	true ->
	    ?SUCHTHAT(N, MaxMul - Mul * nat(), N /= Max);
	false ->
	    ?LET(N, nat(), MaxMul - Mul * N)
    end.

-spec number_mul_min_max(integer() | float(), integer() | float(), integer() | float(), tuple(boolean(), boolean())) ->  eqc:gen_gen(integer()) | eqc:gen_gen(float()).
number_mul_min_max(Mul,Min,Max,{MinExc,MaxExc}) ->
    MinMul = (1 + floor((Min-1) / Mul)),
    MaxMul = floor(Max/  Mul),

    case {MinExc,MaxExc} of

        {true,true} ->
            ?SUCHTHAT(N, eqc_gen:choose(MinMul,MaxMul), 
                      ((N * Mul)/= Min) and ((N * Mul) /= Max));
        
        {true,false} ->
            ?SUCHTHAT(N, eqc_gen:choose(MinMul,MaxMul), 
                      (N * Mul) /= Min);
        
        {false,true} ->
            ?SUCHTHAT(N, eqc_gen:choose(MinMul,MaxMul), 
                      (N * Mul) /= Max);
        
        {false,false} ->
            ?LET(N, eqc_gen:choose(MinMul,MaxMul), 
                 Mul * N)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec boolean() -> eqc_gen:gen(boolean()).
boolean() ->
    eqc_gen:bool().

-spec string() -> eqc_gen:gen(string()).
string() ->
    % TODO: generator of valid JSON strings
    % Its not a good generator. Implementation has to be changed
    ?LET(Name,name(),list_to_binary(Name)).
    %?SIZED(Size,list_to_binary(name())).


-spec stringGen(integer()) -> eqc_gen:gen(string()).
stringGen(0) ->
    [];

stringGen(N) ->
    ?LET({S,G},{eqc_gen:choose($a,$z), stringGen(N-1)}, [S|G]).


%random integer generator between Min and Max values

-spec randInt (integer(), integer()) -> eqc_gen:gen(integer()).
randInt (Min, Max) ->
	eqc_gen:choose(Min,Max).

%maybe its not very efficient

-spec randFlt(float(), float()) -> eqc_gen:gen(float()).
randFlt (Min, _) ->
    ?LET(Flt, eqc_gen:real(), Min + Flt).

%% candidate for removal
propname() ->
    name().


% candidate for removal
name() ->
    eqc_gen:non_empty(eqc_gen:list(eqc_gen:choose($a,$z))).

any() ->
    eqc_gen:oneof
      ([string(),number(),integer(),boolean(),object(),array(),null()]).

object() ->
    %% TODO: generator for object
    %% (could it be integrated in... json/1?)
  json({struct,[{<<"type">>,<<"object">>}]}).

array() ->
    %% TODO: generator for array (no items)
    %% (it could be integrated in array/1)
  ?LET(Size,nat(),lists:map(fun (_) -> any() end, lists:duplicate(Size,32))).

isMultiple(N,Mul) when Mul > 0 ->
	N rem Mul == 0.

%test, not implemented yet
isMultipleFloat(F,Mul) when Mul > 0 ->
    is_integer(F/Mul).

% -spec choose_properties([string()],natural(),natural()) -> eqc_gen:gen([string()]).
choose_properties(P, Min, Max) when Max >= Min -> 
    ?LOG("Choosing properties: ~p~n",[P]),
    ?LOG("Min, Max: ~p,~p~n",[Min,Max]),
    ?LET(N, eqc_gen:choose (Min,Max), choose_n_properties(P,N,Min)).
    

choose_n_properties(_List,0,_Min) ->
    [];
choose_n_properties([],_,Min) when Min =< 0 ->
    [];
choose_n_properties(List, N, Min) ->
    ?LOG("N is  ~p ~n",[N]),
    ?LET(I, eqc_gen:choose(1, length(List)), 
    [lists:nth(I,List) | choose_n_properties(delete_nth_element(I,List), N-1, Min -1)]).

floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;

floor(X) ->
    trunc(X).

delete_nth_element(N, List) ->
    ?LOG("Removing ~p element from list -> ~p~n",[N,List]),
    delete_nth_element(N-1,List, []).

delete_nth_element(0, [_nthEl|T], Res) ->
    concat_and_reverse(T, Res);

delete_nth_element(N, [H|T], Res) ->
    delete_nth_element(N-1, T, [H|Res]).

concat_and_reverse([],Res) ->
    lists:reverse(Res);

concat_and_reverse([H|T], Res) ->
    concat_and_reverse(T, [H|Res]).

% <<"i.*">>
property_name(Pattern) ->
    ?LOG("Pattern name: ~p~n",[Pattern]),
    RegularExpression = binary_to_list(Pattern),
    InternalRegularExpression = regexp_parse:string(RegularExpression),
    ?LET
       (String,
        gen_string_from_regexp:gen(InternalRegularExpression),
        list_to_binary(String)).

% {<<"i.*">>, {struct, [{<<"type">>,<<"integer">>}]}}
pattern_gen(_,0) ->
    [];
pattern_gen({Pattern, Schema},N) when N > 0 ->
    ?LOG("{Pattern, schema} = ~p~n", [{Pattern,Schema}]),
    [{property_name(Pattern), Schema} | pattern_gen({Pattern,Schema}, N-1)].

pattern_gen(Pattern_Schema) ->
    ?LOG("{Pattern_schema} = ~p~n", [Pattern_Schema]),
    ?LET(N,natural(), pattern_gen(Pattern_Schema,N)).


% { "i.*" : { "type" : "integer" }, "s.*" : { "type" : "string" } }
% 

%{struct, [{<<"i.*">>, {struct, [{<<"type">>,<<"integer">>}]}}, {<<"s.*">>, {struct, [{ <<"type">>,<<"string">>}]}}]}
create_patterns(undefined) ->
[];

create_patterns(PatternPropList) ->
    ?LOG("Inside create_patterns, PatternPropList is ~p~n",[PatternPropList]),
    L = lists:map (fun(X) -> pattern_gen(X) end, PatternPropList),
    ?LOG("Final patterns created: ~p~n",[L]),
    L.



%%% INTEGER OR GEN. OF INTEGER????
-spec create_additionals ( json:json_term(), integer(), integer()) -> list(eqc_gen:gen(json:json_term())).

create_additionals({struct, AddTypes}, AddRand, StrRand) ->
    lists:foldl( fun (Add, _Res) ->
                         additional_gen(Add,AddRand,StrRand)
                 end,[],AddTypes).



-spec additional_gen (json:json_term(), integer(), integer()) -> list(eqc_gen:gen(json:json_term())).

additional_gen(_Schema,0,_) ->
    [];

additional_gen(Schema,N, Length) when N > 0 ->

         [ { stringGen(Length) , {struct,[Schema]}}  | additional_gen(Schema,N-1,Length)].
