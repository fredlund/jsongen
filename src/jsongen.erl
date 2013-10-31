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


-module(jsongen).

-export([json/1]).

-compile(export_all).

%%LOGS
% -define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.

-define(MAX_ARRAY_SIZE,100).
-define(MAX_STR_LENGTH,100).
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
      Required = jsonschema:keyword(Schema, "required",[]),
      PatternProperties = jsonschema:patternProperties(Schema),
      AdditionalProperties = jsonschema:keyword(Schema, "additionalProperties",true),
      RawMaxProperties = jsonschema:maxProperties(Schema),
      MaxProperties =
	if
	  RawMaxProperties==undefined,
	  PatternProperties==undefined,
	  AdditionalProperties==false ->
	    %% The following if is wrong in that it does not handle
	    %% the case when there are required properties which are not
	    %% in properties.
	    if
	      Properties==[] -> 0;
	      true -> length(Properties)
	    end;
	  true ->
	    ?MAX_PROPERTIES
	end,

      ReqProps = [{P,S} || {P,S} <- Properties, lists:member(P, Required)],
      OptProps = [{P,S} || {P,S} <- Properties, not lists:member(P, Required)],

      case AdditionalProperties of 
        false -> 
          AddP = undefined;
        true -> 
          AddP = {};
        AddSchema ->
          AddP = AddSchema
      end,

      ?LOG("Max Properties: ~p~n",[MaxProperties]),
      ?LOG("Required is: ~p~n",[ReqProps]),
      ?LOG("Not Required is: ~p~n",[OptProps]),
      ?LOG("AddProps are: ~p~n", [AddP]),
      ?LOG("PatternProperties are: ~p~n",[PatternProperties]),

      MinOpts = MinProperties - length(ReqProps),
      MaxOpts = MaxProperties - length(ReqProps),
            
      case {PatternProperties,AddP} of
        {undefined,undefined} ->
          ?LET(N, randInt(MinOpts,length(OptProps)),
               ?LET(OptPropsGen, choose_n_properties(OptProps,N),
                    begin
                      RawProperties = 
                        [{P,json(S,Options)} ||
                          {P,S} <- ReqProps
                            ++ 
                            OptPropsGen
                        ],
                      case proplists:get_value(randomize_properties,Options,true) of
                        true ->
                          ?LET(Props,
                               randomize_list(RawProperties),
                               {struct, Props});
                        false ->
                          {struct,RawProperties}
                      end
                    end));
           
        {PatternProperties, undefined} ->
          ?LET(N_opt, randInt(0,length(OptProps)),
               ?LET(N_prop, randInt(MinOpts - N_opt,MaxOpts - N_opt),
                    ?LET({OptPropsGen, PatPropsGen},
                         {choose_n_properties(OptProps,N_opt),
                          create_patterns(PatternProperties,N_prop)},
                         begin
                           ?LOG("**FINAL PROPS: ~p~n",
                                [ReqProps ++ OptPropsGen ++ PatPropsGen]),
                           RawProperties = 
                             [{P,json(S,Options)} ||
                               {P,S} <- ReqProps
                                 ++ 
                                 OptPropsGen
                                 ++
                                 lists:concat(PatPropsGen)
                             ],
                           case proplists:get_value(randomize_properties,Options,true) of
                             true ->
                               ?LET(Props,
                                    randomize_list(RawProperties),
                                    {struct, Props});
                             false ->
                               {struct,RawProperties}
                           end
                         end)));

        {undefined,AddP} ->
          ?LOG("{Min,Max}: {~p,~p}~n",[MinOpts,MaxOpts]),
          ?LET(N_opt, randInt(0,length(OptProps)),
               ?LET(N_add, randInt(MinOpts - N_opt,MaxOpts - N_opt),
                    ?LET({OptPropsGen, AddPropsGen},
                         {choose_n_properties(OptProps,N_opt),
                          create_additionals(AddP,N_add)},
                         begin
                           RawProperties = 
                             [{P,json(S,Options)} ||
                               {P,S} <- ReqProps
                                 ++ 
                                 OptPropsGen
                                 ++
                                 AddPropsGen
                             ],
                           case proplists:get_value(randomize_properties,Options,true) of
                             true ->
                               ?LET(Props,
                                    randomize_list(RawProperties),
                                    {struct, Props});
                             false ->
                               {struct,RawProperties}
                           end
                         end)));
        
        {PatternProperties,AddP} ->
          ?LET(N_opt, randInt(0,length(OptProps)),
               ?LET(N_pat, randInt(0, MaxOpts - N_opt),
                    ?LET(N_add, randInt(MinOpts - N_opt - N_pat, MaxOpts - N_opt - N_pat),
                         ?LET({OptPropsGen,PatPropsGen,AddPropsGen},
                              {choose_n_properties(OptProps,N_opt),
                               create_patterns(PatternProperties,N_pat),
                               create_additionals(AddP,N_add)},
                              begin
                                RawProperties = 
                                  [{P,json(S,Options)} ||
                                    {P,S} <- ReqProps
                                      ++ 
                                      OptPropsGen
                                      ++
                                      PatPropsGen
                                      ++
                                      AddPropsGen
                                  ],
                                case proplists:get_value(randomize_properties,Options,true) of
                                  true ->
                                    ?LET(Props,
                                         randomize_list(RawProperties),
                                         {struct, Props});
                                  false ->
                                    {struct,RawProperties}
                                end
                              end))))
          end;

          %% case AdditionalProperties of
          %%     undefined ->

          %%         ?LET(OptPropsGen, choose_properties(OptProps,MinOpts,MaxOpts),
          %%              ?LET(PatternGen, create_patterns(PatternProperties, 
          %%                                               MinOpts - length(OptPropsGen)),
          %%                   ?LET(PatternPropsGen, choose_properties(lists:concat(PatternGen), 
          %%                                               MinOpts - length(OptPropsGen), MaxOpts), 
          %%                        begin
          %%                           ?LOG("*** FINAL PROPERTIES: ~p***~n",
          %%                                [length(OptPropsGen ++ PatternPropsGen ++ ReqProps)]),
          %%                       RawProperties = 
          %%                           [{P,json(S,Options)} ||
          %%                               {P,S} <- ReqProps
          %%                                   ++ 
          %%                                   OptPropsGen ++ PatternPropsGen
          %%                           ],
          %%                       case proplists:get_value(randomize_properties,Options,true) of
          %%                           true ->
          %%                               ?LET(Props,
          %%                                    randomize_list(RawProperties),
          %%                                    {struct, Props});
          %%                           false ->
          %%                               {struct,RawProperties}
          %%                       end
          %%                   end)));        



          %%     %% THIS IMPLEMENTATION HAS TO BE UPDATED
          %%     _ ->
          %%   ?LET({PatternGen,AdditionalGen},
	  %%        {create_patterns(PatternProperties),[]},
          %%        ?LET(Optionals, 
          %%             choose_properties
	  %%       	(OptProps
	  %%       	 ++ lists:concat(PatternGen)
	  %%       	 ++ AdditionalGen,
	  %%       	 MinOpts, MaxOpts),    
	  %%             begin
	  %%       	RawProperties = 
	  %%       	  [{P,json(S,Options)} ||
	  %%       	    {P,S} <- ReqProps
	  %%       	      ++ 
	  %%       	      Optionals
	  %%       	  ],
	  %%       	case proplists:get_value(randomize_properties,Options,true) of
	  %%       	  true ->
	  %%       	    ?LET(Props,
	  %%       		 randomize_list(RawProperties),
	  %%       		 {struct, Props});
	  %%       	  false ->
	  %%       	    {struct,RawProperties}
	  %%       	end
	  %%             end))
          %%     end;
        
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
        
        %% any
        %%     Any JSON data, including "null".
        <<"any">> ->
          ?LOG("'any' keyword found",[]),
	    anyQ();


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
    ?LOG("Min/Mul is ~p~n",[(Min/Mul)]),
    MinMul = case (Min/Mul) == trunc(Min/Mul) of
        % if you add 1 unit, Minimum value will never be generated but its inside the valid range
                 true -> 
                     ?LOG("TRUE~n",[]),
                     trunc(Min/Mul); 
                 false -> 
                     ?LOG("FALSE~n",[]),
                     1 + floor(Min/Mul)
             end,
    MaxMul = floor(Max/  Mul),

    ?LOG ("MinMul is ~p~n",[MinMul]),
    case {MinExc,MaxExc} of

        {true,true} ->
            ?SUCHTHAT(N, 
                      ?LET(N, eqc_gen:choose(MinMul,MaxMul), Mul*N),
                      (N =/= Min) and (N =/= Max));
        
        {true,false} ->
            ?SUCHTHAT(N, 
                      ?LET(N, eqc_gen:choose(MinMul,MaxMul), Mul*N),
                      (N =/= Min));
        
        {false,true} ->
            ?SUCHTHAT(N, 
                      ?LET(N, eqc_gen:choose(MinMul,MaxMul), Mul*N),
                      (N =/= Max));
        
        {false,false} ->
            ?LET(N, eqc_gen:choose(MinMul,MaxMul), 
                 Mul * N)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Basic schema generators for additional

anyType() ->
    ?LOG("Choosing random type ('any' keyword)...~n",[]),
     eqc_gen:oneof
      ([stringType(),numberType(),integerType(),booleanType()]).


-spec stringType() -> eqc_gen:gen(string()).
stringType() ->
    {struct,[{<<"type">>,<<"string">>}]}.

numberType() ->
    {struct,[{<<"type">>,<<"number">>}]}.

integerType() ->
    {struct,[{<<"type">>,<<"integer">>}]}.

-spec booleanType() -> eqc_gen:gen(boolean()).
booleanType() ->
    {struct,[{<<"type">>,<<"boolean">>}]}.


anyQ() ->
    ?LOG("Choosing random type ('any' keyword)...~n",[]),
     eqc_gen:oneof
      ([stringSchema(),numberSchema(),integerSchema(),booleanSchema(),objectSchema(),arraySchema(),null()]).

-spec stringSchema() -> eqc_gen:gen(string()).
stringSchema() ->
    json({struct,[{<<"type">>,<<"string">>}]}).
    % TODO: generator of valid JSON strings
    % Its not a good generator. Implementation has to be changed
    %?LET(Name,name(),list_to_binary(Name)).
    %?SIZED(Size,list_to_binary(name())).

numberSchema() ->
    json({struct,[{<<"type">>,<<"number">>}]}).

integerSchema() ->
    json({struct,[{<<"type">>,<<"integer">>}]}).

-spec booleanSchema() -> eqc_gen:gen(boolean()).
booleanSchema() ->
    json({struct,[{<<"type">>,<<"boolean">>}]}).


objectSchema() ->
  json({struct,[{<<"type">>,<<"object">>},{<<"properties">>,{struct,[]}},{<<"additionalProperties">>,<<"false">>}]}).

arraySchema() ->
    json({struct,[{<<"type">>,<<"array">>},{<<"items">>,{struct,[{<<"type">>,<<"integer">>}]}}]}).
  %?LET(Size,nat(),lists:map(fun (_) -> any() end, lists:duplicate(Size,32))).

nullSchema() ->
    json({struct,[{<<"type">>,<<"null">>}]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
randString() ->
    ?LET(N, positive(),
         ?LET(S,stringGen(N), list_to_binary(S))).

-spec stringGen(integer()) -> eqc_gen:gen(string()).
stringGen(0) ->
    [];

stringGen(N) ->
    ?LET({S,G},{eqc_gen:choose($a,$z), stringGen(N-1)}, [S|G]).


%random integer generator between Min and Max values

-spec randInt2 (integer(), integer()) -> eqc_gen:gen(integer()).
randInt2 (Min, Max) ->
	eqc_gen:choose(Min,Max).

randInt (Min,Max) ->

    case {Min,Max} of 
            
        {undefined,undefined} -> 
            positive();
        {undefined,Max} ->
            eqc_gen:choose(1,Max);
        {Min,undefined} ->
            natural_gte(Min);
        {Min,Max} -> 
            eqc_gen:choose(Min,Max)
    end.

-spec randFlt(float(), float()) -> eqc_gen:gen(float()).
randFlt (Min, _) ->
    ?LET(Flt, eqc_gen:real(), Min + Flt).

-spec boolean() -> eqc_gen:gen(boolean()).
boolean() ->
    eqc_gen:bool().

%% candidate for removal
propname() ->
    name().


% candidate for removal
name() ->
    eqc_gen:non_empty(eqc_gen:list(eqc_gen:choose($a,$z))).


isMultiple(N,Mul) when Mul > 0 ->
	N rem Mul == 0.

%test, not implemented yet
isMultipleFloat(F,Mul) when Mul > 0 ->
    is_integer(F/Mul).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generators for patternProperties keyword

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
    io:format("~n** THIS MAY TAKE A WHILE **~n"),
    io:format("~nLOADING..."),
    ?LET(N,natural(), pattern_gen(Pattern_Schema,N)).

pattern_gen_range(Pattern_Schema, Min) ->
    ?LOG("{Pattern_schema} = ~p~n", [Pattern_Schema]),
    ?LET(N,natural_gte(Min), pattern_gen(Pattern_Schema,N)).
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

create_patterns(undefined,_) ->
[];

create_patterns(PatternPropList, MinimumProps) ->
    ?LOG("create_patterns with minmum, PatternPropList is ~p, and Min is ~p~n",
         [PatternPropList, MinimumProps]),
    Min = ceiling(MinimumProps / length(PatternPropList)),
    io:format("~n** THIS MAY TAKE A WHILE **~n"),
    io:format("~nLOADING..."),
    L = lists:map (fun(X) -> pattern_gen_range(X,Min) end, PatternPropList),
    ?LOG("Final patterns created: ~p~n",[L]),
    L.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generators for additionalProperties keyword

%% -spec create_additionals ( json:json_term(), integer(), integer()) -> list(eqc_gen:gen(json:json_term())).

%% create_additionals({struct, AddTypes}, AddRand, StrRand) ->
%%     lists:foldl( fun (Add, _Res) ->
%%                          additional_gen(Add,AddRand,StrRand)
%%                  end,[],AddTypes).
%% %%%%%%%%%%%%%%%%%

%% % <<"i.*">>
%% property_name(Pattern) ->
%%     ?LOG("Pattern name: ~p~n",[Pattern]),
%%     RegularExpression = binary_to_list(Pattern),
%%     InternalRegularExpression = regexp_parse:string(RegularExpression),
%%     ?LET
%%        (String,
%%         gen_string_from_regexp:gen(InternalRegularExpression),
%%         list_to_binary(String)).

%% % {<<"i.*">>, {struct, [{<<"type">>,<<"integer">>}]}}
%% pattern_gen(_,0) ->
%%     [];
%% pattern_gen({Pattern, Schema},N) when N > 0 ->
%%     ?LOG("{Pattern, schema} = ~p~n", [{Pattern,Schema}]),
%%     [{property_name(Pattern), Schema} | pattern_gen({Pattern,Schema}, N-1)].

%% pattern_gen(Pattern_Schema) ->
%%     ?LOG("{Pattern_schema} = ~p~n", [Pattern_Schema]),
%%     io:format("~n** THIS MAY TAKE A WHILE **~n"),
%%     io:format("~nLOADING..."),
%%     ?LET(N,natural(), pattern_gen(Pattern_Schema,N)).

%% pattern_gen_range(Pattern_Schema, Min) ->
%%     ?LOG("{Pattern_schema} = ~p~n", [Pattern_Schema]),
%%     ?LET(N,natural_gte(Min), pattern_gen(Pattern_Schema,N)).
%% % { "i.*" : { "type" : "integer" }, "s.*" : { "type" : "string" } }
%% % 

%% %{struct, [{<<"i.*">>, {struct, [{<<"type">>,<<"integer">>}]}}, {<<"s.*">>, {struct, [{ <<"type">>,<<"string">>}]}}]}
%% create_patterns(undefined) ->
%% [];

%% create_patterns(PatternPropList) ->
%%     ?LOG("Inside create_patterns, PatternPropList is ~p~n",[PatternPropList]),
%%     L = lists:map (fun(X) -> pattern_gen(X) end, PatternPropList),
%%     ?LOG("Final patterns created: ~p~n",[L]),
%%     L.
%%%%%%%%

additional_gen (_,0) ->
[];

additional_gen(AdditionalSchema,N) when N > 0 ->
    case AdditionalSchema of
        {} ->
            [{randString(),anyType()} | additional_gen(AdditionalSchema,N-1)];
        Schema ->
            [{randString(),Schema} | additional_gen(AdditionalSchema,N-1)]
    end.

create_additionals({},N) ->
    additional_gen({},N);

create_additionals(AddPropList,N) ->
    ?LOG("create_additionals with fix value, AdditionalPropList is ~p, and N is ~p~n",
            [AddPropList, N]),
    FinalProps = ceiling(N / length(AddPropList)),
    io:format("~n** THIS MAY TAKE A WHILE **~n"),
    io:format("~nLOADING..."),
    L = lists:map (fun(X) -> additional_gen(X,FinalProps) end, AddPropList),
    ?LOG("Final patterns created: ~p~n",[L]),
    L.


%%%%%%%%%%%%%%%%%%%%%
%% -spec additional_gen (json:json_term(), integer(), integer()) -> list(eqc_gen:gen(json:json_term())).

%% additional_gen(_Schema,0,_) ->
%%     [];

%% additional_gen(Schema,N, Length) when N > 0 ->

%%          [ { stringGen(Length) , {struct,[Schema]}}  | additional_gen(Schema,N-1,Length)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Misc functions

% -spec choose_properties([string()],natural(),natural()) -> eqc_gen:gen([string()]).

choose_properties(P, Min, Max) when Max >= Min -> 
    ?LOG("Choosing properties: ~p~n",[P]),
    ?LOG("Min, Max: ~p,~p~n",[Min,Max]),
    ?LET(N, eqc_gen:choose (max(0,Min),Max), choose_n_properties(P,N)).
    

choose_n_properties(L,N) ->
  randomize_list(L,N,length(L)).

randomize_list(L) ->
  Length = length(L),
  randomize_list(L,Length,Length).
randomize_list([],_,_Length) -> [];
randomize_list(_List,0,_Length) -> [];
randomize_list(List, N, Length) ->
    ?LET(I, eqc_gen:choose(1, Length), 
    [lists:nth(I,List) |
     randomize_list(delete_nth_element(I,List), N-1, Length-1)]).

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
    io:format("."),
    delete_nth_element(N-1,List, []).

delete_nth_element(0, [_nthEl|T], Res) ->
    concat_and_reverse(T, Res);

delete_nth_element(N, [H|T], Res) ->
    delete_nth_element(N-1, T, [H|Res]).

concat_and_reverse([],Res) ->
    io:format("."),
    lists:reverse(Res);

concat_and_reverse([H|T], Res) ->
    concat_and_reverse(T, [H|Res]).

ceiling(X) ->
    T = trunc(X),
    case (X - T) of

        Negative when Negative < 0 -> T;
        Positive when Positive > 0 -> T + 1;
        _ -> T

    end.


