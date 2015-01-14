%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro <lcastro@udc.es>
%%% @copyright (C) 2014
%%% @doc
%%%     Utility module to inspect code of test properties and models.
%%% @end
%%%-------------------------------------------------------------------

-module(readspec_inspect).

-include("readspec.hrl").
-include("records.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([model_description/1, property_description/2]).
-export([property_definition/3, property_definition/4]).
-export([falsify/1]).

%% @doc Extracts the module description from the edoc comments on the source code
%% @end
-spec model_description(ModelModule :: atom()) -> ModuleDescription :: string().
model_description(ModelModule) ->
    extract_module_description(ModelModule).

%% @doc Extracts the property description from the edoc comments on the source code
%% @end
-spec property_description(ModelModule :: atom(),
                           PropertyName :: atom()) -> PropertyDescription :: string().
property_description(ModelModule, PropertyName) ->
    extract_function_description(ModelModule, PropertyName).

%% @doc Extracts the body of a property as a string from the source code
%% @end
-spec property_definition(ModelModule :: atom(),
			  PropertyName :: atom(),
			  Values :: any()) -> PropertyBody :: string().
property_definition(ModelModule, PropertyName, Values) ->
    property_definition(ModelModule, PropertyName, [], Values).

-spec property_definition(ModelModule :: atom(),
			  PropertyName :: atom(),
			  Args :: term(),
			  Values :: any()) -> PropertyBody :: string().
property_definition(ModelModule, PropertyName, Arguments, Values) ->
    FullModelModuleName = erlang:list_to_atom(erlang:atom_to_list(ModelModule) ++ ".erl"),
    [Exp] = see:scan_func_str_args(FullModelModuleName, PropertyName, Arguments),
    extract_property_definition(Exp, values_as_atoms(Values)).

%% @doc Reverses the truth value of a given property (for counterexample explanation).
%% @end
-type syntaxTree() :: any(). % as used in the OTP syntax tools (erl_syntax)
-spec falsify(Scenario :: syntaxTree()) -> syntaxTree().
falsify(Scenario) ->
    falsify_aux(Scenario).

%%% -------------------------------------------------------------- %%%

extract_module_description(ModelModule) ->
    XML = get_xml_version(ModelModule),
    module = XML#xmlElement.name,
    Descriptions = lists:flatten([ Element#xmlElement.content || Element <- XML#xmlElement.content,
								 Element#xmlElement.name == description ]),
    case lists:flatten([ Element#xmlElement.content || Element <- Descriptions,
						       Element#xmlElement.name == fullDescription ]) of
	[] -> % there was no general edoc section for module
	    [];
	[FullDescription] ->
	    FullDescription#xmlText.value
    end.


extract_function_description(ModelModule, Function) ->
    FunctionName = erlang:atom_to_list(Function),
    XML = get_xml_version(ModelModule),
    module = XML#xmlElement.name,
    Functions = lists:flatten([ Element#xmlElement.content || Element <- XML#xmlElement.content,
							      Element#xmlElement.name == functions ]),
    [FunctionDescription] = lists:filter(fun(Element) when is_record(Element, xmlElement) -> 
						 [] =/= [ Element#xmlElement.content || Attribute <- Element#xmlElement.attributes,
											Attribute#xmlAttribute.name == name,
											Attribute#xmlAttribute.value == FunctionName ]
					 end, Functions),
    function = FunctionDescription#xmlElement.name,
    Descriptions = lists:flatten([ Element#xmlElement.content || Element <- FunctionDescription#xmlElement.content,
								 Element#xmlElement.name == description ]),
    case lists:flatten([ Element#xmlElement.content || Element <- Descriptions,
						       Element#xmlElement.name == fullDescription ]) of
	[] -> % there was no edoc description for function
	    [];
	[FullDescription] ->
	    FullDescription#xmlText.value
    end.

get_xml_version(Module) ->
    FileName = erlang:atom_to_list(Module) ++ ".erl",
    {Module, XML} = edoc_extract:source(FileName, edoc_lib:get_doc_env(FileName), []),
    XML.

% ----- ----- ----- ----- ----- -----  ----- ----- ----- ----- ----- %

extract_property_definition(Exp, Values) when is_record(Exp, exp_iface) ->
    AppDefs = Exp#exp_iface.var_defs,
    PropDefs = [extract_property_definition_aux(AppDef, Values) || AppDef <- AppDefs],
    lists:flatten(PropDefs).

extract_property_definition_aux(App, Values) when is_record(App, apply) ->
    case lists:flatten([ Clauses || {'fun',_,{clauses,Clauses}} <- App#apply.arg_list]) of
	[] ->
	    FunDef = erl_syntax:application(erl_syntax:module_qualifier(erl_syntax:atom(App#apply.module),
									erl_syntax:operator(App#apply.call)),
					    App#apply.arg_list),
	    %% TODO: replace values in FunDef
	    ?DEBUG("Pretty-printing: ~p~n", [FunDef]),
	    erl_prettypr:format(FunDef);
	[FunDef] ->
	    [FunBody] = erl_syntax:clause_body(FunDef),
	    ?DEBUG("Pretty-printing: ~p~n", [{FunBody,replace_values(FunBody, Values)}]),
	    case replace_values(FunBody, Values) of
		{op,_,'not',Property} ->
		    erl_prettypr:format(Property) ++ ?ISFALSE;
		Property ->
		    erl_prettypr:format(Property)
	    end
    end.

replace_values(Exp, Values) ->
    {NewExp,_NotBindedValues,_BindedValues} = transverse_exp(Exp, Values, []),
    NewExp.

transverse_exp({var,N,Name}, [Value|MoreValues], UsedValues) when is_atom(Name) ->
    case lists:keyfind(Name, 1, UsedValues) of
	false ->
	    {{var,N,Value}, MoreValues, [{Name,Value}|UsedValues]};
	{Name,UsedValue} ->
	    {{var,N,UsedValue}, [Value|MoreValues], UsedValues}
    end;
transverse_exp(Exp, Values, UsedValues) when is_tuple(Exp) ->
    ExpList = erlang:tuple_to_list(Exp),
    {NewExpList, LessValues, MoreUsedValues} = transverse_exp(ExpList, Values, UsedValues),
    {list_to_tuple(NewExpList), LessValues, MoreUsedValues};
transverse_exp(Exp, Values, UsedValues) when is_list(Exp) ->
    lists:foldl(fun(Member, {RExp, Vs, UVs}) ->
			case transverse_exp(Member, Vs, UVs) of
			    {{var,N,Value}, MoreValues, MoreUsedValues} ->
				{RExp++[{var,N,Value}], MoreValues, MoreUsedValues};
			    {Other, MoreVs, MoreUsedVs} ->
				{RExp++[Other], MoreVs, MoreUsedVs}
			end
		end, {[], Values, UsedValues}, Exp);
transverse_exp(Exp, Values, UsedValues) ->
    {Exp, Values, UsedValues}.


falsify_aux({tree,form_list,Attributes,List}) ->
    {tree,form_list,Attributes,falsify(List)};
falsify_aux(Term = {tree,string,Attributes,Text}) ->
    [First | Words] = string:tokens(Text, " "),
    case string:equal(First, string:strip(?THEN)) of
	true ->
	    [Last, PrevToLast | RestOfWords] = lists:reverse(Words),
	    case string:equal(PrevToLast ++ " " ++ Last, string:strip(?ISFALSE)) of
		true ->
		    {tree,string,Attributes,lists:flatten([[First, 32], lists:reverse(RestOfWords), ?ISTRUE])};
		false ->
		    {tree,string,Attributes,lists:flatten([[First, 32], lists:reverse(RestOfWords), ?ISFALSE])}
	    end;
	false ->
	    Term
    end;
falsify_aux(TermList) when is_list(TermList) ->	
    [falsify(Term) || Term <- TermList];
falsify_aux(Other) ->	
    Other.

values_as_atoms(Value) when is_atom(Value) ->
    Value;
values_as_atoms(Value) when is_integer(Value) ->
    to_atom(Value);
values_as_atoms(Values) when is_tuple(Values) ->
    [to_atom(Value) || Value <- erlang:tuple_to_list(Values)];
values_as_atoms(Values) when is_list(Values) ->
    [to_atom(Value) || Value <- Values].

to_atom(Term) when is_integer(Term) ->
    erlang:list_to_atom(to_string(Term));
to_atom([]) ->
    '[]';
to_atom(Term) when is_list(Term) ->
    List = lists:foldl(fun(T, AccIn) -> AccIn ++ to_string(T) ++ ","  end, "[", Term),
    erlang:list_to_atom(string:substr(List, 1, length(List)-1) ++ "]");
to_atom(Term) when is_tuple(Term) ->
    List = lists:foldl(fun(T, AccIn) when is_list(T) ->
			       case io_lib:printable_unicode_list(T) of
				   true  -> AccIn ++ T ++ ",";
				   false -> AccIn ++ to_string(to_atom(T)) ++ "," 
			       end;
			  (T, AccIn) ->
			       AccIn ++ to_string(T) ++ ","
		       end, "{", erlang:tuple_to_list(Term)),
    erlang:list_to_atom(string:substr(List, 1, length(List)-1) ++ "}").

to_string(Term) when is_atom(Term) ->
    erlang:atom_to_list(Term);
to_string(Term) when is_integer(Term) ->
    erlang:integer_to_list(Term);
to_string(Term) when is_list(Term) ->
    Term;
to_string(Term) when is_tuple(Term) ->
    List = lists:foldl(fun(T, AccIn) -> AccIn ++ to_string(T) ++ ","  end, "{", erlang:tuple_to_list(Term)),
    string:substr(List, 1, length(List)-1) ++ "}".
    
