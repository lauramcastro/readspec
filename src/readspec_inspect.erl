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
			  Values :: [any()]) -> {PropertyBody :: string(), Aliases :: [atom()]}.
property_definition(ModelModule, PropertyName, Values) ->
    property_definition(ModelModule, PropertyName, [], Values).

-spec property_definition(ModelModule :: atom(),
			  PropertyName :: atom(),
			  Args :: term(),
			  Values :: [any()]) -> {PropertyBody :: string(), Aliases :: [atom()]}.
property_definition(ModelModule, PropertyName, Arguments, Values) ->
    FullModelModuleName = erlang:list_to_atom(erlang:atom_to_list(ModelModule) ++ ".erl"),
    [Exp] = see:scan_func_str_args(FullModelModuleName, PropertyName, Arguments),
    extract_property_definition(Exp, Values).

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
    lists:foldl(fun({Prop, Alias}, {ListofProp, ListofAlias}) ->
			{Prop ++ ListofProp, Alias ++ ListofAlias}
		end, {[], []}, PropDefs).

extract_property_definition_aux(App, Values) when is_record(App, apply) ->
    {FunBody, NValues} = extract_property_body(App#apply.arg_list),
    {Prop,As} = replace_values(FunBody, Values, NValues),
    ?DEBUG("Replacing ~p (as ~p) in ~p got ~p (and ~p)~n", [Values, NValues, FunBody, Prop, As]),
    case {Prop,As} of
	{{op,_,'not',Property},Aliases} ->
	    try erl_prettypr:format(Property) of X -> {X ++ ?ISFALSE, []}
	    catch _:_ -> {erl_prettypr:format(FunBody) ++ ?ISFALSE, Aliases} end;
	{Property, Aliases} ->
	    try erl_prettypr:format(Property) of X -> {X,[]}
	    catch _:_ -> {erl_prettypr:format(FunBody),Aliases} end
    end.

extract_property_body(Property) ->
    extract_property_body_aux(Property, 0).

extract_property_body_aux({call,_,_,CallBody}, N) ->
    extract_property_body_aux(CallBody, N);
extract_property_body_aux(BodyTree, N) when is_list(BodyTree) ->
    case lists:flatten([ Clauses || {'fun',_,{clauses,Clauses}} <- BodyTree]) of
	[]                        -> {[], N};
	[{clause,_,_,_,[Clause]}] -> extract_property_body_aux(Clause, N+1)
    end;
extract_property_body_aux(Body, N) ->
    {Body, N}.


replace_values(Exp, Values, NValues) ->
    {NewExp,_NotBindedValues,_N,BindedValues} = transverse_exp(Exp, Values, NValues, []),
    Aliases = [VarName || {VarName,_Value} <- BindedValues],
    {NewExp,Aliases}.

transverse_exp({var,N,Name}, Values, NValues, UsedValues) when is_atom(Name) ->
    case lists:keyfind(Name, 1, UsedValues) of
	false ->
	    {Value, MoreValues} = pick(Values, NValues),
	    NewValue = values_as_atoms(Value),
	    {{var,N,NewValue}, MoreValues, NValues-1, [{Name,NewValue}|UsedValues]};
	{Name,UsedValue} ->
	    {{var,N,UsedValue}, Values, NValues, UsedValues}
    end;
transverse_exp(Exp, Values, NValues, UsedValues) when is_tuple(Exp) ->
    ExpList = erlang:tuple_to_list(Exp),
    {NewExpList, LessValues, RestNValues, MoreUsedValues} = transverse_exp(ExpList, Values, NValues, UsedValues),
    {list_to_tuple(NewExpList), LessValues, RestNValues, MoreUsedValues};
transverse_exp(Exp, Values, NValues, UsedValues) when is_list(Exp) ->
    lists:foldl(fun(Member, {RExp, Vs, NVs, UVs}) ->
			case transverse_exp(Member, Vs, NVs, UVs) of
			    {{var,N,Value}, MoreValues, RestNValues, MoreUsedValues} ->
				{RExp++[{var,N,Value}], MoreValues, RestNValues, MoreUsedValues};
			    {Other, MoreVs, NVals, MoreUsedVs} ->
				{RExp++[Other], MoreVs, NVals, MoreUsedVs}
			end
		end, {[], Values, NValues, UsedValues}, Exp);
transverse_exp(Exp, Values, NValues, UsedValues) ->
    {Exp, Values, NValues, UsedValues}.


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

pick([Value], 1) ->
    {Value,[]};
pick(Value, 1) ->
    {Value,[]};
pick(Values,_) when is_tuple(Values) ->
    [H|T]=erlang:tuple_to_list(Values),
    {H,erlang:list_to_tuple(T)};
pick(Values,_) when is_list(Values) ->
    [H|T]=Values,
    {H,T}.

values_as_atoms([]) ->
    '[]';
values_as_atoms(Value) when is_atom(Value) ->
    Value;
values_as_atoms(Value) when is_integer(Value) ->
    erlang:list_to_atom(to_string(Value));
values_as_atoms(Value) when is_tuple(Value) ->
    list_to_tuple([values_as_atoms(V) || V <- erlang:tuple_to_list(Value)]);
values_as_atoms(Value) when is_list(Value) ->
    List = lists:foldl(fun(T, AccIn) -> AccIn ++ to_string(T) ++ ","  end, "[", Value),
    String = string:substr(List, 1, length(List)-1) ++ "]",
    try erlang:list_to_atom(String) of X -> X catch _:_ -> [values_as_atoms(V) || V <- Value] end.


to_string(Term) when is_atom(Term) ->
    erlang:atom_to_list(Term);
to_string(Term) when is_integer(Term) ->
    erlang:integer_to_list(Term);
to_string(Term) when is_list(Term) ->
    case io_lib:printable_unicode_list(Term) of
	true  -> Term;
 	false -> lists:flatten([ to_string(T) || T <- Term])
    end;
to_string(Term) when is_tuple(Term) ->
    List = lists:foldl(fun(T, AccIn) -> AccIn ++ to_string(T) ++ ","  end, "{", erlang:tuple_to_list(Term)),
    string:substr(List, 1, length(List)-1) ++ "}".
